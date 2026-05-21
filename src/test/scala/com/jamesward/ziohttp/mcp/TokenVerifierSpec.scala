package com.jamesward.ziohttp.mcp

import com.jamesward.ziohttp.mcp.auth.*
import com.nimbusds.jose.{JOSEObjectType, JWSAlgorithm, JWSHeader}
import com.nimbusds.jose.crypto.RSASSASigner
import com.nimbusds.jose.jwk.RSAKey
import com.nimbusds.jose.jwk.gen.RSAKeyGenerator
import com.nimbusds.jwt.{JWTClaimsSet, SignedJWT}
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

import java.security.PublicKey
import java.security.interfaces.RSAPublicKey
import java.util.{Date, UUID}

object TokenVerifierSpec extends ZIOSpecDefault:

  // --- Test fixtures ---

  /** RSA keypair with a stable kid for signing test JWTs. */
  private val testKid = "test-kid-1"

  /** RSA JWK (with private key) generated once for all tests. */
  private val testRsaJwk: RSAKey =
    new RSAKeyGenerator(2048).keyID(testKid).generate()

  private val testPublicKey: RSAPublicKey = testRsaJwk.toRSAPublicKey
  private val testSigner: RSASSASigner    = new RSASSASigner(testRsaJwk)

  /** Build a JWK JSON object for the test public key (for the AS-mock JWKS endpoint). */
  private def jwkPublicJson(jwk: RSAKey): Json.Obj =
    jwk.toPublicJWK.toJSONString.fromJson[Json.Obj].toOption.get

  /** Sign a JWT with the test private key. Caller controls iss/aud/scope/exp. */
  private def signTestJwt(
    iss: String,
    aud: String,
    scope: String,
    sub: String = "test-sub",
    expDelta: Duration = 1.hour,
    notBeforeDelta: Duration = (-1).minute,
    kid: String = testKid,
    signer: RSASSASigner = testSigner,
  ): String =
    val now = java.time.Instant.now()
    val claims = new JWTClaimsSet.Builder()
      .issuer(iss)
      .audience(aud)
      .subject(sub)
      .expirationTime(Date.from(now.plusSeconds(expDelta.toSeconds)))
      .notBeforeTime(Date.from(now.plusSeconds(notBeforeDelta.toSeconds)))
      .issueTime(Date.from(now))
      .jwtID(UUID.randomUUID().toString)
      .claim("scope", scope)
      .claim("client_id", sub)
      .build()
    val header = new JWSHeader.Builder(JWSAlgorithm.RS256)
      .keyID(kid)
      .`type`(JOSEObjectType.JWT)
      .build()
    val signed = new SignedJWT(header, claims)
    signed.sign(signer)
    signed.serialize()

  /** Spin up a small in-process server that serves AS metadata + JWKS. */
  private def asServerRoutes(callCounter: Ref[Int]): Routes[Any, Response] =
    val jwks = Json.Obj(Chunk(
      "keys" -> Json.Arr(Chunk(jwkPublicJson(testRsaJwk)))
    )).toJson
    Routes(
      Method.GET / ".well-known" / "oauth-authorization-server" -> handler { (req: Request) =>
        // Build issuer URL based on the actual host header so the verifier's expectedIssuer matches
        val host = req.rawHeader("host").map(h => s"http://$h").getOrElse("http://localhost")
        val metadata = Json.Obj(Chunk(
          "issuer" -> Json.Str(host),
          "jwks_uri" -> Json.Str(s"$host/jwks"),
        )).toJson
        ZIO.succeed(Response.json(metadata))
      },
      Method.GET / "jwks" -> handler { (_: Request) =>
        callCounter.update(_ + 1).as(Response.json(jwks))
      },
    )

  /** Helper that boots the AS-mock server and wires a JwksTokenVerifier against it. */
  private def withVerifier[A](body: (TokenVerifier[Any], String, Ref[Int]) => ZIO[Client, AuthError, A]): ZIO[Server & Client & Scope, Throwable, A] =
    for
      counter <- Ref.make(0)
      port    <- Server.install(asServerRoutes(counter))
      issuer   = s"http://localhost:$port"
      verifier <- TokenVerifier.discoverJwks(
                    issuer = issuer,
                    refreshInterval = 1.hour,
                  )
      result  <- body(verifier, issuer, counter).mapError {
                   case e: AuthError => RuntimeException(e.toString): Throwable
                 }
    yield result

  override def spec =
    suite("TokenVerifierSpec")(

      suite("end-to-end JWT verification")(

        test("valid token → Principal with sub, scopes, audience, issuer"):
          withVerifier { (verifier, issuer, _) =>
            val token = signTestJwt(iss = issuer, aud = "https://mcp.example.com/mcp", scope = "mcp:tools admin")
            verifier.verify(token).map { p =>
              assertTrue(
                p.subject.contains("test-sub"),
                p.scopes == Set(OauthScope("mcp:tools"), OauthScope("admin")),
                p.audience == Set("https://mcp.example.com/mcp"),
                p.issuer.contains(issuer),
                p.raw == token,
              )
            }
          }
        ,

        test("token signed by an unknown key → Invalid"):
          withVerifier { (verifier, issuer, _) =>
            // Generate a different keypair, sign a token with our claimed kid, but the verifier
            // will look up testKid from the JWKS and find our published key — which won't match
            // this token's actual signature.
            val otherJwk = new RSAKeyGenerator(2048).keyID(testKid).generate()
            val otherSigner = new RSASSASigner(otherJwk)
            val tampered = signTestJwt(
              iss = issuer,
              aud = "https://mcp.example.com/mcp",
              scope = "mcp:tools",
              signer = otherSigner,
            )
            verifier.verify(tampered).either.map { result =>
              assertTrue(result.left.toOption.exists {
                case _: AuthError.Invalid => true
                case _ => false
              })
            }
          }
        ,

        test("expired token → Expired"):
          withVerifier { (verifier, issuer, _) =>
            val token = signTestJwt(
              iss = issuer,
              aud = "https://mcp.example.com/mcp",
              scope = "mcp:tools",
              expDelta = (-1).hour,  // exp in the past
              notBeforeDelta = (-2).hour,
            )
            verifier.verify(token).either.map { result =>
              assertTrue(result == Left(AuthError.Expired))
            }
          }
        ,

        test("any audience is accepted by the verifier (audience check moved to middleware)"):
          withVerifier { (verifier, issuer, _) =>
            val token = signTestJwt(iss = issuer, aud = "https://other.example.com/mcp", scope = "mcp:tools")
            verifier.verify(token).map { p =>
              assertTrue(p.audience == Set("https://other.example.com/mcp"))
            }
          }
        ,

        test("wrong issuer → IssuerMismatch"):
          withVerifier { (verifier, issuer, _) =>
            val token = signTestJwt(iss = "https://wrong.example.com", aud = "https://mcp.example.com/mcp", scope = "mcp:tools")
            verifier.verify(token).either.map { result =>
              assertTrue(result.left.toOption.exists {
                case AuthError.IssuerMismatch(_, _) => true
                case _ => false
              })
            }
          }
        ,

        test("unknown kid → Invalid"):
          withVerifier { (verifier, issuer, _) =>
            val token = signTestJwt(
              iss = issuer,
              aud = "https://mcp.example.com/mcp",
              scope = "mcp:tools",
              kid = "unknown-kid",
            )
            verifier.verify(token).either.map { result =>
              assertTrue(result.left.toOption.exists {
                case _: AuthError.Invalid => true
                case _ => false
              })
            }
          }
        ,

        test("JWKS is cached: verification doesn't trigger additional HTTP calls"):
          withVerifier { (verifier, issuer, counter) =>
            val token = signTestJwt(iss = issuer, aud = "https://mcp.example.com/mcp", scope = "mcp:tools")
            for
              before <- counter.get
              _      <- verifier.verify(token)
              _      <- verifier.verify(token)
              after  <- counter.get
            yield
              // The new validator may make a fixed number of bookkeeping fetches at startup
              // (initial fetch + first scheduled refresh), but verifications themselves
              // never trigger additional fetches because the JWKS lives in an
              // AtomicReference. Assert the count is unchanged across two verifications.
              assertTrue(after == before)
          }
        ,

        test("malformed JWT (not three parts) → Invalid"):
          withVerifier { (verifier, _, _) =>
            verifier.verify("not.a.jwt.token").either.map { result =>
              assertTrue(result.left.toOption.exists {
                case _: AuthError.Invalid => true
                case _ => false
              })
            }
          }
        ,
      ),
    ).provide(Server.defaultWithPort(0), Client.default, Scope.default) @@ sequential @@ withLiveClock @@ timeout(30.seconds)
