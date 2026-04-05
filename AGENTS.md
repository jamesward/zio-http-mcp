# Agent Guidelines

## README Examples

Every code example in README.md must have a corresponding test in `McpToolSpec`. When adding or modifying a README example, add or update the matching test. The test suites in McpToolSpec are organized to mirror the README sections.

## Testing

- Run `./sbt "testOnly *McpToolSpec*"` for unit tests
- Run `./sbt "testOnly *ConformanceSpec*"` for MCP conformance tests (requires Docker)
- Conformance tests use testcontainers with `host.testcontainers.internal` for Docker networking (rootless Docker compatible)
