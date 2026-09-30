#!/usr/bin/env sh
git log --merges --oneline "$1..HEAD"
