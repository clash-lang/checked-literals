#!/usr/bin/env bash
set -euo pipefail

fourmolu --quiet --mode inplace --check-idempotence $(git ls-files '*.hs')
cabal-gild --mode format --io=checked-literals.cabal
