#!/usr/bin/env bash
# Load every channel module and sanity-check every exported package.
#
# Runs check-packages.scm in guix repl *script mode*: unlike heredoc
# REPL sessions (which swallow errors and always exit 0), script mode
# propagates the script's exit status.
#
# Set GUIX to override the guix invocation, e.g. for pinned CI runs:
#   GUIX="guix time-machine -C channels-lock.scm --" ./test-modules-load.sh

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
GUIX=${GUIX:-guix}

# shellcheck disable=SC2086
exec $GUIX repl -L "$REPO_ROOT/systole" -- "$REPO_ROOT/tests/packages/check-packages.scm"
