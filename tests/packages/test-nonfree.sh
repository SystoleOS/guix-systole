#!/usr/bin/env bash
# Every non-free package -- and every package with a non-free package in
# its input closure -- must be #:substitutable? #f, so the build hub never
# builds, publishes or relays it.  See check-nonfree.scm.
#
# Set GUIX to override the guix invocation, e.g. for pinned CI runs:
#   GUIX="guix time-machine -C channels-lock.scm --" ./test-nonfree.sh

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
GUIX=${GUIX:-guix}

# shellcheck disable=SC2086
exec $GUIX repl -L "$REPO_ROOT/systole" -- "$REPO_ROOT/tests/packages/check-nonfree.scm"
