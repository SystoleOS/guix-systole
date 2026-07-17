#!/usr/bin/env bash
# Load all installer/OS modules and verify the ISO-build entry points.
#
# Runs check-installer.scm in guix repl *script mode* so failures
# propagate.  guile + guile-newt/guile-parted/guile-webutils are needed
# because (gnu installer newt ...) imports (newt)/(parted)/(webutils
# multipart) at load time; guile itself must be in the shell profile so
# GUILE_LOAD_PATH gets populated.
#
# Set GUIX to override the guix invocation, e.g. for pinned CI runs:
#   GUIX="guix time-machine -C channels-lock.scm --" ./test-dry-run.sh
# ((os install) imports nonguix modules, so the guix that runs the repl
# must have the channels from channels-lock.scm available.)

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
GUIX=${GUIX:-guix}

# shellcheck disable=SC2086
exec $GUIX shell guile guile-newt guile-parted guile-webutils -- \
    $GUIX repl -L "$REPO_ROOT/system" -L "$REPO_ROOT/systole" -- \
    "$REPO_ROOT/tests/installer/check-installer.scm"
