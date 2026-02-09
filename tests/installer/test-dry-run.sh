#!/usr/bin/env bash
# Test installer in dry-run mode

set -e

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$REPO_ROOT"

echo "Testing installer dry-run..."

# Test that installer modules load
echo "  Testing installer module loading..."
if ! guix repl -L "$REPO_ROOT/system" <<'EOF' >/dev/null 2>&1
,m (installer installer)
,m (installer steps)
,m (installer final)
(display "Modules loaded successfully\n")
EOF
then
    echo "✗ Failed to load installer modules"
    exit 1
fi

echo "✓ Installer modules loaded successfully"

# TODO: Add actual dry-run test when we have the infrastructure
echo "  Note: Full dry-run test not yet implemented"
echo "  This would test: (run-installer #:dry-run? #t)"

exit 0
