#!/usr/bin/env bash
# Test that all package modules load correctly

set -e

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$REPO_ROOT"

echo "Testing that all package modules load..."

MODULES=(
    "(systole packages)"
    "(systole packages slicer)"
    "(systole packages vtk)"
    "(systole packages itk)"
    "(systole packages ctk)"
    "(systole packages teem)"
    "(systole packages maths)"
    "(systole packages libarchive)"
    "(systole packages qrestapi)"
    "(systole packages openigtlink)"
)

FAILED=0

for module in "${MODULES[@]}"; do
    echo -n "  Testing $module... "
    if guix repl -L . <<EOF >/dev/null 2>&1
,m $module
(display "OK")
EOF
    then
        echo "✓"
    else
        echo "✗ FAILED"
        FAILED=$((FAILED + 1))
    fi
done

if [ $FAILED -eq 0 ]; then
    echo "All modules loaded successfully!"
    exit 0
else
    echo "$FAILED module(s) failed to load"
    exit 1
fi
