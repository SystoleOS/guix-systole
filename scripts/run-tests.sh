#!/usr/bin/env bash
# Main test runner script for Guix-Systole
#
# Deliberately NOT `set -e`: run_test failures must feed the PASS/FAIL
# counters and the final summary instead of aborting the script.

set -uo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Get repository root
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT" || exit 1

# Override the guix invocation for pinned runs, e.g. in CI:
#   GUIX="guix time-machine -C channels-lock.scm --" ./scripts/run-tests.sh
export GUIX=${GUIX:-guix}

# Logging functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Test runner function
run_test() {
    local test_name="$1"
    local test_command="$2"

    TESTS_RUN=$((TESTS_RUN + 1))
    echo ""
    log_info "Running: $test_name"

    if eval "$test_command"; then
        TESTS_PASSED=$((TESTS_PASSED + 1))
        echo -e "${GREEN}✓${NC} $test_name passed"
        return 0
    else
        TESTS_FAILED=$((TESTS_FAILED + 1))
        echo -e "${RED}✗${NC} $test_name failed"
        return 1
    fi
}

# Test categories
test_packages() {
    log_info "=== Running Package Tests ==="

    for test_script in tests/packages/test-*.sh; do
        if [ -f "$test_script" ]; then
            run_test "$(basename "$test_script")" "$test_script" || true
        fi
    done
}

test_installer() {
    log_info "=== Running Installer Tests ==="

    for test_script in tests/installer/test-*.sh; do
        if [ -f "$test_script" ]; then
            run_test "$(basename "$test_script")" "$test_script" || true
        fi
    done
}

# Single source of truth for the lint list -- CI calls this script, so
# keep additions here rather than in the workflow files.
LINT_PACKAGES=(
    "vtk-slicer"
    "vtkaddon"
    "itk-slicer"
    "slicer-5.8"
    "slicer-volumes-5.8"
    "ctk"
    "ctkapplauncher"
    "teem-slicer"
    "netcdf-slicer"
    "libarchive-slicer"
    "qrestapi"
    "openigtlink"
    "slicer-openigtlink"
)

LINT_ALLOWLIST="$REPO_ROOT/tests/lint-allowlist.regex"

# `guix lint` exits 0 even when it emits warnings, so gate on its
# output: anything not matching the allowlist of documented technical
# debt fails the check.
lint_package() {
    local pkg="$1"
    local out
    # shellcheck disable=SC2086
    out=$($GUIX lint -L "$REPO_ROOT/systole" --exclude=archival "$pkg" 2>&1 \
              | grep -v '^;;;' || true)
    if [ -s "$LINT_ALLOWLIST" ]; then
        out=$(printf '%s\n' "$out" | grep -Ev -f "$LINT_ALLOWLIST" || true)
    fi
    out=$(printf '%s\n' "$out" | sed '/^[[:space:]]*$/d')
    if [ -n "$out" ]; then
        printf '%s\n' "$out"
        return 1
    fi
    return 0
}

test_lint() {
    log_info "=== Running Lint Tests ==="

    for package in "${LINT_PACKAGES[@]}"; do
        run_test "Lint $package" "lint_package $package" || true
    done
}

test_build() {
    log_info "=== Running Build Tests ==="
    log_warn "Build tests can take a long time."

    local packages=(
        "vtk-slicer"
        "itk-slicer"
        "ctk"
    )

    for package in "${packages[@]}"; do
        run_test "Build $package" \
            "timeout 7200 \$GUIX build -L \"$REPO_ROOT/systole\" $package" || true
    done
}

# Print usage
usage() {
    cat <<EOF
Usage: $0 [OPTIONS] [CATEGORY]

Test runner for Guix-Systole

CATEGORIES:
    all         Run packages + installer + lint tests (default; no builds)
    packages    Run package tests
    installer   Run installer tests (module loading only)
    lint        Run lint tests
    build       Run build tests (slow!)

OPTIONS:
    -h, --help      Show this help message
    -v, --verbose   Verbose output

EXAMPLES:
    $0                  # Run all tests except builds
    $0 lint             # Run only lint tests
    $0 packages         # Run only package tests
    $0 build            # Actually compile vtk-slicer/itk-slicer/ctk

VM TESTS:
    For comprehensive VM-based system tests (boots full VMs):
        ./scripts/run-vm-tests.sh [basic|deploy-key|no-ssh|all]

    VM tests verify:
    - Installer boots successfully in QEMU
    - Deploy key SSH configuration works correctly
    - Services are running as expected

    Note: VM tests are slower (2-5 minutes each) and require more resources.

EOF
}

# Main execution
main() {
    local category="${1:-all}"

    log_info "Guix-Systole Test Runner"
    log_info "Repository: $REPO_ROOT"
    echo ""

    case "$category" in
        packages)
            test_packages
            ;;
        installer)
            test_installer
            ;;
        lint)
            test_lint
            ;;
        build)
            test_build
            ;;
        all)
            test_packages
            test_installer
            test_lint
            log_warn "Build tests not included in 'all'. Run '$0 build' explicitly."
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            log_error "Unknown test category: $category"
            usage
            exit 1
            ;;
    esac

    # Print summary
    echo ""
    echo "============================================"
    log_info "Test Summary"
    echo "============================================"
    echo "Tests run:    $TESTS_RUN"
    echo -e "Tests passed: ${GREEN}$TESTS_PASSED${NC}"
    echo -e "Tests failed: ${RED}$TESTS_FAILED${NC}"
    echo ""

    if [ $TESTS_FAILED -eq 0 ]; then
        log_info "All tests passed! 🎉"
        exit 0
    else
        log_error "Some tests failed"
        exit 1
    fi
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -v|--verbose)
            set -x
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            break
            ;;
    esac
done

main "$@"
