#!/usr/bin/env bash
# Main test runner script for Guix-Systole

set -e

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
cd "$REPO_ROOT"

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

    # Test that packages module loads
    run_test "Load systole packages module" \
        "guix repl -L . <<< ',m (systole packages)' >/dev/null 2>&1"

    # Run package test scripts if they exist
    if [ -d "tests/packages" ]; then
        for test_script in tests/packages/test-*.sh; do
            if [ -f "$test_script" ]; then
                run_test "$(basename $test_script)" "$test_script"
            fi
        done
    fi
}

test_installer() {
    log_info "=== Running Installer Tests ==="

    # Test that installer modules load
    run_test "Load installer modules" \
        "guix repl -L . <<< ',m (installer installer)' >/dev/null 2>&1"

    # Run installer test scripts if they exist
    if [ -d "tests/installer" ]; then
        for test_script in tests/installer/test-*.sh; do
            if [ -f "$test_script" ]; then
                run_test "$(basename $test_script)" "$test_script"
            fi
        done
    fi
}

test_lint() {
    log_info "=== Running Lint Tests ==="

    local packages=(
        "vtk-slicer"
        "itk-slicer"
        "ctk"
        "libarchive-slicer"
        "qrestapi"
        "vtkaddon"
        "teem-slicer"
    )

    for package in "${packages[@]}"; do
        run_test "Lint $package" \
            "guix lint -L . --exclude=archival $package"
    done
}

test_build() {
    log_info "=== Running Build Tests ==="
    log_warn "Build tests can take a long time. Use --skip-build to skip."

    if [ "$SKIP_BUILD" = "true" ]; then
        log_warn "Skipping build tests (--skip-build flag set)"
        return 0
    fi

    local packages=(
        "vtk-slicer"
        "itk-slicer"
        "ctk"
    )

    for package in "${packages[@]}"; do
        run_test "Build $package" \
            "timeout 7200 guix build -L . $package"
    done
}

# Print usage
usage() {
    cat <<EOF
Usage: $0 [OPTIONS] [CATEGORY]

Test runner for Guix-Systole

CATEGORIES:
    all         Run all tests (default)
    packages    Run package tests
    installer   Run installer tests (module loading only)
    lint        Run lint tests
    build       Run build tests (slow!)

OPTIONS:
    -h, --help      Show this help message
    --skip-build    Skip build tests (they take a long time)
    -v, --verbose   Verbose output

EXAMPLES:
    $0                  # Run all tests except builds
    $0 lint             # Run only lint tests
    $0 --skip-build     # Run all tests except builds
    $0 packages         # Run only package tests

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
            if [ "$SKIP_BUILD" != "true" ]; then
                log_warn "Build tests skipped by default. Use 'build' category to run them."
            fi
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
SKIP_BUILD=true
VERBOSE=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --skip-build)
            SKIP_BUILD=true
            shift
            ;;
        -v|--verbose)
            VERBOSE=true
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
