#!/usr/bin/env bash
# Run Systole VM-based system tests using Guix test framework
# Copyright © 2026 Rafael Palomar <rafael.palomar@ous-research.no>
# SPDX-License-Identifier: GPL-3.0-or-later

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
FAILURES=0
TOTAL_TESTS=0

info() {
    echo -e "${GREEN}[INFO]${NC} $*"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

success() {
    echo -e "${GREEN}✓${NC} $*"
}

fail() {
    echo -e "${RED}✗${NC} $*"
}

usage() {
    cat <<EOF
Usage: $0 [TEST_NAME]

Run Systole VM-based system tests.

ARGUMENTS:
    TEST_NAME    Optional. Run specific test by name. If omitted, runs all tests.

AVAILABLE TESTS:
    basic        - Test basic installer boots and has expected packages
    deploy-key   - Test installer with SSH deploy key
    no-ssh       - Test installer without deploy key has no SSH
    all          - Run all tests (default)

EXAMPLES:
    $0                  # Run all tests
    $0 basic            # Run only basic installer test
    $0 deploy-key       # Run only deploy key test

NOTES:
    - These tests build and boot full VMs - VERY resource intensive!
    - First run will build many dependencies (can take 30+ minutes per test)
    - Subsequent runs are faster if builds are cached
    - Requires KVM support for reasonable performance
    - Each test (after builds) takes 2-5 minutes to run
    - Tests run in isolated VMs and are safe to run
    - Use Ctrl+C to cancel if needed

EOF
    exit 0
}

run_test() {
    local test_name=$1
    local test_expression=$2
    local description=$3

    info "Running: ${BLUE}${test_name}${NC}"
    info "Description: ${description}"

    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    # Run the test
    if guix build -L "$REPO_DIR/systole" -L "$REPO_DIR/system" \
                  -e "$test_expression" 2>&1 | tee /tmp/test-${test_name}.log; then
        success "${test_name} passed"
        echo ""
        return 0
    else
        fail "${test_name} failed"
        error "See /tmp/test-${test_name}.log for details"
        echo ""
        FAILURES=$((FAILURES + 1))
        return 1
    fi
}

# Parse arguments
TEST_SELECTION="all"
if [[ $# -gt 0 ]]; then
    case $1 in
        -h|--help)
            usage
            ;;
        basic|deploy-key|no-ssh|all)
            TEST_SELECTION=$1
            ;;
        *)
            error "Unknown test: $1"
            usage
            ;;
    esac
fi

cd "$REPO_DIR"

info "Systole VM Test Runner"
info "Repository: $REPO_DIR"
echo ""

warn "VM tests are VERY resource-intensive!"
warn "First run: May take 30-60+ minutes to build VMs"
warn "Subsequent runs: 2-5 minutes per test (if cached)"
warn "Press Ctrl+C to cancel at any time"
echo ""
sleep 2

# Run selected tests
if [[ "$TEST_SELECTION" == "all" ]] || [[ "$TEST_SELECTION" == "basic" ]]; then
    run_test "systole-installer-basic" \
             "((@ (srfi srfi-1) first) ((@ (gnu tests) system-test-value) (@ (systole tests installer) %test-systole-installer-basic)))" \
             "Test basic Systole installer boots and has expected characteristics"
fi

if [[ "$TEST_SELECTION" == "all" ]] || [[ "$TEST_SELECTION" == "deploy-key" ]]; then
    run_test "systole-installer-deploy-key" \
             "((@ (srfi srfi-1) first) ((@ (gnu tests) system-test-value) (@ (systole tests installer) %test-systole-installer-deploy-key)))" \
             "Test Systole installer with SSH deploy key boots and SSH is configured"
fi

if [[ "$TEST_SELECTION" == "all" ]] || [[ "$TEST_SELECTION" == "no-ssh" ]]; then
    run_test "systole-installer-no-ssh-without-key" \
             "((@ (srfi srfi-1) first) ((@ (gnu tests) system-test-value) (@ (systole tests installer) %test-systole-installer-no-ssh-without-key)))" \
             "Test Systole installer without deploy key has no SSH daemon"
fi

# Print summary
echo ""
echo "============================================"
info "Test Summary"
echo "============================================"
echo "Tests run:    $TOTAL_TESTS"
echo -e "Tests passed: ${GREEN}$((TOTAL_TESTS - FAILURES))${NC}"
echo -e "Tests failed: ${RED}${FAILURES}${NC}"
echo ""

if [[ $FAILURES -eq 0 ]]; then
    success "All VM tests passed! 🎉"
    exit 0
else
    error "$FAILURES test(s) failed"
    info "Check logs in /tmp/test-*.log for details"
    exit 1
fi
