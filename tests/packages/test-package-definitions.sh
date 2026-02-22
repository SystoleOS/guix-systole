#!/usr/bin/env bash
# Test that package definitions actually exist and are accessible

set -e

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$REPO_ROOT"

echo "Testing that package definitions exist and are accessible..."

FAILED=0

# Test function to verify a package is defined
test_package() {
    local module="$1"
    local package="$2"

    echo -n "  Testing $package in $module... "

    # Test that we can access the package
    if guix repl -L . <<EOF >/dev/null 2>&1
(use-modules $module)
(unless (defined? '$package)
  (error "Package not defined: $package"))
$package
EOF
    then
        echo "✓"
        return 0
    else
        echo "✗ FAILED"
        FAILED=$((FAILED + 1))
        return 1
    fi
}

# Test VTK packages
test_package "(systole packages vtk)" "vtk-slicer"
test_package "(systole packages vtk)" "vtkaddon"

# Test ITK packages
test_package "(systole packages itk)" "itk-slicer"

# Test Slicer packages
test_package "(systole packages slicer)" "slicer-5.8"
test_package "(systole packages slicer)" "slicer-volumes-5.8"
test_package "(systole packages slicer)" "slicer-terminologies-5.8"
test_package "(systole packages slicer)" "slicer-subjecthierarchy-5.8"
test_package "(systole packages slicer)" "slicer-colors-5.8"
test_package "(systole packages slicer)" "slicer-units-5.8"
test_package "(systole packages slicer)" "slicer-tables-5.8"
test_package "(systole packages slicer)" "slicer-cameras-5.8"
test_package "(systole packages slicer)" "slicer-data-5.8"
test_package "(systole packages slicer)" "slicer-markups-5.8"
test_package "(systole packages slicer)" "slicer-annotations-5.8"
test_package "(systole packages slicer)" "slicer-models-5.8"
test_package "(systole packages slicer)" "slicer-sequences-5.8"
test_package "(systole packages slicer)" "slicer-viewcontrollers-5.8"
test_package "(systole packages slicer)" "slicer-reformat-5.8"
test_package "(systole packages slicer)" "slicer-plots-5.8"
test_package "(systole packages slicer)" "slicer-sceneviews-5.8"
test_package "(systole packages slicer)" "slicer-segmentations-5.8"
test_package "(systole packages slicer)" "slicer-volumerendering-5.8"
test_package "(systole packages slicer)" "slicer-transforms-5.8"
test_package "(systole packages slicer)" "slicer-texts-5.8"
test_package "(systole packages slicer)" "slicer-slicerwelcome-5.8"
test_package "(systole packages slicer)" "slicer-all-5.8"

# Test CTK
test_package "(systole packages ctk)" "ctk"
test_package "(systole packages ctk)" "ctkapplauncher"

# Test Teem
test_package "(systole packages teem)" "teem-slicer"

# Test Math packages
test_package "(systole packages maths)" "netcdf-slicer"

# Test libarchive
test_package "(systole packages libarchive)" "libarchive-slicer"

# Test qrestapi
test_package "(systole packages qrestapi)" "qrestapi"

# Test openigtlink
test_package "(systole packages openigtlink)" "openigtlink"
test_package "(systole packages openigtlink)" "slicer-openigtlink"

if [ $FAILED -eq 0 ]; then
    echo "All package definitions verified successfully!"
    exit 0
else
    echo "$FAILED package definition(s) failed verification"
    exit 1
fi
