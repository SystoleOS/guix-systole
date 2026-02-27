---
name: guix-systole-dev
description: >
  Develop and extend the guix-systole Guix channel. Use for questions about
  adding Slicer module packages, writing patches, using make-slicer-loadable-module
  and make-slicer-scripted-module, wiring inter-module dependencies, running
  guix build/lint, and understanding the channel architecture.
version: "5.8.1"
requires: []
---

# guix-systole-dev Skill

This skill provides read-only access to the guix-systole channel source files
(`.scm` package definitions) at a content-addressed Guix store path, plus a
two-layer guide covering GNU Guix channel concepts and the guix-systole Slicer
patch workflow.

## Data Sources

Run `guix-systole-dev-skill status` to print the active store path.

| Variable | Content |
|---|---|
| `$GUIX_SYSTOLE_SOURCE` | guix-systole `.scm` files and skill docs (read-only store path) |
| `$SLICER_SYSTOLE_DIR` | Mutable Slicer-Systole git checkout (set in your CLAUDE.md) |

---

## Layer 1 — GNU Guix Channel Fundamentals

### Channel Structure

The channel directory is `systole/`.  All package definitions live under
`systole/systole/packages/`.  The channel is loaded with `-L $(pwd)/systole`.

```
systole/
  guix.scm                      # channel descriptor
  systole/packages/
    slicer.scm                  # 3D Slicer packages (largest file)
    vtk.scm                     # VTK packages
    itk.scm                     # ITK packages
    ctk.scm                     # CTK packages
    pythonqt.scm                # PythonQt packages
    claude-skills.scm           # Claude skill packages (this skill!)
    patches/slicer/             # Guix patches for Slicer (0001-0048 + per-module subdirs)
    skills/                     # SKILL.md files for each skill package
```

### Package Records

Key fields in `(package ...)`:

| Field | Purpose |
|---|---|
| `name` | String package name (e.g., `"slicer-markups-5.8"`) |
| `version` | Version string |
| `source` | `(origin ...)` or `#f` for meta-packages |
| `build-system` | e.g., `cmake-build-system`, `trivial-build-system` |
| `arguments` | `(list #:configure-flags ...)` — build arguments |
| `inputs` | Runtime dependencies (not propagated to dependents) |
| `native-inputs` | Build-time only (not in closure) |
| `propagated-inputs` | Appear in dependents' environments |

**Inheritance**: `(package (inherit base-pkg) (name "new-name") ...)` — override only
the fields you need.  Use `(modify-inputs (package-inputs base) (replace "foo" bar))`
to swap a single dependency.

### Gexps (G-expressions)

Gexps are Guix's build-side expression syntax:

- `#~(...)` — a gexp (evaluated in the build environment)
- `#$pkg` — splice the store path of `pkg` into a gexp
- `#$(file-append pkg "/subdir")` — store path with subpath appended
- `#$@list` — splice a list of values
- `(computed-file "name" gexp)` — build a file from a gexp
- `(local-file "path")` — include a local file in the store
- `(local-file "path" #:recursive? #t)` — include a local directory
- `(with-imported-modules '((guix build utils)) #~(...))` — import modules into builder

Path from package to subpath:
```scheme
(file-append some-package "/bin/some-tool")
;; expands to e.g. "/gnu/store/abc...-some-package-1.0/bin/some-tool"
```

### Build Systems

**cmake-build-system**: standard CMake configure/build/install.
Configure flags go in `#:configure-flags`:
```scheme
(arguments
 (list #:configure-flags
       #~(list (string-append "-DFOO_DIR=" #$(file-append foo-pkg "/include"))
               "-DBAR=ON")))
```

**trivial-build-system**: runs arbitrary Scheme code in `#:builder`.

### Patch Infrastructure

Patches are stored in `systole/systole/packages/patches/`.  They are referenced
in `(origin ...)` with `(patches (search-patches "slicer/0001-foo.patch"))`.

Guix searches for patches relative to `%patch-path`, which includes the channel's
`patches/` directory (configured in `guix.scm`).

To **replace** all patches in an inherited origin (e.g., for standalone module builds):
```scheme
(origin (inherit (package-source slicer-python-5.8))
        (patches (search-patches "slicer/markups/0001-foo.patch"
                                 "slicer/markups/0002-bar.patch")))
```

### Factory Functions

**`make-slicer-loadable-module`** (in `slicer.scm`): builds a loadable module as a
standalone CMake project.  Key keyword arguments:

| Argument | Purpose |
|---|---|
| `#:module-name` | String, e.g., `"Markups"` |
| `#:module-subdir` | Subdir under `Modules/Loadable/`, e.g., `"Markups"` |
| `#:patches` | List of patch paths (replaces slicer-python-5.8's patches) |
| `#:extra-inputs` | Additional package inputs |
| `#:extra-configure-flags` | Additional CMake `-D` flags (as a gexp list) |
| `#:slicer` | Base Slicer package (default: `slicer-python-5.8`) |
| `#:validate-runpath?` | Whether to validate ELF RPATHs (default: `#f`) |

**`make-slicer-scripted-module`** (in `slicer.scm`): builds a scripted (Python) module.
Always uses `slicer-python-5.8`.  Key arguments: `#:module-name`, `#:module-subdir`,
`#:patches`.

### Key Gotchas

- **Always use absolute `-L` path**: `guix build -L $(pwd)/systole` — `local-file`
  resolves relative to the `.scm` file location, not the working directory.
- **`(inherit ...)` replaces only listed fields** — all others come from the parent.
- **Patches in `slicer-5.8` are NOT applied to standalone module builds** — modules
  replace the patches list entirely (they don't append).

### Testing Commands

```bash
# Syntax check (fast — just loads the module)
guix repl -L . <<< ',m (systole packages slicer)'
guix repl -L . <<< ',m (systole packages claude-skills)'

# Build a specific package
guix build -L $(pwd)/systole slicer-cameras-5.8
guix build -L $(pwd)/systole slicer-python-all-5.8

# Lint (skip archival check which needs network)
guix lint -L systole --exclude=archival slicer-cameras-5.8

# Enter a test shell
guix shell -L systole slicer-python-all-5.8

# Run all fast tests
./scripts/run-tests.sh
./scripts/run-tests.sh packages   # module-loading tests only
```

---

## Layer 2 — guix-systole Slicer Patch Workflow

### Repository Layout

```
patches/slicer/
  0001-...patch  through  0048-...patch  # Applied to slicer-5.8 (full build)
  markups/0001-...  0002-...             # Applied to slicer-markups-5.8 only
  subjecthierarchy/0001-... 0008-...
  segmentations/0001-...
  colors/  units/  tables/  ...          # One subdir per module that needs patches
```

### Slicer-Systole Git Repository

The patches are git commits in `$SLICER_SYSTOLE_DIR` (default: `~/src/Slicer/Slicer-Systole`).

**Base commit** (all branches must start here):
```
11eaf62e5a70b828021ff8beebbdd14d10d4f51c
```

**Branch naming**:

| Type | Branch pattern |
|---|---|
| Main Slicer patches | `guix-systole-slicer-5.8` |
| Loadable module | `guix-systole-<name>-module-5.8.1` |
| Scripted module | `guix-systole-<name>-scripted-module-5.8.1` |

### Creating / Updating a Module Branch

```bash
cd $SLICER_SYSTOLE_DIR

# Create a new branch from the base commit
git checkout 11eaf62e -b guix-systole-<name>-module-5.8.1

# Make your changes to Modules/Loadable/<Name>/CMakeLists.txt etc.
# Then commit with the guix-systole format:
git commit -m "[ENH][packages/slicer] Add standalone build support for <Name>"

# Export patches (ALWAYS use absolute --output-directory)
git format-patch -N 11eaf62e..HEAD \
  --output-directory /home/rafael/src/guix-systole/systole/systole/packages/patches/slicer/<name>/
```

**Commit format**: `[Prefix][Component] Title`
- Prefixes: `BUG:` `COMP:` `DOC:` `ENH:` `PERF:` `STYLE:` `WIP:`
- Components: `packages/slicer`, `packages/vtk`, etc.
- **NEVER add `Co-Authored-By: Claude` to commits.**

### CMakeLists Preamble for Loadable Modules

Add this to the top of `Modules/Loadable/<Name>/CMakeLists.txt`:

```cmake
cmake_minimum_required(VERSION 3.16.3...3.19.7 FATAL_ERROR)
project(<Name>)
set(CMAKE_CXX_STANDARD "17")
find_package(Slicer REQUIRED)
include(${Slicer_USE_FILE})
if(NOT Slicer_INSTALL_QTLOADABLEMODULES_SHARE_DIR)
  set(Slicer_INSTALL_QTLOADABLEMODULES_SHARE_DIR
      "./share/Slicer-${Slicer_VERSION}/qt-loadable-modules")
endif()
if(NOT Slicer_INSTALL_QTLOADABLEMODULES_LIB_DIR)
  set(Slicer_INSTALL_QTLOADABLEMODULES_LIB_DIR
      "./lib/Slicer-${Slicer_VERSION}/qt-loadable-modules")
endif()
```

### CMakeLists Preamble for Scripted Modules

```cmake
cmake_minimum_required(VERSION 3.16.3...3.19.7 FATAL_ERROR)
project(<Name>)
set(CMAKE_CXX_STANDARD "17")
find_package(Slicer REQUIRED)
include(${Slicer_USE_FILE})
if(NOT Slicer_QTSCRIPTEDMODULES_LIB_DIR)
  set(Slicer_QTSCRIPTEDMODULES_LIB_DIR
      "lib/Slicer-${Slicer_VERSION}/qt-scripted-modules")
endif()
if(NOT Slicer_INSTALL_QTSCRIPTEDMODULES_LIB_DIR)
  set(Slicer_INSTALL_QTSCRIPTEDMODULES_LIB_DIR
      "lib/Slicer-${Slicer_VERSION}/qt-scripted-modules")
endif()
```

### Basic make-slicer-loadable-module Usage

```scheme
;; Minimal (no inter-module deps)
(define-public slicer-colors-5.8
  (make-slicer-loadable-module
   #:module-name "Colors"
   #:module-subdir "Colors"
   #:patches (search-patches "slicer/colors/0001-add-standalone-build.patch")))

;; With inter-module dependency
(define-public slicer-data-5.8
  (make-slicer-loadable-module
   #:module-name "Data"
   #:module-subdir "Data"
   #:patches (search-patches "slicer/data/0001-add-standalone-build.patch")
   #:extra-inputs
   (list slicer-cameras-5.8
         slicer-subjecthierarchy-5.8)
   #:extra-configure-flags
   #~(list (string-append "-DqSlicerSubjectHierarchyModuleWidgets_INCLUDE_DIRS="
                          #$(file-append slicer-subjecthierarchy-5.8
                             "/include/Slicer-5.8/qt-loadable-modules/SubjectHierarchy"))
           (string-append "-DEXTRA_MODULE_LIB_DIRS="
                          #$(file-append slicer-cameras-5.8
                             "/lib/Slicer-5.8/qt-loadable-modules")
                          ";"
                          #$(file-append slicer-subjecthierarchy-5.8
                             "/lib/Slicer-5.8/qt-loadable-modules")))))
```

### make-slicer-scripted-module Usage

```scheme
(define-public slicer-sampledata-5.8
  (make-slicer-scripted-module
   #:module-name "SampleData"
   #:module-subdir "SampleData"
   #:patches (search-patches "slicer/sampledata/0001-add-standalone-build.patch")))
```

### Inter-Module Dependency Wiring

**Include path pattern**: `<pkg>/include/Slicer-5.8/qt-loadable-modules/<LibName>`
**Lib path pattern**: `<pkg>/lib/Slicer-5.8/qt-loadable-modules`

Multiple lib dirs are joined with `;` (CMake list separator):
```scheme
(string-append #$(file-append pkg-a "/lib/Slicer-5.8/qt-loadable-modules")
               ";"
               #$(file-append pkg-b "/lib/Slicer-5.8/qt-loadable-modules"))
```

In `CMakeLists.txt`, add `LINK_DIRECTORIES ${EXTRA_MODULE_LIB_DIRS}` to ALL macro
calls: `slicerMacroBuildLoadableModule`, `SlicerMacroBuildModuleMRML`,
`SlicerMacroBuildModuleLogic`, `SlicerMacroBuildModuleWidgets`.

### Adding a New Module — Checklist

1. **Check the source** in `$GUIX_SYSTOLE_SOURCE` to see if it's already packaged.
2. **Check `$SLICER_SOURCE/Modules/Loadable/<Name>/CMakeLists.txt`** for dependencies.
3. **Create the branch** in `$SLICER_SYSTOLE_DIR` from the base commit.
4. **Add the CMakeLists preamble** (loadable or scripted variant).
5. **Add `LINK_DIRECTORIES ${EXTRA_MODULE_LIB_DIRS}`** to all macro calls.
6. **Commit and export patches** with `git format-patch`.
7. **Add the package definition** in `slicer.scm` using the factory function.
8. **Add to `%slicer-5.8-loadable-modules`** (or scripted list).
9. **Syntax check**: `guix repl -L . <<< ',m (systole packages slicer)'`
10. **Build**: `guix build -L $(pwd)/systole slicer-<name>-5.8`

### ABI Rule

**Always build against `slicer-python-5.8`** (never `slicer-5.8`).
`make-slicer-loadable-module` defaults to `#:slicer slicer-python-5.8`.
Do NOT override to `slicer-5.8` — it causes ABI mismatches with `ctk-python`/`vtk-slicer-python`.

### Known Build Issues

| Symptom | Root cause | Fix |
|---|---|---|
| `Slicer_INSTALL_QTLOADABLEMODULES_*` vars empty | `Slicer_BUILD_QTLOADABLEMODULES=OFF` in install tree | Add preamble guards (see above) |
| `VTK_INCLUDE_DIRS` empty | VTK 9.x changed CMake interface | Add `VTK::CommonCore` to `TARGET_LIBRARIES` |
| `ITK_INCLUDE_DIRS` not found | ITK not auto-added | Add `${ITK_INCLUDE_DIRS}` to `INCLUDE_DIRECTORIES` |
| `qSlicerBaseQTCore` undefined | `--as-needed` drops empty `Slicer_GUI_LIBRARY` | Fixed in patches 0025, 0030 |
| `ctkAbstractPythonManager::executeString()` undefined | Not transitively linked in standalone | Add `CTKScriptingPythonCore` to `TARGET_LIBRARIES` |
| `Python3::Python` not linked | Scripted effects use Python C API directly | Add `Python3::Python` to `${KIT}_TARGET_LIBRARIES` when `Slicer_USE_PYTHONQT` |
| `PYTHON_EXECUTABLE` empty | Called before `find_package(Python3)` | Patch to add fallback to `Python3_EXECUTABLE` (patch 0036) |
| `Slicer_BINARY_DIR` empty in install config | Scripted subdirs use build-tree var | Use `${CMAKE_BINARY_DIR}` + `file(MAKE_DIRECTORY ...)` |
| CLI-dependent module fails | `Slicer_BUILD_CLI=OFF` | CropVolume/GeneralizedReformat are blocked — don't package |

### Searching the .scm Snapshot

```bash
# Find make-slicer-loadable-module usage patterns
grep -n 'make-slicer-loadable-module' \
  $GUIX_SYSTOLE_SOURCE/systole/systole/packages/slicer.scm | head -20

# Find inter-module dependency patterns (extra-configure-flags)
grep -B2 -A8 'extra-configure-flags' \
  $GUIX_SYSTOLE_SOURCE/systole/systole/packages/slicer.scm | head -60

# List packaged modules
grep -n 'define-public slicer-' \
  $GUIX_SYSTOLE_SOURCE/systole/systole/packages/slicer.scm

# Find existing patch subdirectories
ls $GUIX_SYSTOLE_SOURCE/systole/systole/packages/patches/slicer/

# Look at a specific module's package definition
grep -A30 'define-public slicer-markups-5.8' \
  $GUIX_SYSTOLE_SOURCE/systole/systole/packages/slicer.scm
```
