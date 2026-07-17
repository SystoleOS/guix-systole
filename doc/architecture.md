# Architecture: the Slicer Modularization

This channel's centerpiece is a from-scratch packaging of 3D Slicer as
a family of independent Guix packages — the application, every
loadable, scripted, and CLI module, and the libraries underneath — with
no SuperBuild and no vendored dependencies. This document explains why
it exists, how the layers fit together, and what maintaining it
involves.

## Why: no SuperBuild

Upstream Slicer builds through a CMake *SuperBuild* that downloads and
compiles pinned copies of its entire dependency stack (VTK, ITK, CTK,
PythonQt, teem, LibArchive, Python itself, ...) into one monolithic
build tree. That model is incompatible with Guix:

- Guix builds are hermetic — no network access at build time, so the
  SuperBuild's download steps cannot run.
- Guix wants one package per component, so security fixes (e.g. the
  `libarchive-slicer` 3.8.1 update) rebuild only what depends on them,
  and substitutes are granular.
- SystemOS deployments compose the stack per machine: a kiosk image can
  install `slicer-5.8` plus exactly the module packages it needs
  instead of one all-or-nothing bundle.

So the channel builds Slicer with `Slicer_SUPERBUILD=OFF` against
system (Guix-provided) libraries, and carries a patch series that
teaches Slicer's CMake infrastructure to work as a normal
installed-tree dependency: install development files, expose install
paths in `SlicerConfig.cmake`, and let each module build as a
standalone CMake project against an *installed* Slicer.

## Module layout

`(systole packages slicer)` is a thin facade; the real definitions live
in three per-concern modules, all under
`systole/systole/packages/`:

| Module | File | Contents |
|---|---|---|
| `(systole packages slicer-factory)` | `slicer-factory.scm` | version-neutral build infrastructure: `slicerexecutionmodel` and the three module factories |
| `(systole packages slicer-5-8)` | `slicer-5-8.scm` | the Slicer 5.8.1 stack: `slicer-5.8`, 21 loadable + 17 scripted + 23 CLI module packages, `slicer-all-5.8` |
| `(systole packages slicer-5-10)` | `slicer-5-10.scm` | the Slicer 5.10.0 stack: `slicer-5.10`, 21 loadable + 18 scripted module packages, `slicer-all-5.10` |

The facade re-exports every public binding from all three, so
`#:use-module (systole packages slicer)` and
`guix build -L systole slicer-5.8` keep working regardless of where a
binding is defined.

The per-version module names use `-5-8`/`-5-10` rather than
`-5.8`/`-5.10`: Guile's load-path search treats a dot in the last
module-name component as a file extension and refuses to append
`.scm`, so dotted module names can never be autoloaded.

## The layer diagram

Python support is a hard requirement and always enabled. There is no
`slicer-python-*` package family and no plain `slicer` package: the
canonical names *are* the Python-enabled variants. Private non-Python
bases exist only as unexported `%`-prefixed definitions (`%slicer-5.8`,
`%ctk`, `%vtk-slicer`, ...).

```
pythonqt-commontk          (PythonQt with CTK patches)
    └── ctk                (Python scripting ON)
        ├── vtk-slicer     (VTK_WRAP_PYTHON=ON)
        ├── vtkaddon       (vtkAddon_WRAP_PYTHON=ON)
        └── itk-slicer     (no ITK Python wrapping yet)
            └── slicer-5.8                 (application; bin/Slicer → SlicerApp-real)
                ├── slicer-terminologies-5.8, slicer-markups-5.8, ...   (21 loadable)
                ├── slicer-sampledata-5.8, slicer-dicom-5.8, ...        (17 scripted)
                ├── slicer-add-scalar-volumes-5.8, ...                  (23 CLI)
                └── slicer-all-5.8         (meta-package: everything above
                                            + numpy/scipy/pydicom/requests/
                                              dicomweb-client/pip)
```

The 5.10 stack mirrors this shape (`slicer-5.10`, `slicer-all-5.10`;
scripted modules additionally include `lineprofile`; CLI module
packages are not yet ported to 5.10).

**ABI rule:** every standalone module is built against the canonical
base Slicer of its stack (`slicer-5.8` or `slicer-5.10`), whose
`SlicerConfig.cmake` unconditionally sets `Slicer_USE_PYTHONQT=ON`, so
module builds produce `*Python.so`/`*PythonQt.so` wrappers matching the
application's ABI. Mixing Python and non-Python VTK/CTK variants in
one build causes CMake "Some but not all targets already defined"
errors — another reason the non-Python bases are private.

Python runtime packages (numpy, scipy, pydicom, ...) are deliberately
propagated by `slicer-all-<version>` and *not* by the base package:
a `--without-tests=<pkg>` transformation on any of them would otherwise
cascade through the base Slicer into every module package and produce
duplicate derivations.

## The factory functions

`slicer-factory.scm` exports three factories. All are parameterized on
the base Slicer, so both version stacks share one implementation; each
per-version module defines local wrappers with the version-specific
arguments pre-applied:

```scheme
;; slicer-5-8.scm
(define (make-slicer-loadable-module . args)
  (apply factory:make-slicer-loadable-module
         #:slicer slicer-5.8
         #:slicer-version "5.8"
         #:pythonqt pythonqt-commontk
         args))
```

Common keywords (all three factories):

| Keyword | Meaning |
|---|---|
| `#:slicer` | base Slicer package (source origin, version, license; loadable/scripted also build against it) |
| `#:slicer-version` | version string, e.g. `"5.8"` — selects the patch directory and the `lib/Slicer-<version>` install layout |
| `#:name` | package name, e.g. `"slicer-volumes-5.8"` |
| `#:module-subdir` | source subdirectory under `Modules/Loadable/`, `Modules/Scripted/`, or `Modules/CLI/` |
| `#:patches` | patch file names relative to `patches/slicer-<version>/` (**replace** the base package's patches) |
| `#:synopsis`, `#:description` | package metadata |
| `#:extra-inputs` | extra build-time inputs (inter-module build deps) |
| `#:extra-configure-flags` | gexp evaluating to extra CMake `-D` flags |

Factory-specific details:

- **`make-slicer-loadable-module`** additionally takes `#:pythonqt`
  (passed as `PYTHONQT_INSTALL_DIR` so `find_package(PythonQt)` inside
  `UseSlicer.cmake` resolves) and `#:propagated-inputs` (runtime module
  deps). It configures `Modules/Loadable/<subdir>` as a standalone
  project with `-DSlicer_DIR=<slicer>/lib/Slicer-<version>` and
  `-DSlicer_INSTALL_DEVELOPMENT=ON` (so downstream modules can build
  against it). Inputs are the base Slicer's own inputs plus the base
  Slicer itself plus `#:extra-inputs`; the base Slicer and the declared
  runtime deps are propagated.
- **`make-slicer-scripted-module`** is the same pattern for
  `Modules/Scripted/<subdir>` (no `#:pythonqt` needed).
- **`make-slicer-cli-module`** builds `Modules/CLI/<subdir>` against
  SlicerExecutionModel + ITK only — pure-ITK CLI modules never call
  `find_package(Slicer)`, so no installed Slicer, VTK, CTK, or Qt is
  needed. Headers like `itkPluginUtilities.h` are read directly from
  the Slicer *source* tree during configure. Executables install to
  `lib/Slicer-<version>/cli-modules`.

Note the factories do **not** `(inherit slicer)`: Guix forbids a
package appearing both as an input and in its own inheritance chain.
Only the source origin is reused; each module is an independent
package.

## Inter-module dependency wiring

Slicer modules link against each other (Markups uses Colors widgets,
SubjectHierarchy needs Terminologies, ...). A dependent module
declares:

```scheme
(define-public slicer-markups-5.8
  (make-slicer-loadable-module
   ...
   ;; build-time: headers + libs of other modules
   #:extra-inputs (list slicer-colors-5.8 slicer-annotations-5.8)
   #:extra-configure-flags
   #~(list (string-append "-DvtkSlicerColorsModuleLogic_INCLUDE_DIRS="
                          #$slicer-colors-5.8
                          "/include/Slicer-5.8/qt-loadable-modules/vtkSlicerColorsModuleLogic")
           (string-append "-DEXTRA_MODULE_LIB_DIRS="
                          #$slicer-colors-5.8 "/lib/Slicer-5.8/qt-loadable-modules;"
                          #$slicer-annotations-5.8 "/lib/Slicer-5.8/qt-loadable-modules"))
   ;; runtime: modules dlopen-ed alongside this one
   #:propagated-inputs (list slicer-colors-5.8 slicer-annotations-5.8)))
```

- `<LibName>_INCLUDE_DIRS` replaces the `_SOURCE_DIR`/`_BINARY_DIR`
  variables the in-tree build would have used (the module patches
  rewrite the CMakeLists accordingly).
- `EXTRA_MODULE_LIB_DIRS` is consumed by the patched Slicer build
  macros (`SlicerMacroBuildLoadableModule`, `SlicerMacroBuildModuleLogic`,
  `SlicerMacroBuildModuleMRML`, `SlicerMacroBuildModuleWidgets`, ...)
  as `LINK_DIRECTORIES`; use CMake's `;` separator for multiple paths.
- `#:propagated-inputs` ensures the depended-on module lands in any
  profile that installs the dependent one, so runtime discovery (below)
  finds its shared libraries.

## Runtime module discovery

A Guix profile containing `slicer-5.8` plus module packages is a
symlink farm; nothing tells vanilla Slicer where the modules are. The
channel closes that gap with search paths declared on the base Slicer
package plus a set of startup patches (numbers below are the 5.8
series; 5.10 carries equivalents):

- `SLICER_ADDITIONAL_MODULE_PATHS` (`native-search-path`): Guix
  populates it from every profile package's
  `lib/Slicer-<version>/qt-loadable-modules`, `qt-scripted-modules`,
  and `cli-modules` directories.
- `SLICER_PYTHONPATH` (`native-search-path`): collects `bin/Python`,
  `lib/Slicer-<version>`, and `lib/python3.x/site-packages` from the
  profile. (A profile-level `PYTHONPATH` search path existed briefly
  and was removed — it leaked Slicer's Python onto every interpreter in
  the profile; see `etc/news.scm`.)
- `SLICER_INIT_DIR` (`native-search-path`, single-value): a theming
  package can provide `splash.png`, `style.qss`, and `init.py` under
  `share/slicer-init/`.
- `CMAKE_PREFIX_PATH` (files `("")`) so extension builds re-resolve
  transitive CMake dependencies from the profile.

Startup/config patch responsibilities:

| Patch | Responsibility |
|---|---|
| 0045 | read `SLICER_ADDITIONAL_MODULE_PATHS` as additional module directories at startup |
| 0046 | extend `LD_LIBRARY_PATH` and build the in-process `PYTHONPATH` from `SLICER_ADDITIONAL_MODULE_PATHS`/`SLICER_PYTHONPATH` at `init()` time — before any module `dlopen()`; glibc re-reads `LD_LIBRARY_PATH` on each `dlopen()` call, so this is sufficient |
| 0047 | register the CTK plugin path with Qt and disable the QtWebEngine sandbox |
| 0048 | prepend the CTK and vtkAddon lib dirs to `PYTHONPATH` (exposes `vtkAddon_LIB_DIR` via `vtkSlicerConfigure.h.in`) |
| 0051–0052 | load splash screen/stylesheet and run `init.py` from `SLICER_INIT_DIR` |
| 0071 | expose `Slicer_QTLOADABLEMODULES_{SUBDIR,BIN_DIR,LIB_DIR}` in the install-tree `SlicerConfig.cmake` so extension builds install their `.so` files into `lib/Slicer-<version>/qt-loadable-modules/` rather than the build root |
| 0072 | register the Slicer qMRML *designer plugins* directory with Qt (`QCoreApplication::addLibraryPath`) so `slicer.util.loadUI()` resolves qMRML custom widgets |

The rest of the base series (0001–0044, 0053–0079 for 5.8) is CMake
surgery: finding system dependencies without SuperBuild, installing
development files, and exposing install-tree variables. Patches carry
`COMP`/`ENH` prefixes following the Slicer commit convention.

`bin/Slicer` is a plain symlink to `SlicerApp-real` (no CTK applauncher
wrapper); all startup environment handling happens in C++ via the
patches above.

## Patch workflow

Patches are git commits on named branches of a Slicer fork checkout
(conventionally `~/src/Slicer/Slicer-Systole`):

- Base branches: `guix-systole-slicer-5.8` (forked at upstream commit
  `11eaf62e5a70b828021ff8beebbdd14d10d4f51c`) and
  `guix-systole-slicer-5.10` (forked at
  `a2b6d082be04274a849884fbb1e85634a9df90fb`).
- Per-module branches: `guix-systole-<name>-module-<x.y.z>` (loadable)
  and `guix-systole-<name>-scripted-module-<x.y.z>` (scripted), each
  containing only that module's standalone-build commits on top of the
  base commit.

Regenerate a module's patch series with an absolute output directory:

```bash
cd ~/src/Slicer/Slicer-Systole
git format-patch -N <base-commit>..HEAD \
  --output-directory /path/to/guix-systole/systole/systole/packages/patches/slicer-<version>/<name>/
```

On the channel side, patches live in version-scoped directories —
`patches/slicer-5.8/` and `patches/slicer-5.10/`, with per-module
subdirectories (`markups/`, `subjecthierarchy/`, `cli/<name>/`, ...) —
and are resolved by the `slicer-patch` helper in `(systole packages)`:

```scheme
(define (slicer-patch version name)
  (local-file (string-append systole-patches "/slicer-" version "/" name)
              (basename name)))
```

Using `local-file` bypasses the global `%patch-path`, so 5.8 and 5.10
patches with identical relative names (most of them) resolve
independently. The factories map `#:patches` through this helper with
their `#:slicer-version`.

Two rules worth repeating:

1. **Module patches replace, never append.** A fix in the base
   package's patch list is invisible to standalone module builds.
2. **Standalone modules need a CMakeLists preamble patch** —
   `cmake_minimum_required`/`project()`/`find_package(Slicer)` plus
   fallbacks for the `Slicer_INSTALL_QT*MODULES_*_DIR` variables — since
   upstream assumes `add_subdirectory()` from the Slicer root.

## The 5.8/5.10 parallel-stack pattern

Both stacks coexist in one channel and one profile-naming scheme:
every package name and every install path carries the version
(`slicer-markups-5.8` vs `slicer-markups-5.10`,
`lib/Slicer-5.8/...` vs `lib/Slicer-5.10/...`), and patches live in
disjoint directories. Nothing is shared at build time except the
factory code, `slicerexecutionmodel`, and the library stack
(`vtk-slicer`, `ctk`, `itk-slicer`, ... are currently common to both).

Bumping to a new Slicer version therefore means:

1. Fork a new base branch `guix-systole-slicer-<new>` in
   Slicer-Systole at the upstream release commit and rebase/port the
   base patch series; regenerate into `patches/slicer-<new>/`.
2. Register the new patch directory in `%patch-path`
   (`systole/systole/packages.scm`) — needed for the base package's
   `search-patches`; the module factories use `slicer-patch` and only
   need the directory to exist.
3. Create `slicer-5-<new>.scm` (start from the previous version's
   module), define `%slicer-<new>`/`slicer-<new>`, the factory
   wrappers, port each module branch and its patches, and define the
   module lists and `slicer-all-<new>`.
4. Add the new module to the facade list in `slicer.scm`.
5. Update the search-path file lists (`lib/Slicer-<new>/...`) and any
   version-derived flags; check Python minor-version changes (5.10
   moved to Python 3.12, which required `python-numpy-3.12`).
6. Run `./scripts/run-tests.sh packages` (the sweep loads every module
   and touches every package) and build `slicer-all-<new>`.

## Related documents

- [doc/slicer-packaging-notes.md](slicer-packaging-notes.md) — detailed
  design notes and the annotated package source walkthrough (formerly
  `packages/slicer.md`).
- [doc/testing.md](testing.md) — the test harness.
- [doc/channel-management.md](channel-management.md) — channel pinning
  and reproducible builds.
