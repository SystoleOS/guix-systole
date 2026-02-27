---
name: slicer
description: >
  Search and reason over 3D Slicer source code, dependencies, and community
  discussions. Use for questions about MRML scene graphs, VTK/ITK/CTK pipelines,
  Slicer Python scripting, C++ module development, extension development, Qt-based
  module UI, segmentation, volume rendering, DICOM workflows, the Slicer build
  system, and guix-systole specific runtime behavior.
version: "5.8.1"
requires: []
---

# Slicer Skill

This skill provides read-only access to 3D Slicer and its core dependency sources,
installed as Guix packages at content-addressed store paths.  All data is
reproducible and version-pinned to the exact commits used by guix-systole.

## Data Sources

All paths are baked into the `slicer-skill` package at build time.  Run
`slicer-skill status` to print the active store paths for the current installation.

| Variable | Content |
|---|---|
| `SLICER_SOURCE` | 3D Slicer source tree (`slicer-source-5.8`) |
| `VTK_SOURCE` | VTK source tree (`vtk-slicer-source`) |
| `ITK_SOURCE` | ITK source tree (`itk-slicer-source`) |
| `CTK_SOURCE` | CTK source tree (`ctk-source`) |
| `SLICER_PATCHES` | guix-systole patches applied to Slicer (`patches/slicer/`) |

---

## Layer 1 — Standard Slicer Development

### Project Structure

Inspect `$SLICER_SOURCE/` for the top-level layout:

- `Base/` — Core application framework
  - `Base/Python/slicer/` — The `slicer` Python package (`util.py`,
    `ScriptedLoadableModule.py`, parameter node wrapper).  Read these to understand
    the Python API surface.
  - `Base/QTCore/` — Non-GUI logic (settings, I/O manager, module factory)
  - `Base/QTGUI/` — Main application GUI (layout manager, module panel)
- `Libs/` — Shared libraries independent of Qt
  - `Libs/MRML/Core/` — MRML scene graph: node classes, events, serialization.
    Header files (`vtkMRML*.h`) document the node hierarchy.
  - `Libs/vtkSegmentationCore/` — Segmentation data structures
  - `Libs/vtkITK/` — VTK/ITK bridge filters
- `Modules/Loadable/` — C++ modules with Qt UI
- `Modules/Scripted/` — Python-only modules
- `Modules/CLI/` — Command-line interface modules (SlicerExecutionModel)

### Module Types

Read these reference implementations:

- **Scripted modules**: `$SLICER_SOURCE/Modules/Scripted/SampleData/` or
  `SegmentStatistics/`.  Base classes in
  `$SLICER_SOURCE/Base/Python/slicer/ScriptedLoadableModule.py`.
- **Loadable modules** (C++ + Qt): `$SLICER_SOURCE/Modules/Loadable/Volumes/` or
  `Markups/`.  Pattern: `qSlicer*Module`, widget, logic, MRML node classes.
- **CLI modules**: `$SLICER_SOURCE/Modules/CLI/AddScalarVolumes/` — XML descriptor
  + C++/Python executable.  Note: CLI is **disabled** in guix-systole (see Layer 2).

### MRML Scene Graph

- Conceptual overview: `$SLICER_SOURCE/Docs/developer_guide/mrml_overview.md`
- Node hierarchy: headers in `$SLICER_SOURCE/Libs/MRML/Core/vtkMRML*Node.h`
- Python API: `$SLICER_SOURCE/Base/Python/slicer/util.py` — `getNode()`,
  `loadVolume()`, `arrayFromVolume()`, `updateVolumeFromArray()`

### Python API

- `$SLICER_SOURCE/Base/Python/slicer/util.py` — primary utility functions
- `$SLICER_SOURCE/Base/Python/slicer/ScriptedLoadableModule.py` — module base classes
- `$SLICER_SOURCE/Base/Python/slicer/parameterNodeWrapper/` — declarative parameters
- `$SLICER_SOURCE/Base/Python/slicer/__init__.py` — top-level namespace

### Script Repository

The script repository at `$SLICER_SOURCE/Docs/developer_guide/script_repository.md`
and `script_repository/*.md` contains working Python recipes.  **Search here first**
before writing code from scratch.  Key files:

| File | Topics |
|---|---|
| `script_repository/volumes.md` | Loading, NumPy access, scalar/vector data |
| `script_repository/segmentations.md` | Segment Editor, effects, import/export |
| `script_repository/markups.md` | Fiducials, curves, planes, ROIs |
| `script_repository/transforms.md` | Linear and non-linear transforms |
| `script_repository/dicom.md` | DICOM loading, exporting, database |
| `script_repository/gui.md` | Layouts, views, keyboard shortcuts |
| `script_repository/models.md` | Surface meshes, polydata |
| `script_repository/sequences.md` | Time sequences |

### VTK and ITK

- VTK headers and examples: `$VTK_SOURCE/`
- ITK headers and examples: `$ITK_SOURCE/`
- Slicer's VTK/ITK bridge: `$SLICER_SOURCE/Libs/vtkITK/`
- VTK pipeline patterns in Slicer: `.cxx` files in `$SLICER_SOURCE/Modules/Loadable/`

### CTK (Common Toolkit)

CTK provides the Qt-based widget library, plugin framework, and DICOM stack.

- Widget catalog: `$CTK_SOURCE/Libs/Visualization/VTK/Widgets/` and
  `$CTK_SOURCE/Libs/Widgets/`
- Python scripting manager: `$CTK_SOURCE/Libs/Scripting/Python/`
- DICOM: `$CTK_SOURCE/Libs/DICOM/`

### Segment Editor

- Module and widget: `$SLICER_SOURCE/Modules/Scripted/SegmentEditor/`
- Python effects (templates for custom effects):
  `$SLICER_SOURCE/Modules/Loadable/Segmentations/EditorEffects/Python/SegmentEditorEffects/`
- Abstract base classes: `AbstractScriptedSegmentEditorEffect.py` in same dir

### Extension Development

- Developer guide: `$SLICER_SOURCE/Docs/developer_guide/extensions.md`
- Extension Wizard (scaffolding tool): `$SLICER_SOURCE/Modules/Scripted/ExtensionWizard/`
- Real extension examples: well-structured extensions live in the Extensions Index

### Common Pitfalls

- **`arrayFromVolume` returns a view.** After in-place modification call
  `slicer.util.arrayFromVolumeModified(node)` to notify the display pipeline.
- **Node names are not unique.** Use `node.GetID()` for reliable identification.
- **Long operations block the UI.** Use `slicer.app.processEvents()` or a background
  thread.
- **Coordinate systems.** Slicer uses RAS internally; many formats use LPS.  Sign
  flips are a common source of bugs.
- **`arrayFromVolume` axis order is KJI** (slice, row, column), not IJK.

---

## Layer 2 — guix-systole Slicer Differences

This section documents how guix-systole Slicer **diverges from upstream** behavior.
Read this when upstream documentation gives conflicting or confusing results.

### Runtime Differences

**`bin/Slicer` is a direct ELF binary symlink**
`bin/Slicer` → `bin/SlicerApp-real`.  There is no launcher wrapper script.  You can
run `gdb bin/SlicerApp-real` directly.  See patch `0045`–`0048` in `$SLICER_PATCHES`.

**Module discovery via environment variable**
Modules are discovered through `SLICER_ADDITIONAL_MODULE_PATHS` (a Guix
`native-search-path`), not via `--additional-module-path` CLI flags.  The env var
scans both `qt-loadable-modules` and `qt-scripted-modules` directories in the
profile.  The CLI flag `--additional-module-path` (singular) only keeps the last
value and should not be used for multiple paths.  Patch: `0045`.

**`LD_LIBRARY_PATH` extended at startup**
At process startup, Slicer extends `LD_LIBRARY_PATH` with every directory listed in
`SLICER_ADDITIONAL_MODULE_PATHS`.  This ensures `dlopen` can find side-libraries of
loadable modules.  Patch: `0046`.

**`PYTHONPATH` assembled at startup**
`PYTHONPATH` is prepended with `CTK_LIBRARY_DIR:vtkAddon_LIB_DIR:SLICER_PYTHONPATH`
at startup.  `CTK_LIBRARY_DIR` and `vtkAddon_LIB_DIR` are compile-time constants
baked into `vtkSlicerConfigure.h`.  `SLICER_PYTHONPATH` is a Guix `native-search-path`
that scans `bin/Python`, `lib/Slicer-5.8`, and `lib/python3.11/site-packages` in the
profile.  Patches: `0046`, `0048`.

**Qt plugin path registered at startup**
`QCoreApplication::addLibraryPath(CTK_LIBRARY_DIR)` is called in
`preInitializeApplication()`, making CTK's Qt plugins available.  `QTWEBENGINE_DISABLE_SANDBOX=1`
is also set.  Patch: `0047`.

### Build Configuration Differences

| Option | guix-systole value | Upstream default |
|---|---|---|
| `Slicer_BUILD_CLI` | `OFF` | `ON` |
| `Slicer_BUILD_CLI_SUPPORT` | `ON` | `ON` |
| `Slicer_BUILD_EXTENSIONMANAGER_SUPPORT` | `OFF` | `ON` |
| `Slicer_BUILD_QTLOADABLEMODULES` | `OFF` (built separately) | `ON` |
| `Slicer_BUILD_QTSCRIPTEDMODULES` | `OFF` (built separately) | `ON` |
| `Slicer_USE_PYTHONQT` | `ON` | `ON` |
| `Slicer_BUILD_ITKPython` | `OFF` | varies |
| `Slicer_BUILD_DICOM_SUPPORT` | `ON` | `ON` |

**Implication of `Slicer_BUILD_CLI=OFF`**: CLI module executables are not available.
The CLI support library (`vtkSlicerCLIModuleLogic`) IS built (via
`Slicer_BUILD_CLI_SUPPORT=ON`), but no CLI executables exist.  Modules that depend
on `vtkSlicerCLIModuleLogic.h` (e.g., CropVolume) cannot be built standalone.

### Standalone Module Development with guix-systole

When building a loadable module as a standalone Guix package (not inside Slicer's
superbuild), the module's top-level `CMakeLists.txt` must include this preamble
before any Slicer macros:

```cmake
cmake_minimum_required(VERSION 3.16.3...3.19.7 FATAL_ERROR)
project(<ModuleName>)
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

The install directory guards are needed because `Slicer_BUILD_QTLOADABLEMODULES=OFF`
in the install tree leaves those variables empty.  Patch `0037` in `$SLICER_PATCHES`
explains the root cause.

Add `LINK_DIRECTORIES ${EXTRA_MODULE_LIB_DIRS}` to all CMake macro calls when the
module depends on other standalone modules (see existing loadable modules in
`guix-systole` for examples).

### Reading the Patches

The patches directory `$SLICER_PATCHES` contains all modifications applied to the
upstream Slicer source.  Each patch is a git commit on the
`guix-systole-slicer-5.8` branch.  Key patches:

| Patch | What it does |
|---|---|
| `0037-COMP-Fix-module-install-dir-variables-for-install-tr.patch` | Exposes `QTLOADABLEMODULES_*` install-dir variables in the install tree; guards recalculation in `UseSlicer.cmake` |
| `0045-ENH-Read-SLICER_ADDITIONAL_MODULE_PATHS-env-var.patch` | Module discovery from env var |
| `0046-ENH-Extend-LD_LIBRARY_PATH-and-PYTHONPATH.patch` | Runtime library and Python path setup |
| `0047-ENH-Register-CTK-plugin-path.patch` | Qt plugin path and WebEngine sandbox |
| `0048-ENH-Prepend-CTK-and-vtkAddon-lib-dirs-to-PYTHONPATH.patch` | Compile-time lib dir constants in `vtkSlicerConfigure.h.in` |

Per-module patches live in subdirectories: `$SLICER_PATCHES/markups/`,
`$SLICER_PATCHES/segmentations/`, `$SLICER_PATCHES/subjecthierarchy/`, etc.

---

## Searching Effectively

```bash
# Find a class definition
grep -rn "class qSlicerMarkupsModule" $SLICER_SOURCE/

# Find CMake macro usage
grep -rn "slicerMacroBuildLoadableModule" $SLICER_SOURCE/Modules/Loadable/

# Find all MRML node headers
find $SLICER_SOURCE/Libs/MRML/Core -name "vtkMRML*Node.h"

# Find VTK class
grep -rn "class VTK_EXPORT vtkMRMLVolumeNode" $VTK_SOURCE/

# Search patch for a specific file
grep -rn "UseSlicer.cmake" $SLICER_PATCHES/

# Find scripted module example
ls $SLICER_SOURCE/Modules/Scripted/
```
