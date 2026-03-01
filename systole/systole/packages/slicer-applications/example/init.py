# SystoleOS Example Application — startup initialization script.
#
# Executed by Slicer after slicerqt.py (Python environment fully initialized).
# Use this script to configure application-level defaults, hide unneeded UI
# elements, or set up application-specific module state.

import slicer

# ── Application identity ──────────────────────────────────────────────────────

app = slicer.app
app.applicationName = "SystoleOS Example Application"

# ── Toolbar visibility ────────────────────────────────────────────────────────

mainWindow = slicer.util.mainWindow()
if mainWindow:
    # Keep only the toolbar elements relevant to this application.
    for toolBarName in ("MouseModeToolBar", "CaptureToolBar"):
        tb = mainWindow.findChild(slicer.qMRMLWidget, toolBarName)
        if tb:
            tb.setVisible(False)

# ── Layout default ─────────────────────────────────────────────────────────────

# Start in the conventional Four-Up layout (three slice views + 3-D view).
# layoutManager() may be None when init.py runs (before the main window is
# fully assembled), so guard against that.
lm = slicer.app.layoutManager()
if lm:
    lm.setLayout(slicer.vtkMRMLLayoutNode.SlicerLayoutFourUpView)

# ── Done ───────────────────────────────────────────────────────────────────────

print("[SystoleOS] Example application initialized.")
