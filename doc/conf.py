project = "SystoleOS Guix Channel"
copyright = "2025-2026, Oslo University Hospital"
author = "SystoleOS Contributors"

extensions = ["myst_parser"]
myst_enable_extensions = ["colon_fence", "deflist"]

source_suffix = {".rst": "restructuredtext", ".md": "markdown"}
master_doc = "index"

exclude_patterns = ["_build"]
html_theme = "alabaster"
html_theme_options = {"github_user": "SystoleOS", "github_repo": "guix-systole"}
