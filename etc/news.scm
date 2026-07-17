;;; Channel news for guix-systole.
;;; Displayed by 'guix pull --news'; see "Writing Channel News" in the
;;; Guix manual.  Newest entries first.  Entry commits must be ancestors
;;; on the channel branch users pull.

(channel-news
 (version 0)

 (entry (commit "8395c62440fd096b147b3918fc7a85b6aad32b61")
        (title (en "Slicer packages no longer set PYTHONPATH"))
        (body
         (en "The @code{slicer-5.8} and @code{slicer-5.10} packages no longer
declare a @env{PYTHONPATH} native search path.  Guix dropped profile-level
@env{PYTHONPATH} in 2021; a profile containing Slicer would force Slicer's
Python 3.11/3.12 module paths onto every other Python interpreter in that
profile.  Slicer's own module discovery is unaffected: the
@env{SLICER_PYTHONPATH} search path remains and Slicer builds its in-process
Python path from it at startup.  To use Slicer's Python modules from a plain
@command{python3}, set @env{PYTHONPATH} from @env{SLICER_PYTHONPATH}
explicitly.")))

 (entry (commit "89c36d9555e196fc820e2ac7f444d88b5bf4b5d9")
        (title (en "guix.moe substitutes are now opt-in"))
        (body
         (en "The @code{systole-transformation-guix} procedure no longer
authorizes the community-run @code{cache-cdn.guix.moe} substitute server by
default.  Pass @code{#:community-substitutes? #t} to keep using it.  The
public installer images still opt in so that binary substitutes for the
Slicer stack remain available; systems built directly from the
transformations now default to nonguix substitutes only.")))

 (entry (commit "3ab96c4050668d958364b1960c8070b4409aa496")
        (title (en "Security update: libarchive 3.8.1 and teem r7265"))
        (body
         (en "@code{libarchive-slicer} moves from the 2022 3.6.1 snapshot to
Slicer's @code{slicer-v3.8.1-2025-06-01} branch, picking up CVE-2022-36227
and the 2025 RAR series fixes (CVE-2025-5914 through CVE-2025-5918).
@code{teem-slicer} moves from a 2015 snapshot to
@code{slicer-2025-05-18-r7265}.  Both pins match what upstream Slicer
requires from 5.10 on.  Slicer packages rebuild as a consequence.")))

 (entry (commit "5ad4d83dd58fe755570daebecc74aad4e6756f20")
        (title (en "Slicer 5.10 stack available"))
        (body
         (en "A complete, parallel 3D Slicer 5.10.0 stack is now available:
@code{slicer-5.10}, per-module packages, and the @code{slicer-all-5.10}
meta-package, together with the matching dependency variants
(@code{vtk-slicer-9.5}, @code{itk-slicer} 5.4.4, @code{ctk-for-slicer-5.10},
Python 3.12).  The 5.8 stack is unchanged.")))

 (entry (commit "6bfc86f699eb74dca9027c1f6b2e69b6a983d1e1")
        (title (en "Python-enabled packages are now canonical; -python names removed"))
        (body
         (en "Python support became a hard requirement across the Slicer
stack, and the separate @code{-python} package family was folded into the
canonical names: use @code{ctk}, @code{vtk-slicer}, @code{vtkaddon},
@code{slicer-5.8}, @code{igsio}, @code{slicer-igt}, etc.  The former
@code{*-python} names (e.g. @code{slicer-python-5.8},
@code{slicer-python-all-5.8}) no longer exist; non-Python private bases are
unexported.")))
 )
