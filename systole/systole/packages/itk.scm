;; 
;; Copyright @ 2025 Oslo University Hospital
;;
;; This file is part of SystoleOS.
;;
;; SystoleOS is free software: you can redistribute it and/or modify it under the 
;; terms of the GNU General Public License as published by the Free Software 
;; Foundation, either version 3 of the License, or (at your option) any later version.
;;
;; SystoleOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
;; PURPOSE. See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along 
;; with SystoleOS. If not, see <https://www.gnu.org/licenses/>.
;; 

(define-module (systole packages itk)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages base)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (systole packages maths)
  #:use-module (gnu packages python)
  #:use-module (systole packages vtk))

(define-public itk-slicer
  (package
    (inherit insight-toolkit)
    (name "itk-slicer")
    (version "5.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Slicer/ITK")
             (commit "29b78d73c81d6c00c393416598d16058704c535c")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13iz2f8r5rr9xi8w2j42iidrpn18yi9mkvnw47n6d2wyrvjjl1aj"))))
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list ;Tests
         "-DITK_USE_SYSTEM_GOOGLETEST:BOOL=OFF"
         "-DBUILD_TESTING:BOOL=OFF"

         ;; Libraries
         "-DITK_USE_SYSTEM_LIBRARIES:BOOL=ON"
         "-DBUILD_SHARED_LIBS:BOOL=ON"

         ;; Misc
         "-DITK_USE_GPU:BOOL=OFF"
         "-DBUILD_EXAMPLES:BOOL=OFF"
         "-DITK_WRAPPING:BOOL=OFF"
         "-DITK_BUILD_DEFAULT_MODULES:BOOL=ON"
         "-DITK_WRAP_PYTHON:BOOL=OFF"
         "-DKWSYS_USE_MD5:BOOL=ON" ;Required by SlicerExecutionModel
         "-DITK_USE_SYSTEM_DCMTK:BOOL=ON"
         "-DITK_USE_SYSTEM_ZLIB:BOOL=ON"

         ;; Modules
         ;; "-DModule_ITKReview:BOOL=ON"
         "-DModule_ITKIODCMTK:BOOL=ON"
         "-DModule_MGHIO:BOOL=ON"
         ;; "-DModule_ITKIOMINC:BOOL=ON"
         "-DModule_IOScanco:BOOL=ON"
         "-DModule_MorphologicalContourInterpolation:BOOL=ON"
         "-DModule_GrowCut:BOOL=ON"
         "-DModule_AdaptiveDenoising:BOOL=ON"
         "-DModule_SimpleITKFilters:BOOL=ON"
         "-DModule_GenericLabelInterpolator:BOOL=ON"
         "-DModule_ITKVtkGlue:BOOL=ON"
         "-DITK_FORBID_DOWNLOADS:BOOL=ON"

         ;; Legacy
         "-DITK_LEGACY_REMOVE:BOOL=OFF" ;<-- Allow LEGACY ITKv4 features for now.
         "-DITK_LEGACY_SILENT:BOOL=OFF" ;<-- Use of legacy code will produce compiler warnings
         "-DModule_ITKDeprecated:BOOL=ON" ;<-- Needed for ITKv5 now. (itkMultiThreader.h and MutexLock backwards compatibility.)

         ;; Optimization: force the compiler-default instruction set
         ;; to ensure compatibility with older CPUs.
         "-DITK_CXX_OPTIMIZATION_FLAGS:STRING="
         "-DITK_C_OPTIMIZATION_FLAGS:STRING=")

      #:phases
      #~(modify-phases %standard-phases
          ;; Symlink the remote-module source trees into the ITK tree.
          (add-before 'configure 'modules-symlink
            (lambda _
              (symlink #$(this-package-input "itk-growcut")
                       "Modules/Remote/ITKGrowCut")
              (symlink #$(this-package-input "itk-mghimageio")
                       "Modules/Remote/ITKMGHIO")
              (symlink #$(this-package-input "itk-adaptivedenoising")
                       "Modules/Remote/ITKAdaptiveDenoising")
              (symlink #$(this-package-input "itk-ioscanco")
                       "Modules/Remote/ITKIOScanco")
              (symlink #$(this-package-input
                          "itk-morphologicalcontourinterpolation")
                       "Modules/Remote/ITKMorphologicalContourInterpolation")
              (symlink #$(this-package-input "itk-iotransformdcmtk")
                       "Modules/Remote/ITKIOTransformDCMTK"))))))

    (inputs (modify-inputs (package-inputs insight-toolkit)
                           (replace "hdf5" hdf5-1.10)
              (append ;vtk
                      dcmtk
                      double-conversion
                      freetype
                      gl2ps
                      glew
                      jsoncpp
                      libharu
                      libtheora
                      libxml++
                      lz4
                      mpich
                      netcdf-slicer
                      proj
                      qtbase-5
                      vtk-slicer

                      ;; GrowCut
                      itk-growcut
                      itk-mghimageio
                      itk-adaptivedenoising
                      itk-ioscanco
                      itk-iotransformdcmtk
                      itk-morphologicalcontourinterpolation)))

    (home-page "https://github.com/Slicer/ITK/")))

(define-public itk-slicer-source
  ;; Upstream ITK source at the exact commit used by itk-slicer.
  ;; No build patches — suitable as a read-only reference for code search.
  (package
    (inherit itk-slicer)
    (name "itk-slicer-source")
    (source (origin (inherit (package-source itk-slicer))
                    (patches '())))
    (build-system copy-build-system)
    (outputs '("out"))
    (inputs '())
    (propagated-inputs '())
    (native-search-paths '())
    (arguments
     ;; The git checkout is already the bare source tree; install it as-is.
     (list #:install-plan #~'(("." "/"))))
    (synopsis "ITK source tree (Slicer variant)")
    (description
     "Upstream ITK source tree at the exact commit used by @code{itk-slicer},
without any Guix-specific build patches.  Useful as a read-only reference for
code search and API exploration.")))

;;
;; Slicer 5.10 variant — ITK 5.4.4
;;

;; ITK 5.4.4 for use by the Slicer 5.10 stack.
;; Inherits all configure flags and remote modules from itk-slicer (5.4.0);
;; only the source commit and vtk dependency are updated.
(define-public itk-slicer-5.4.4
  (package
    (inherit itk-slicer)
    (name "itk-slicer")
    (version "5.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Slicer/ITK")
             (commit "e5dd69339bf0c436db3650eadd3c2a940c330b77")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dxkpd6ws691a4km48w5waii1148wnmkjrd84xv0pkdbbbay46k7"))))
    (inputs (modify-inputs (package-inputs itk-slicer)
              (replace "vtk-slicer" vtk-slicer-9.5)
              (prepend python-3.12)))
    (home-page "https://github.com/Slicer/ITK/")))

;; NOTE: itk-slicer uses vtk-slicer (Python-enabled) after the vtk.scm rename.
;; ITK Python wrapping (ITK_WRAP_PYTHON) remains OFF because python-pygccxml
;; is not packaged in Guix.  Once available, extend itk-slicer with:
;;   #:configure-flags: cons* "-DITK_WRAPPING:BOOL=ON" "-DITK_WRAP_PYTHON:BOOL=ON"
;;   #:native-inputs: prepend castxml python-pygccxml
;;   #:inputs: prepend python

(define itk-growcut
  (package
    (name "itk-growcut")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/InsightSoftwareConsortium/ITKGrowCut")
             (commit "cbf93ab65117abfbf5798745117e34f22ff04728")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03fzj55bczip5mmis4b074yq7bwjiwzgy49yvqfnnlhhjr9lzkm9"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("." "/"))
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/InsightSoftwareConsortium/ITKGrowCut")
    (synopsis "ITK GrowCut segmentation module")
    (description "This package provides the ITK GrowCut segmentation module.")
    (license license:asl2.0)))

(define itk-mghimageio
  (package
    (name "itk-mghimageio")
    (version "5.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/InsightSoftwareConsortium/ITKMGHImageIO")
             (commit "0adac35fa22945c7a5f3a63dd8d01454577c24d3")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x6f9b3vcawfdh8lp7492cvx41p70768a0cy11qiqx9xb2hvhnl9"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("." "Modules/Remote/ITKMGHImageIO/"))
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/InsightSoftwareConsortium/ITKMGHImageIO")
    (synopsis "ITK IO for storing MGH images")
    (description "ITK IO for images stored in mgh, mgz and mgh.gz formats.")
    (license license:cc-by4.0)))

(define itk-adaptivedenoising
  (package
    (name "itk-adaptivedenoising")
    (version "5.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ntustison/ITKAdaptiveDenoising")
             (commit "012ba8882167b64405f7cefc489655f8395093ea")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wl4zsjjska3ar6nhp9bppqh3f2xm8wvy8mb57r86iaaq4dff3fg"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("." "/"))
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/ntustison/ITKAdaptiveDenoising")
    (synopsis "ITK IO for storing MGH images")
    (description "ITK IO for images stored in mgh, mgz and mgh.gz formats.")
    (license license:asl2.0)))

(define itk-ioscanco
  (package
    (name "itk-ioscanco")
    (version "5.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KitwareMedical/ITKIOScanco")
             (commit "12fc12b01a964ccbd30bc8743f4e6cabaa2dcd5e")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n3h9bhpg3fq7p0bqsipwi06h80zn0h11qnpsgqbn4vq2paw4pw4"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("." "/"))
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://itk-io-scanco-app.on.fleek.co/")
    (synopsis "ITK Image IO for Scanco MicroCT .ISQ files")
    (description "ITK Image IO for Scanco MicroCT .ISQ files")
    (license license:asl2.0)))

(define itk-morphologicalcontourinterpolation
  (package
    (name "itk-morphologicalcontourinterpolation")
    (version "5.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KitwareMedical/ITKMorphologicalContourInterpolation")
             (commit "439e40c41ff2676126f5572722e7b2a46a41e776")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03vm94yyia2sddn7c67x20zcz31n56szdnxmxr33snfl6jw54354"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("." "/"))
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://insight-journal.org/browse/publication/977")
    (synopsis
     "An ITK-based implementation of morphological contour interpolation")
    (description
     "An ITK-based implementation of morphological contour interpolation")
    (license license:asl2.0)))

(define itk-iotransformdcmtk
  (package
    (name "itk-iotransformdcmtk")
    (version "5.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/InsightSoftwareConsortium/ITKIOTransformDCMTK")
             (commit "e97e0e8c27809eea1834dd534a47fc06168e3e45")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0808y78q4z2dp9djh0r52h9bv5jm4ypp0c66xvkh2sm8glhw984f"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("." "/"))
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page
     "https://github.com/InsightSoftwareConsortium/ITKIOTransformDCMTK")
    (synopsis "An ITK module to read DICOM spatial transforms.")
    (description "An ITK module to read DICOM spatial transforms.")
    (license license:asl2.0)))
