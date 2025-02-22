(define-module (guix-systole packages itk)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages xiph)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix-systole packages vtk))

(define-public itk-slicer
  (package
    (inherit insight-toolkit)
    (name "itk-slicer")
    (version "5.4.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/Slicer/ITK/archive/29b78d73c81d6c00c393416598d16058704c535c.tar.gz")
       (sha256
        (base32 "1cqy2rzcskfjaszww4isp6g2mg4viqcp7qacfvrc97pq1qvrs5lb"))))
    (arguments
     `(#:tests? #f
       #:configure-flags (list ;Tests
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
                          "-DITK_INSTALL_NO_DEVELOPMENT:BOOL=ON"
                          "-DITK_WRAP_PYTHON:BOOL=OFF"
                          "-DKWSYS_USE_MD5:BOOL=ON" ;Required by SlicerExecutionModel
                          "-DITK_USE_SYSTEM_DCMTK:BOOL=ON"
                          "-DITK_USE_SYSTEM_ZLIB:BOOL=ON"

                          ;; Modules
                          ;; "-DModule_ITKReview:BOOL=ON"
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
                          
                          ;; Optimization
                          "-DITK_CXX_OPTIMIZATION_FLAGS:STRING=" ;Force compiler-default instruction set to ensure compatibility with older CPUs
                          "-DITK_C_OPTIMIZATION_FLAGS:STRING=" ;Force compiler-default instruction set to ensure compatibility with older CPUs
                          )

       #:phases (modify-phases %standard-phases
                  ;; Symlink modules
                  (add-before 'configure 'modules-symlink
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (symlink (assoc-ref inputs "itk-growcut")
                               "Modules/Remote/ITKGrowCut")
                      (symlink (assoc-ref inputs "itk-mghimageio")
                               "Modules/Remote/ITKMGHIO")
                      (symlink (assoc-ref inputs "itk-adaptivedenoising")
                               "Modules/Remote/ITKAdaptiveDenoising")
                      (symlink (assoc-ref inputs "itk-ioscanco")
                               "Modules/Remote/ITKIOScanco")
                      (symlink (assoc-ref inputs
                                "itk-morphologicalcontourinterpolation")
                       "Modules/Remote/ITKMorphologicalContourInterpolation")
                      #t)))))

    (inputs (modify-inputs (package-inputs insight-toolkit)
              (append ;vtk
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
                      netcdf
                      proj
                      vtk

                      ;; GrowCut
                      itk-growcut
                      itk-mghimageio
                      itk-adaptivedenoising
                      itk-ioscanco
                      itk-morphologicalcontourinterpolation)))

    (home-page "https://github.com/Slicer/ITK/")))

(define-public itk-growcut
  (package
    (name "itk-growcut")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/InsightSoftwareConsortium/ITKGrowCut/archive/cbf93ab65117abfbf5798745117e34f22ff04728.tar.gz")
       (sha256
        (base32 "0is0a2lic6r3d2h4md7csmlbpphfwgqkjmwlh7yvwfbyy1mdngbd"))))
    (build-system copy-build-system)
    (arguments
     
     `(#:install-plan '(("." "/"))
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/InsightSoftwareConsortium/ITKGrowCut")
    (synopsis "ITK GrowCut segmentation module")
    (description "This package provides the ITK GrowCut segmentation module.")
    (license license:asl2.0)))

(define-public itk-mghimageio
  (package
    (name "itk-mghimageio")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/InsightSoftwareConsortium/ITKMGHImageIO/archive/0adac35fa22945c7a5f3a63dd8d01454577c24d3.tar.gz")
       (sha256
        (base32 "10b3qzxwk2jq17dbcihfj6l2arr27wa6z7p9pvj7jjxfp512khqy"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("." "Modules/Remote/ITKMGHImageIO/"))
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/InsightSoftwareConsortium/ITKMGHImageIO")
    (synopsis "ITK IO for storing MGH images")
    (description "ITK IO for images stored in mgh, mgz and mgh.gz formats.")
    (license license:cc-by4.0)))

(define-public itk-adaptivedenoising
  (package
    (name "itk-adaptivedenoising")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/ntustison/ITKAdaptiveDenoising/archive/012ba8882167b64405f7cefc489655f8395093ea.tar.gz")
       (sha256
        (base32 "0mmxx2csrn67a9b6bhixwna7kgphijwz20312b0qwlriw54c61r5"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("." "/"))
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/ntustison/ITKAdaptiveDenoising")
    (synopsis "ITK IO for storing MGH images")
    (description "ITK IO for images stored in mgh, mgz and mgh.gz formats.")
    (license license:asl2.0)))

(define-public itk-ioscanco
  (package
    (name "itk-ioscanco")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/KitwareMedical/ITKIOScanco/archive/12fc12b01a964ccbd30bc8743f4e6cabaa2dcd5e.tar.gz")
       (sha256
        (base32 "1fv5qr8jax820rabn1vrv7bjxziwk0lwmcvc5mq86q2kclnjlyz4"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("." "/"))
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://itk-io-scanco-app.on.fleek.co/")
    (synopsis "ITK Image IO for Scanco MicroCT .ISQ files")
    (description "ITK Image IO for Scanco MicroCT .ISQ files")
    (license license:asl2.0)))

(define-public itk-morphologicalcontourinterpolation
  (package
    (name "itk-morphologicalcontourinterpolation")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/KitwareMedical/ITKMorphologicalContourInterpolation/archive/439e40c41ff2676126f5572722e7b2a46a41e776.tar.gz")
       (sha256
        (base32 "07s260k29gvra5wnxjn19wr3cbd6rdvd9pcnkawis0c7ji33g0lm"))))
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
