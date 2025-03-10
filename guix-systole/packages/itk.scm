(define-module (guix-systole packages itk)
  #:use-module (gnu packages image-processing)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)

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

                          ;; Modules
                          "-DModule_ITKReview:BOOL=OFF"
                          "-DModule_MGHIO:BOOL=OFF"
                          "-DModule_ITKIOMINC:BOOL=OFF"
                          "-DModule_IOScanco:BOOL=OFF"
                          "-DModule_MorphologicalContourInterpolation:BOOL=OFF"
                          "-DModule_GrowCut:BOOL=ON"
                          "-DITK_FORBID_DOWNLOADS:BOOL=ON")

       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'do-not-tune
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (substitute* "CMake/ITKSetStandardCompilerFlags.cmake"
                        (("-mtune=native")
                         ""))))
                  (add-after 'unpack 'ignore-warnings
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (substitute* "Wrapping/Generators/Python/CMakeLists.txt"
                        (("-Werror")
                         "")))))))

    (inputs (modify-inputs (package-inputs insight-toolkit)
              (append itk-growcut)))

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
     `(#:install-plan '(("." "Modules/Remote/ITKGrowCut/"))
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://github.com/InsightSoftwareConsortium/ITKGrowCut")
    (synopsis "ITK GrowCut segmentation module")
    (description "This package provides the ITK GrowCut segmentation module.")
    (license license:asl2.0)))
