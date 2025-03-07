(define-module (guix-systole packages itk)
  #:use-module (gnu packages image-processing)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages))

(define-public itk-slicer
  (package
    (inherit insight-toolkit)
    (name "itk-slicer")
    (version "5.4.0")
    (source (origin
              (method url-fetch)
              (uri
               "https://github.com/Slicer/ITK/archive/29b78d73c81d6c00c393416598d16058704c535c.tar.gz")
              (sha256
               (base32
                "1cqy2rzcskfjaszww4isp6g2mg4viqcp7qacfvrc97pq1qvrs5lb"))))
    (arguments
     `(#:configure-flags (list "-DITK_USE_SYSTEM_GOOGLETEST:BOOL=OFF"
                          "-DITK_USE_GPU:BOOL=OFF"
                          "-DITK_USE_SYSTEM_LIBRARIES:BOOL=ON"
                          "-DBUILD_TESTING:BOOL=OFF"
                          "-DBUILD_EXAMPLES:BOOL=ON"
                          "-DModule_ITKReview:BOOL=OFF"
                          "-DModule_MGHIO:BOOL=OFF"
                          "-DModule_ITKIOMINC:BOOL=OFF"
                          "-DModule_IOScanco:BOOL=OFF"
                          "-DModule_MorphologicalContourInterpolation:BOOL=OFF"
                          "-DModule_GrowCut:BOOL=ON"
                          "-DITK_FORBID_DOWNLOADS:BOOL=ON")

       #:phases (modify-phases %standard-phases
                  (add-after 'configure 'copy-growcut
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((src (assoc-ref inputs "itk-growcut")))
                        (symlink src "Modules/ITKGrowCut") #t))))))
    (inputs (let ((itk-inputs (package-inputs insight-toolkit)))
              (append itk-inputs
                      `(("itk-growcut" ,(origin
                                      (method git-fetch)
                                      (uri (git-reference (url
                                                           "https://github.com/InsightSoftwareConsortium/ITKGrowCut/")
                                                          (commit
                                                           "cbf93ab65117abfbf5798745117e34f22ff04728")))
                                      (sha256 (base32
                                               "03fzj55bczip5mmis4b074yq7bwjiwzgy49yvqfnnlhhjr9lzkm9"))))))))

    (home-page "https://github.com/Slicer/ITK/")))
