(define-module (guix-systole packages plustoolkit)
               #:use-module ((guix-systole licenses)
                             #:prefix license:)
               #:use-module (guix packages)
               #:use-module (guix download)
               #:use-module (guix git-download)
               #:use-module (guix gexp)
               #:use-module (guix build-system cmake)
               #:use-module (gnu packages)
               #:use-module (gnu packages base)
               #:use-module (gnu packages qt)
               #:use-module (gnu packages gl)
               #:use-module (gnu packages xorg)
               #:use-module (gnu packages kde)
               #:use-module (gnu packages maths)
               #:use-module (gnu packages xiph)
               #:use-module (gnu packages geo)
               #:use-module (gnu packages serialization)
               #:use-module (gnu packages xml)
               #:use-module (gnu packages pdf)
               #:use-module (gnu packages image)
               #:use-module (gnu packages algebra)
               #:use-module (gnu packages mpi)
               #:use-module (gnu packages compression)
               #:use-module (gnu packages fontutils)
               #:use-module (gnu packages libusb)

               ;; PlusBuild specific modules
               ; #:use-module (gnu packages text-utils)
               #:use-module (gnu packages version-control)
               #:use-module (gnu packages image-processing)
               ;; end PlusBuild specific modules

               #:use-module (guix-systole packages)
               #:use-module (guix-systole packages vtk)
               #:use-module (guix-systole packages itk)
               ; #:use-module (guix-systole packages slicer)
               #:use-module (guix-systole packages igsio)
               #:use-module (guix-systole packages openigtlink)
               )

(define-public pluslib
               (package
                 (name "pluslib")
                 (version "2.8")
                 (source
                   (origin
                     (method url-fetch)
                     (uri
                       "https://github.com/PlusToolkit/PlusLib/archive/refs/heads/Plus-2.8.tar.gz")
                     (sha256
                       (base32
                         "0kp241w3r3bbdbkarwzyyz90qg1zy7wjbkbxbwjd660hsvh3qj0p"
                         )
                       )
                     (patches (search-patches
                                ; "PlusLib_find-vtk-slicer.patch"
                                "0016-COMP-packages-plustoolkit-fix-path-to-VTK-ITK.patch"
                                "0025-COMP-packages-plustoolkit-skip-static-library-check-for-openigtlinkio.patch"
                                ))
                     )
                   )
                 (build-system cmake-build-system)
                 (arguments
                   `(#:configure-flags (list
                                         "-DPLUS_OFFLINE_BUILD:BOOL=ON"
                                         ; "-DPLUS_USE_NVIDIA_DVP:BOOL=OFF"
                                         
                                         ; (string-append "-DOpenIGTLinkIO_DIR=" (assoc-ref %build-inputs "openigtlinkio-igsio") "/lib/cmake/OpenIGTLinkIO")
                                         ; "-DPLUS_USE_OpenIGTLink:BOOL=ON"
                                         ))
                   )
                 (inputs (list vtk-slicer
                               itk-slicer
                               qtbase-5
                               qtmultimedia-5
                               qttools-5
                               qtxmlpatterns
                               qtx11extras
                               qtdeclarative-5
                               qtwebengine-5
                               libglvnd
                               libxt

                               kdevelop
                               
                               glew
                               hdf5
                               libtheora
                               netcdf
                               proj
                               jsoncpp
                               libxml2
                               libharu
                               gl2ps
                               libpng
                               eigen
                               openmpi
                               expat
                               double-conversion
                               lz4
                               ; openjpeg
                               ijg-libjpeg
                               freetype

                               openigtlink
                               openigtlinkio-igsio
                               igsio
                               ))
                 (home-page "plustoolkit.github.io")
                 (synopsis "Software library for data acquisition, pre-processing, and calibration for navigated image-guided interventions.")
                 (description "Software library for data acquisition, pre-processing, and calibration for navigated image-guided interventions. See more information at www.plustoolkit.org.")
                 (license license:plus-license)
                )
               )

(define-public plusbuild
               (package
                 (name "plusbuild")
                 (version "2.8")
                 (source
                   (origin
                     (method url-fetch)
                     (uri
                       "https://github.com/PlusToolkit/PlusBuild/archive/refs/heads/Plus-2.8.tar.gz")
                     (sha256
                       (base32
                         "0pd3q1djxdkdfnalcpq2h5wy4v7h44j1i6yjfhs6f1i8b69sl3w0"))
                     ; (patch-flags '("--ignore-whitespace" "-F" "3"))
                     (patches (search-patches
                                "PlusBuild_test_OS_agnostic.patch"
                                "PlusBuild-deps-allow-preset-src-dir.patch"
                                ; "PlusBuild-disable-VTK_USE_QT-check.patch"
                                ))))
                 (build-system cmake-build-system)
                 (arguments
                   `(
                    #:configure-flags (list
                                          ; "-DPLUSBUILD_USE_GIT_PROTOCOL:BOOL=ON"
                                          "-DPLUSBUILD_OFFLINE_BUILD:BOOL=ON"
                                          ; "-DPLUSBUILD_BUILD_SHARED_LIBS:BOOL=OFF"
                                          "-DPLUSBUILD_BUILD_PLUSAPP:BOOL=OFF"

                                          ; "-DPLUSBUILD_USE_3DSlicer:BOOL=ON"

                                          "-DPLUS_USE_NDI:BOOL=ON"
                                          ; "-DPLUS_USE_BKPROFOCUS_CAMERALINK:BOOL=ON"
                                          ; "-DPLUS_USE_BKPROFOCUS_VIDEO:BOOL=ON"
                                          "-DPLUS_USE_VTKVIDEOIO_MKV:BOOL=ON"
                                          "-DPLUS_USE_IGSIO:BOOL=ON"
                                          "-DPLUS_USE_TextRecognizer:BOOL=ON"
                                          "-DPLUSBUILD_USE_Tesseract:BOOL=ON"
                                          "-DPLUS_USE_Tesseract:BOOL=ON"
                                          "-DPLUS_USE_OvrvisionPro:BOOL=ON"
                                          "-DPLUS_USE_OpenCV_VIDEO:BOOL=ON"
                                          "-DPLUSBUILD_USE_OpenCV:BOOL=ON"
                                          ; "-DPLUS_USE_INFRARED_SEEK_CAM:BOOL=ON"
                                          "-DPLUS_USE_OPTICAL_MARKER_TRACKER:BOOL=ON"
                                          "-DPLUSBUILD_USE_aruco:BOOL=ON"

                                          ;; Set source dirs
                                          
                                          ;; Prebuilt packages
                                          (string-append "-DVTK_DIR:PATH="
                                                         (assoc-ref %build-inputs "vtk")
                                                         "/lib/cmake/vtk-9.2")
                                          (string-append "-DITK_DIR:PATH="
                                                         (assoc-ref %build-inputs "itk")
                                                         "/lib/cmake/ITK-5.4")
                                          ; (string-append "-DOpenCV_DIR:PATH="
                                          ;                (assoc-ref %build-inputs "opencv"))
                                          ; (string-append "-DPLUSBUILD_SLICER_BIN_DIRECTORY:PATH="
                                          ;                (assoc-ref %build-inputs "slicer")
                                          ;                "/lib/Slicer-5.8")

                                          ;; Source code
                                          (string-append "-DPLUS_OpenIGTLink_SRC_DIR:PATH="
                                                         (assoc-ref %build-inputs "OpenIGTLink"))
                                          (string-append "-DPLUS_OpenIGTLinkIO_SRC_DIR:PATH="
                                                         (assoc-ref %build-inputs "OpenIGTLinkIO"))
                                          (string-append "-DPLUS_IGSIO_SRC_DIR:PATH="
                                                         (assoc-ref %build-inputs "IGSIO"))
                                          (string-append "-DPLUS_SeekCameraLib_SRC_DIR:PATH="
                                                         (assoc-ref %build-inputs "InfraredSeekCam"))
                                          (string-append "-DPLUS_IntersonSDKCxx_SRC_DIR:PATH="
                                                         (assoc-ref %build-inputs "IntersonSDKCxx"))
                                          (string-append "-DPLUS_OvrvisionPro_src_DIR:PATH="
                                                         (assoc-ref %build-inputs "OvrvisionPro"))
                                          (string-append "-DPLUS_PLUSAPP_DIR:PATH="
                                                         (assoc-ref %build-inputs "PlusApp"))
                                          (string-append "-DPLUS_PLUSLIB_DIR:PATH="
                                                         (assoc-ref %build-inputs "PlusLib"))
                                          (string-append "-DPLUS_PLUSLIBDATA_DIR:PATH="
                                                         (assoc-ref %build-inputs "PlusLibData"))
                                          (string-append "-DPLUS_PLUSMODELCATALOG_DIR:PATH="
                                                         (assoc-ref %build-inputs "PlusModelCatalog"))
                                          (string-append "-DPLUS_leptonica_src_DIR:PATH="
                                                         (assoc-ref %build-inputs "leptonica"))
                                          (string-append "-DPLUS_tessdata_src_DIR:PATH="
                                                         (assoc-ref %build-inputs "tessdata"))
                                          (string-append "-DPLUS_tesseract_src_DIR:PATH="
                                                         (assoc-ref %build-inputs "Tesseract"))
                                          (string-append "-DPLUS_aruco_src_DIR:PATH="
                                                         (assoc-ref %build-inputs "aruco"))
                                          (string-append "-DPLUS_ndicapi_src_DIR:PATH="
                                                         (assoc-ref %build-inputs "ndicapi"))
                                          (string-append "-DPLUS_OpenCV_src_DIR:PATH="
                                                         (assoc-ref %build-inputs "opencv"))
                                          (string-append "-DLibUSB_DIR:PATH="
                                                         (assoc-ref %build-inputs "libsub"))
                                          )))
                 (inputs `(
                           ("qtbase" ,qtbase-5)
                           ("vtk" ,vtk-slicer)
                           ("itk" ,itk-slicer)
                           ; ("slicer" ,slicer-5.8)
                           ("kdevelop" ,kdevelop)
                           ("qtmultimedia" ,qtmultimedia-5)
                           ("qttools" ,qttools-5)
                           ("libglvnd" ,libglvnd)
                           ("qtxmlpatterns" ,qtxmlpatterns)
                           ("qtx11extras" ,qtx11extras)
                           ("libxt" ,libxt)
                           ("qtdeclarative" ,qtdeclarative-5)
                           ("qtwebengine" ,qtwebengine-5)
                           ; ("opencv" ,opencv)
                           ("glew" ,glew)
                           ("hdf5" ,hdf5-1.10)
                           ("libtheora" ,libtheora)
                           ("netcdf" ,netcdf)
                           ("proj" ,proj)
                           ("jsoncpp" ,jsoncpp)
                           ("libxml2" ,libxml2)
                           ("libharu" ,libharu)
                           ("gl2ps" ,gl2ps)
                           ("libpng" ,libpng)
                           ("eigen" ,eigen)
                           ("openmpi" ,openmpi)
                           ("expat" ,expat)
                           ("double-conversion" ,double-conversion)
                           ("lz4" ,lz4)
                           ("ijg-libjpeg" ,ijg-libjpeg)
                           ("freetype" ,freetype)
                           ("git" ,git)
                           ("libsub" ,libusb)

                            ;; PlusLib
                            ("PlusLib" ,(origin
                              (method git-fetch)
                              (uri (git-reference (url "https://github.com/PlusToolkit/PlusLib.git")
                                                  (commit "Plus-2.8")
                                                  ))
                              (sha256 (base32 "0p94138lldfi2xaf0ql5w2gb8qi7qqdk15c03k4fc8s6x6c1iibd"))
                              ))

                            ;; PlusApp
                            ("PlusApp" ,(origin
                              (method git-fetch)
                              (uri (git-reference (url "https://github.com/PlusToolkit/PlusApp.git")
                                                  (commit "Plus-2.8")
                                                  ))
                              (sha256 (base32 "0p94138lldfi2xaf0ql5w2gb8qi7qqdk15c03k4fc8s6x6c1iibd"))
                              ))

                            ;; IGSIO
                            ("IGSIO" ,(origin
                              (method git-fetch)
                              (uri (git-reference (url "https://github.com/IGSIO/IGSIO.git")
                                                  (commit "93b197fd3f8fb5353ccfdc51c1bca1d6b919d7cb")
                                                  ))
                              (sha256 (base32 "0p94138lldfi2xaf0ql5w2gb8qi7qqdk15c03k4fc8s6x6c1iibd"))
                              ))

                            ;; OpenIGTLink
                            ("OpenIGTLink" ,(origin
                              (method git-fetch)
                              (uri (git-reference (url "https://github.com/openigtlink/OpenIGTLink.git")
                                                  (commit "2081e418c48c02e920487e2284996c1e577c1024")
                                                  ))
                              (sha256 (base32 "0p94138lldfi2xaf0ql5w2gb8qi7qqdk15c03k4fc8s6x6c1iibd"))
                              ))

                            ;; OpenIGTLinkIO
                            ("OpenIGTLinkIO" ,(origin
                              (method git-fetch)
                              (uri (git-reference (url "https://github.com/IGSIO/OpenIGTLinkIO.git")
                                                  (commit "1a2eda5ddb795df8bb5bfbba589c9650095ba4cd")
                                                  ))
                              (sha256 (base32 "0p94138lldfi2xaf0ql5w2gb8qi7qqdk15c03k4fc8s6x6c1iibd"))
                              ))

                            ;; OvrvisionPro
                            ("OvrvisionPro" ,(origin
                              (method git-fetch)
                              (uri (git-reference (url "https://github.com/PlusToolkit/OvrvisionProCMake.git")
                                                  (commit "761a54917e6070a8148dc373d640a2ffe191ac7d")
                                                  ))
                              (sha256 (base32 "0p94138lldfi2xaf0ql5w2gb8qi7qqdk15c03k4fc8s6x6c1iibd"))
                              ))

                            ;; PlusLibData
                            ("PlusLibData" ,(origin
                              (method git-fetch)
                              (uri (git-reference (url "https://github.com/PlusToolkit/PlusLibData.git")
                                                  (commit "Plus-2.8")
                                                  ))
                              (sha256 (base32 "0p94138lldfi2xaf0ql5w2gb8qi7qqdk15c03k4fc8s6x6c1iibd"))
                              ))

                            ;; PlusModelCatalog
                            ("PlusModelCatalog" ,(origin
                              (method git-fetch)
                              (uri (git-reference (url "https://github.com/PlusToolkit/PlusModelCatalog.git")
                                                  (commit "651be1563b40a3552f18436f580d1c14d1b388f9")
                                                  ))
                              (sha256 (base32 "0p94138lldfi2xaf0ql5w2gb8qi7qqdk15c03k4fc8s6x6c1iibd"))
                              ))

                            ;; leptonica
                            ("leptonica" ,(origin
                              (method git-fetch)
                              (uri (git-reference (url "https://github.com/PLUSToolkit/leptonica.git")
                                                  (commit "3130874380fcc2b0268d4f57863cbacdf9d1e9a4")
                                                  ))
                              (sha256 (base32 "0p94138lldfi2xaf0ql5w2gb8qi7qqdk15c03k4fc8s6x6c1iibd"))
                              ))

                            ;; tessdata
                            ("tessdata" ,(origin
                              (method git-fetch)
                              (uri (git-reference (url "https://github.com/PLUSToolkit/tessdata.git")
                                                  (commit "436f296294e97e01a2fbcd0744edf8577c491a8d")
                                                  ))
                              (sha256 (base32 "0p94138lldfi2xaf0ql5w2gb8qi7qqdk15c03k4fc8s6x6c1iibd"))
                              ))

                            ;; Tesseract
                            ("Tesseract" ,(origin
                              (method git-fetch)
                              (uri (git-reference (url "https://github.com/PLUSToolkit/tesseract-ocr-cmake.git")
                                                  (commit "21855d0568a9253dede4e223aae71c0249b90438")
                                                  ))
                              (sha256 (base32 "0p94138lldfi2xaf0ql5w2gb8qi7qqdk15c03k4fc8s6x6c1iibd"))
                              ))

                            ;; aruco
                            ("aruco" ,(origin
                              (method git-fetch)
                              (uri (git-reference (url "https://github.com/PlusToolkit/aruco.git")
                                                  (commit "cf93865edf157aa45c64c8f2084dcef59e58dda3")
                                                  ))
                              (sha256 (base32 "0p94138lldfi2xaf0ql5w2gb8qi7qqdk15c03k4fc8s6x6c1iibd"))
                              ))

                            ;; ndicapi
                            ("ndicapi" ,(origin
                              (method git-fetch)
                              (uri (git-reference (url "https://github.com/PlusToolkit/ndicapi.git")
                                                  (commit "7af09f359cf713de095246bf0dae2763b1c7d135")
                                                  ))
                              (sha256 (base32 "0p94138lldfi2xaf0ql5w2gb8qi7qqdk15c03k4fc8s6x6c1iibd"))
                              ))

                            ;; InfraredSeekCam
                            ("InfraredSeekCam" ,(origin
                              (method git-fetch)
                              (uri (git-reference (url "https://github.com/medtec4susdev/libseek-thermal.git")
                                                  (commit "c4fa40f200ef1277675e882358884049b8934d5e")
                                                  ))
                              (sha256 (base32 "0p94138lldfi2xaf0ql5w2gb8qi7qqdk15c03k4fc8s6x6c1iibd"))
                              ))

                            ;; IntersonSDKCxx
                            ("IntersonSDKCxx" ,(origin
                              (method git-fetch)
                              (uri (git-reference (url "https://github.com/KitwareMedical/IntersonSDKCxx")
                                                  (commit "819d620052be7e9b232e12d8946793c15cfbf5a30")
                                                  ))
                              (sha256 (base32 "0p94138lldfi2xaf0ql5w2gb8qi7qqdk15c03k4fc8s6x6c1iibd"))
                              ))

                            ;; OpenCV (version 3.3.1)
                            ("opencv" ,(origin
                                         (method git-fetch)
                                         (uri (git-reference (url "https://github.com/opencv/opencv.git")
                                                             (commit "0d6518aaa05bc66b5724844938b6920627c5f13c")
                                                             ))
                                         (sha256 (base32 "0p94138lldfi2xaf0ql5w2gb8qi7qqdk15c03k4fc8s6x6c1iibd"))
                                         )
                             )

                            ))
                 (home-page "plustoolkit.github.io")
                 (synopsis "CMake scripts for building Plus library, applications, and all required dependencies")
                 (description "This project contains CMake scripts for building Plus library, applications, and all required dependencies.")
                 (license license:plus-license)))
