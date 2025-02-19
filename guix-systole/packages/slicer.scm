(define-module (slicer)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages freedesktop)
  #:use-module (guix licenses))

(define-public 3d-slicer
  (package
    (name "slicer")
    (version "v5.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Slicer/Slicer")
             (commit (string-append "v" version))))
       (sha256
        ; (base32 "1r61adxxm7x8h2cyy01yxs85m02h58mzw7gbxd82svn8a9hvif49"))))
        (base32 "08w9ib03kjnwgryh3i81sw48kcqlfh3l11wg93nczz7vi3zppimv"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list  "-DSlicer_USE_SYSTEM_LibFFI=ON"
              "-DSlicer_USE_PYTHONQT=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare-build-environment
           (lambda _
             ;; Fix shebang in setup script
             (substitute* "Utilities/SetupForDevelopment.sh"
               (("#!/bin/sh") "#!/bin/bash"))
             
             ;; Create build directory
             (mkdir-p "Slicer-SuperBuild-Debug")
             (chdir "Slicer-SuperBuild-Debug")
             
             ;; Set environment variables
             (setenv "CMAKE_TLS_VERIFY" "0")
             (setenv "EP_EXECUTE_DISABLE_CAPTURE_OUTPUTS" "1")
             #t))
         (replace 'build
           (lambda _
             (invoke "cmake" "-S" "../.."
                     "-DSlicer_USE_SYSTEM_LibFFI=ON"
                     "-DSlicer_USE_PYTHONQT=OFF")
             (invoke "make")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "Slicer-build/Slicer" 
                            (string-append out "/bin"))
               #t))))))

    
    (inputs
      (list libxt
        git
        openssl
        perl
        libffi
        mesa       ;; libGL equivalent
        libxinerama

        wayland

        qtbase-5
        qtmultimedia-5
        qtxmlpatterns
        qtdeclarative-5
        qtsvg-5
        qtx11extras
        qtwebengine-5
        qtwebchannel-5
        qttools-5))
      ;  ("python" ,python-3.14)
    (native-inputs
     `(("gcc" ,gcc)
       ("cmake" ,cmake)
       ("pkg-config" ,pkg-config)))
    (synopsis "3D Slicer - Medical visualization and computing environment")
    (description
     "3D Slicer is a multi-platform, free and open source software package for 
visualization and medical image computing. It provides capabilities for:
@itemize
@item Medical image processing and analysis
@item Segmentation and registration
@item Three-dimensional visualization
@item Support for various imaging modalities
@end itemize")
    (home-page "https://www.slicer.org/")
    (license  bsd-3)))

(define-public 3d-slicer-dev
  (package
    (inherit 3d-slicer)
    (name "3d-slicer-dev")
    (inputs
     `(("3d-slicer" ,3d-slicer)
       ,@(package-inputs 3d-slicer)))
    (native-inputs
     `(("debug-tools" ,gdb)
       ,@(package-native-inputs 3d-slicer)))
    (synopsis "Development environment for 3D Slicer")))