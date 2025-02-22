(define-module (insight-toolkit)
    #:use-module (gnu packages algebra)
    #:use-module (gnu packages base)
    #:use-module (gnu packages ccache)
    #:use-module (gnu packages certs)
    #:use-module (gnu packages cmake)
    #:use-module (gnu packages check)
    #:use-module (gnu packages compression)
    #:use-module (gnu packages image)
    #:use-module ((gnu packages image-processing) #:prefix imgproc:)
    #:use-module (gnu packages gl)
    #:use-module (gnu packages maths)
    #:use-module (gnu packages perl)
    #:use-module (gnu packages pkg-config)
    #:use-module (gnu packages python)
    #:use-module (gnu packages swig)
    #:use-module (gnu packages tbb)
    #:use-module (gnu packages version-control)
    #:use-module (gnu packages xml)
    
    #:use-module (guix build-system cmake)
    #:use-module (guix build python-build-system)
    #:use-module (guix build utils)
    
    #:use-module (guix gexp)
    #:use-module (guix git-download)
    #:use-module ((guix licenses) #:prefix license:)
    #:use-module (guix packages))

(define-public insight-toolkit
  (package
    (name "insight-toolkit")
    (version "5.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Slicer/ITK")
             (commit "29b78d73c81d6c00c393416598d16058704c535c")))
       (sha256(base32 "13iz2f8r5rr9xi8w2j42iidrpn18yi9mkvnw47n6d2wyrvjjl1aj"))))
    (build-system cmake-build-system)
    (arguments
      `( #:tests? #f        ; tests require network access and external data
        #:configure-flags
        (list  
            "-DITK_USE_SYSTEM_GOOGLETEST=OFF"
            "-DITK_USE_GPU=ON"
            "-DITK_USE_SYSTEM_LIBRARIES=ON"
            "-DITK_BUILD_SHARED=ON"
            ;; This prevents "GTest::GTest" from being added to the 
            ;; ITK_LIBRARIES variable in the installed CMake files. This is
            ;; necessary as other packages using insight-toolkit could not be
            ;; configured otherwise.
            "-DGTEST_ROOT=gtest"

            ;; "-DCMAKE_CXX_COMPILER:FILEPATH=${CMAKE_CXX_COMPILER}"
            ;;-DCMAKE_CXX_FLAGS:STRING=${ep_common_cxx_flags}
            ;;-DCMAKE_C_COMPILER:FILEPATH=${CMAKE_C_COMPILER}
            ;;-DCMAKE_C_FLAGS:STRING=${ep_common_c_flags}
            ;;-DCMAKE_CXX_STANDARD:STRING=${CMAKE_CXX_STANDARD}
            ;;-DCMAKE_CXX_STANDARD_REQUIRED:BOOL=${CMAKE_CXX_STANDARD_REQUIRED}
            ;;-DCMAKE_CXX_EXTENSIONS:BOOL=${CMAKE_CXX_EXTENSIONS}
            ;;-DITK_CXX_OPTIMIZATION_FLAGS:STRING= # Force compiler-default instruction set to ensure compatibility with older CPUs
            ;;-DITK_C_OPTIMIZATION_FLAGS:STRING=  # Force compiler-default instruction set to ensure compatibility with older CPUs
            ;;-DITK_INSTALL_ARCHIVE_DIR:PATH=${Slicer_INSTALL_LIB_DIR}
            ;;-DITK_INSTALL_LIBRARY_DIR:PATH=${Slicer_INSTALL_LIB_DIR}
            "-DBUILD_TESTING=OFF"
            "-DBUILD_EXAMPLES=OFF"
            ;;-DITK_BUILD_DEFAULT_MODULES:BOOL=ON
            ;;-DGIT_EXECUTABLE:FILEPATH=${GIT_EXECUTABLE} # Used in ITKModuleRemote
            "-DModule_ITKReview=ON"
            "-DModule_MGHIO=ON"
            "-DModule_ITKIOMINC=ON"
            "-DModule_IOScanco=ON"
            "-DModule_MorphologicalContourInterpolation=ON"
            "-DModule_GrowCut=ON"
            ;;-DModule_SimpleITKFilters:BOOL=${Slicer_USE_SimpleITK}
            ;;-DModule_GenericLabelInterpolator:BOOL=ON
            ;;-DModule_AdaptiveDenoising:BOOL=ON
            ;;-DBUILD_SHARED_LIBS:BOOL=ON
            ;;-DITK_INSTALL_NO_DEVELOPMENT:BOOL=ON
            ;;-DKWSYS_USE_MD5:BOOL=ON # Required by SlicerExecutionModel
            ;;-DITK_WRAPPING:BOOL=OFF #${BUILD_SHARED_LIBS} ## HACK:  QUICK CHANGE
            ;;-DITK_WRAP_PYTHON:BOOL=OFF
            ;;-DExternalData_OBJECT_STORES:PATH=${ExternalData_OBJECT_STORES}
            ;;# VTK
            ;;-DModule_ITKVtkGlue:BOOL=ON
            ;;-DVTK_DIR:PATH=${VTK_DIR}
            ;;# DCMTK
            ;;-DITK_USE_SYSTEM_DCMTK:BOOL=ON
            ;;-DDCMTK_DIR:PATH=${DCMTK_DIR}
            ;;-DModule_ITKIODCMTK:BOOL=${Slicer_BUILD_DICOM_SUPPORT}
            ;;# ZLIB
            ;;-DITK_USE_SYSTEM_ZLIB:BOOL=ON
            ;;-DZLIB_ROOT:PATH=${ZLIB_ROOT}
            ;;-DZLIB_INCLUDE_DIR:PATH=${ZLIB_INCLUDE_DIR}
            ;;-DZLIB_LIBRARY:FILEPATH=${ZLIB_LIBRARY}
        
            ;;;;; OPTIONAL CMAKE CACHE ARGS
            ;; Required for PythonQT
            ; TODO: figure out where the variables come from. Env var?
            ; -DPYTHON_EXECUTABLE:PATH=${PYTHON_EXECUTABLE}
            ; -DPython3_ROOT_DIR:PATH=${Python3_ROOT_DIR}
            ; -DPython3_INCLUDE_DIR:PATH=${Python3_INCLUDE_DIR}
            ; -DPython3_LIBRARY:FILEPATH=${Python3_LIBRARY}
            ; -DPython3_LIBRARY_DEBUG:FILEPATH=${Python3_LIBRARY_DEBUG}
            ; -DPython3_LIBRARY_RELEASE:FILEPATH=${Python3_LIBRARY_RELEASE}
            ; -DPython3_EXECUTABLE:FILEPATH=${Python3_EXECUTABLE}
        
            ;; If we use TBB
            ; -DModule_ITKTBB:BOOL=ON
            ; -DTBB_DIR:PATH=${TBB_DIR}
        
            ; -DITK_LEGACY_REMOVE:BOOL=OFF   #<-- Allow LEGACY ITKv4 features for now.
            ; -DITK_LEGACY_SILENT:BOOL=OFF   #<-- Use of legacy code will produce compiler warnings
            ; -DModule_ITKDeprecated:BOOL=ON #<-- Needed for ITKv5 now. (itkMultiThreader.h and MutexLock backwards compatibility.)
        
            ;; Additional ITK modules
            ;; TODO: The below CMake commands is from https://github.com/Slicer/Slicer/blob/5.8/SuperBuild/External_ITK.cmake.
            ;; We have to figure out if Slicer uses additional modules, and how we can add such modules
            ; #Add additional user specified modules from this variable
            ; #Slicer_ITK_ADDITIONAL_MODULES
            ; #Add -DModule_${module} for each listed module
            ; #Names in list must match the expected module names in the ITK build system
            ; if(DEFINED Slicer_ITK_ADDITIONAL_MODULES)
            ;   foreach(module ${Slicer_ITK_ADDITIONAL_MODULES})
            ;     list(APPEND EXTERNAL_PROJECT_OPTIONAL_CMAKE_CACHE_ARGS
            ;         -DModule_${module}:BOOL=ON
            ;       )
            ;   endforeach()
            ; endif()
                )

          ;;  #:phases 
          ;;  (modify-phases %standard-phases
          ;;      (add-after 'unpack 'do-not-tune
          ;;          (lambda _
          ;;              (substitute* "CMake/ITKSetStandardCompilerFlags.cmake"
          ;;                  (("-mute=native") ""))
          ;;              #t)))
                       ))
    (inputs
     `(("ccache", ccache)
       ("eigen" ,eigen)
       ("expat" ,expat)
       ("fftw" ,fftw)
       ("fftwf" ,fftwf)
       ("git" ,git)
       ("hdf5" ,hdf5)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("mesa" ,mesa-opencl)
       ("nss-certs", nss-certs)
       ("perl" ,perl)
       ("python" ,python-3.9)
       ("tbb" ,tbb)
       ("vxl" ,imgproc:vxl-1)
       ("zlib" ,zlib)
       ))
    (native-inputs
     (list googletest pkg-config))

    ;; The 'CMake/ITKSetStandardCompilerFlags.cmake' file normally sets
    ;; '-mtune=native -march=corei7', suggesting there's something to be
    ;; gained from CPU-specific optimizations.
    ;; (properties '((tunable? . #t)))

    (home-page "https://github.com/Slicer/ITK/")
    (synopsis "Scientific image processing, segmentation and registration")
    (description "The Insight Toolkit (ITK) is a toolkit for N-dimensional
scientific image processing, segmentation, and registration.  Segmentation is
the process of identifying and classifying data found in a digitally sampled
representation.  Typically the sampled representation is an image acquired
from such medical instrumentation as CT or MRI scanners.  Registration is the
task of aligning or developing correspondences between data.  For example, in
the medical environment, a CT scan may be aligned with a MRI scan in order to
combine the information contained in both.")
    (license license:asl2.0)))