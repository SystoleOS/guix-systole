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
     `(#:tests? #f ;tests require network access and external data
       #:configure-flags (list "-DITK_USE_SYSTEM_GOOGLETEST:BOOL=OFF"
                          "-DITK_USE_GPU:BOOL=OFF"
                          "-DITK_USE_SYSTEM_LIBRARIES:BOOL=ON"
                          ;; "-DITK_BUILD_SHARED:BOOL=ON"
                          ;; This prevents "GTest::GTest" from being added to the
                          ;; ITK_LIBRARIES variable in the installed CMake files. This is
                          ;; necessary as other packages using insight-toolkit could not be
                          ;; configured otherwise.
                          ;; "-DGTEST_ROOT:STRING=gtest"
                          
                          ;; "-DCMAKE_CXX_COMPILER:FILEPATH=${CMAKE_CXX_COMPILER}"
                          ;; -DCMAKE_CXX_FLAGS:STRING=${ep_common_cxx_flags}
                          ;; -DCMAKE_C_COMPILER:FILEPATH=${CMAKE_C_COMPILER}
                          ;; -DCMAKE_C_FLAGS:STRING=${ep_common_c_flags}
                          ;; -DCMAKE_CXX_STANDARD:STRING=${CMAKE_CXX_STANDARD}
                          ;; -DCMAKE_CXX_STANDARD_REQUIRED:BOOL=${CMAKE_CXX_STANDARD_REQUIRED}
                          ;; -DCMAKE_CXX_EXTENSIONS:BOOL=${CMAKE_CXX_EXTENSIONS}
                          ;; -DITK_CXX_OPTIMIZATION_FLAGS:STRING= # Force compiler-default instruction set to ensure compatibility with older CPUs
                          ;; -DITK_C_OPTIMIZATION_FLAGS:STRING=  # Force compiler-default instruction set to ensure compatibility with older CPUs
                          ;; -DITK_INSTALL_ARCHIVE_DIR:PATH=${Slicer_INSTALL_LIB_DIR}
                          ;; -DITK_INSTALL_LIBRARY_DIR:PATH=${Slicer_INSTALL_LIB_DIR}
                          "-DBUILD_TESTING:BOOL=OFF"
                          "-DBUILD_EXAMPLES:BOOL=ON"
                          ;; -DITK_BUILD_DEFAULT_MODULES:BOOL=ON
                          ;; -DGIT_EXECUTABLE:FILEPATH=${GIT_EXECUTABLE} # Used in ITKModuleRemote
                          
                          ;; TODO: Extensions. Build without them first.
                          "-DModule_ITKReview:BOOL=OFF"
                          "-DModule_MGHIO:BOOL=OFF"
                          "-DModule_ITKIOMINC:BOOL=OFF"
                          "-DModule_IOScanco:BOOL=OFF"
                          "-DModule_MorphologicalContourInterpolation:BOOL=OFF"
                          "-DModule_GrowCut:BOOL=ON"
                          "-DITK_FORBID_DOWNLOADS:BOOL=ON"
                          ;; -DModule_SimpleITKFilters:BOOL=${Slicer_USE_SimpleITK}
                          ;; -DModule_GenericLabelInterpolator:BOOL=ON
                          ;; -DModule_AdaptiveDenoising:BOOL=ON
                          ;; -DBUILD_SHARED_LIBS:BOOL=ON
                          ;; -DITK_INSTALL_NO_DEVELOPMENT:BOOL=ON
                          ;; -DKWSYS_USE_MD5:BOOL=ON # Required by SlicerExecutionModel
                          ;; -DITK_WRAPPING:BOOL=OFF #${BUILD_SHARED_LIBS} ## HACK:  QUICK CHANGE
                          ;; -DITK_WRAP_PYTHON:BOOL=OFF
                          ;; -DExternalData_OBJECT_STORES:PATH=${ExternalData_OBJECT_STORES}
                          ;; # VTK
                          ;; -DModule_ITKVtkGlue:BOOL=ON
                          ;; -DVTK_DIR:PATH=${VTK_DIR}
                          ;; # DCMTK
                          ;; -DITK_USE_SYSTEM_DCMTK:BOOL=ON
                          ;; -DDCMTK_DIR:PATH=${DCMTK_DIR}
                          ;; -DModule_ITKIODCMTK:BOOL=${Slicer_BUILD_DICOM_SUPPORT}
                          ;; # ZLIB
                          ;; -DITK_USE_SYSTEM_ZLIB:BOOL=ON
                          ;; -DZLIB_ROOT:PATH=${ZLIB_ROOT}
                          ;; -DZLIB_INCLUDE_DIR:PATH=${ZLIB_INCLUDE_DIR}
                          ;; -DZLIB_LIBRARY:FILEPATH=${ZLIB_LIBRARY}
                          
                          ;; OPTIONAL CMAKE CACHE ARGS
                          ;; Required for PythonQT
                          ;; TODO: figure out where the variables come from. Env var?
                          ;; -DPYTHON_EXECUTABLE:PATH=${PYTHON_EXECUTABLE}
                          ;; -DPython3_ROOT_DIR:PATH=${Python3_ROOT_DIR}
                          ;; -DPython3_INCLUDE_DIR:PATH=${Python3_INCLUDE_DIR}
                          ;; -DPython3_LIBRARY:FILEPATH=${Python3_LIBRARY}
                          ;; -DPython3_LIBRARY_DEBUG:FILEPATH=${Python3_LIBRARY_DEBUG}
                          ;; -DPython3_LIBRARY_RELEASE:FILEPATH=${Python3_LIBRARY_RELEASE}
                          ;; -DPython3_EXECUTABLE:FILEPATH=${Python3_EXECUTABLE}
                          
                          ;; If we use TBB
                          ;; -DModule_ITKTBB:BOOL=ON
                          ;; -DTBB_DIR:PATH=${TBB_DIR}
                          
                          ;; -DITK_LEGACY_REMOVE:BOOL=OFF   #<-- Allow LEGACY ITKv4 features for now.
                          ;; -DITK_LEGACY_SILENT:BOOL=OFF   #<-- Use of legacy code will produce compiler warnings
                          ;; -DModule_ITKDeprecated:BOOL=ON #<-- Needed for ITKv5 now. (itkMultiThreader.h and MutexLock backwards compatibility.)
                          
                          ;; Additional ITK modules
                          ;; TODO: The below CMake commands is from https://github.com/Slicer/Slicer/blob/5.8/SuperBuild/External_ITK.cmake.
                          ;; We have to figure out if Slicer uses additional modules, and how we can add such modules
                          ;; #Add additional user specified modules from this variable
                          ;; #Slicer_ITK_ADDITIONAL_MODULES
                          ;; #Add -DModule_${module} for each listed module
                          ;; #Names in list must match the expected module names in the ITK build system
                          ;; if(DEFINED Slicer_ITK_ADDITIONAL_MODULES)
                          ;; foreach(module ${Slicer_ITK_ADDITIONAL_MODULES})
                          ;; list(APPEND EXTERNAL_PROJECT_OPTIONAL_CMAKE_CACHE_ARGS
                          ;; -DModule_${module}:BOOL=ON
                          ;; )
                          ;; endforeach()
                          ;; endif()
                          )

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
