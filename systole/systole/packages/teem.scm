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

(define-module (systole packages teem)
  #:use-module (guix build-system cmake)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module ((systole licenses)
                #:prefix license:)
  #:use-module (systole packages))

(define-public teem-slicer
  ;; Slicer's fork pin "slicer-2025-05-18-r7265", required by Slicer >= 5.10.
  ;; It brings a decade of NRRD-parser fixes over the previous 2015 snapshot
  ;; and installs a proper CMake package (TeemConfig.cmake + exported 'teem'
  ;; target under lib/cmake/teem), which obsoleted the local install patch.
  (let ((commit "43395a690351cd98235de84775a8747d0bdae106")
        (revision "0"))
    (package
      (name "teem-slicer")
      (version (git-version "1.12.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Slicer/teem")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1pjk744a14g5vklm8648rxvbiw1xfl65wyyy8j38yhxvqwhas2q6"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f
        #:configure-flags #~(list "-DBUILD_SHARED_LIBS:BOOL=ON"
                                  "-DBUILD_TESTING:BOOL=OFF")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'fix-teem-use-file-path
              ;; The install-tree TeemConfig.cmake points Teem_USE_FILE at
              ;; lib/TeemUse.cmake, but the file is installed to
              ;; lib/cmake/teem/.  Upstream only ever consumes the build-tree
              ;; config (SuperBuild), so the mismatch goes unnoticed there.
              (lambda _
                (substitute* "CMakeLists.txt"
                  (("lib\\$\\{EXTRA_INSTALL_PATH\\}/TeemUse\\.cmake")
                   "lib${EXTRA_INSTALL_PATH}/cmake/teem/TeemUse.cmake")))))))
      (home-page "https://github.com/Slicer/teem/")
      (synopsis "Libraries for representing and processing scientific raster data")
      (description
       "Teem is a coordinated group of libraries for representing, processing,
and visualizing scientific raster data.  Teem includes command-line tools that
permit the library functions to be quickly applied to files and streams,
without having to write any code.  This package builds Slicer's fork of Teem,
used by the @code{vtkTeem} layer of 3D Slicer.")
      (license (list license:slul license:lgpl2.1)))))
