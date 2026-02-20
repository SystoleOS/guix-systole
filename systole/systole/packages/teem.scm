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
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module ((systole licenses)
                #:prefix license:)
  #:use-module (systole packages))

(define-public teem-slicer
  (package
    (name "teem-slicer")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/Slicer/teem/archive/e4746083c0e1dc0c137124c41eca5d23adf73bfa.tar.gz")
       (sha256
        (base32 "0y8wwzkflj6v5nx0v8cgzryqlxii0px3mcgb3bff1nhyr5zf9yp1"))
       (patches (search-patches
                 "0005-BUG-packages-teem-fix-missing-teem-config-cmake.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags (list "-DBUILD_SHARED_LIBS:BOOL=ON"
                               "-DBUILD_TESTING:BOOL=OFF")))
    ;; https://discourse.slicer.org/t/unknown-cmake-command-vtkmacrokitpythonwrap-in-snapshot-release-for-vtkteem/13838
    (home-page "https://github.com/Slicer/teem/")
    (synopsis
     "A group of libraries for representing, processing and visualizing scientific raster data.")
    (description
     "Teem is a coordinated group of libraries for representing, processing, and visualizing scientific raster data. Teem includes command-line tools that permit the library functions to be quickly applied to files and streams, without having to write any code. The most important and useful libraries in Teem are: ")
    (license (list license:slul license:lgpl2.1))))
