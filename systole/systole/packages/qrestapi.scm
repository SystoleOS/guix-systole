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

(define-module (systole packages qrestapi)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (systole packages)
  )

(define-public qrestapi
  (package
    (name "qrestapi")
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/commontk/qRestAPI")
             (commit "88c02c5d90169dfe065fa068969e59ada314d3cb")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1na6ac5w2kz496wf83h4zcx9lz2h24gp6arzfizr446xvh8z43qx"))
       (patches (search-patches
                 "0001-ENH-Refactor-CMake-project-infrastructure.patch"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f
       #:configure-flags (list
                          ;; Explicitly use Qt version 5)
                          "-DqRestAPI_QT_VERSION:STRING=5"
                          "-DBUILD_SHARED_LIBS:BOOL=ON"
                          "-DBUILD_TESTING:BOOL=OFF"
                          "-DqRestAPI_STATIC:BOOL=OFF"
                          "-DqRestAPI_INSTALL_NO_DEVELOPMENT:BOOL=OFF")))
    (inputs (list qtbase-5 qtdeclarative-5))
    (home-page "https://github.com/commontk/qRestAPI")
    (synopsis "Qt library to query REST servers synchronously or asynchronously")
    (description
     "qRestAPI is a cross-platform, Qt-based library designed to easily query
any RESTful web services.  It supports multiple interfaces, including
qRestAPI for general RESTful APIs, qGirderAPI for Girder, and qMidasAPI for
Midas.")
    (license license:asl2.0)))

;; Slicer main (slicer-next) pins qRestAPI f933aad, which includes upstream
;; "ENH: Adopt vtkAddon-level packaging": the exported qRestAPI target carries
;; its include directories, and Slicer main relies on that (it dropped both
;; qRestAPI_USE_FILE and the explicit qRestAPI_INCLUDE_DIRS).  The 0001 backport
;; above is part of this commit, so no patch is needed.
(define-public qrestapi-for-slicer-next
  (let ((commit "f933aadb440a8660fe02460142f82cc3cfd9db11")
        (revision "0"))
    (package
      (inherit qrestapi)
      (name "qrestapi-for-slicer-next")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/commontk/qRestAPI")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "11vcy3nx1xw77afmbiy3n0mw13xvqc1a73vf1pmfvpr5d87zxyvw"))))
      (arguments
       (list
        #:tests? #f
        #:configure-flags
        #~(list "-DqRestAPI_QT_VERSION:STRING=5"
                "-DBUILD_SHARED_LIBS:BOOL=ON"
                "-DBUILD_TESTING:BOOL=OFF"
                "-DqRestAPI_INSTALL_DEVELOPMENT:BOOL=ON"
                ;; The default, cmake/qRestAPI, is not searched by
                ;; find_package on GNU/Linux.
                "-DqRestAPI_INSTALL_CMAKE_DIR:STRING=lib/cmake/qRestAPI"))))))
