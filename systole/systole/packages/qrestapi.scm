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
  #:use-module (guix download)
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
       (method url-fetch)
       (uri
        "https://github.com/commontk/qRestAPI/archive/88c02c5d90169dfe065fa068969e59ada314d3cb.tar.gz")
       (sha256
        (base32 "0jfnja3frcm4vkibi1vygdh7f4dmhqxni43bbb3rmlcl6jlyaibl"))
       (patches (search-patches
                 "0001-ENH-Refactor-CMake-project-infrastructure.patch"))
       ))
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
    (synopsis
     "Simple Qt library allowing to synchronously or asynchronously query a REST server.")
    (description
     "qRestAPI is a cross-platform, Qt-based library designed to easily query any RESTful web services. It supports multiple interfaces, including qRestAPI for general RESTful APIs, qGirderAPI for Girder, and qMidasAPI for Midas.")
    (license license:asl2.0)))
