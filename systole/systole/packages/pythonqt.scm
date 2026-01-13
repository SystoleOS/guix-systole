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

(define-module (systole packages pythonqt)
  #:use-module (gnu packages)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages python)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public pythonqt-commontk
  (package
    (name "pythonqt-commontk")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/commontk/PythonQt/archive/0580304d8119caaa6c6a985d43f7109d180af880.tar.gz")
       (sha256
        (base32 "0d7kijid6rj9s2wrfbfx2d5552xxxbxpgvjn6iccsa35ssgsj1hz"))))
    (build-system cmake-build-system)
    (inputs (list qtbase-5 python python-sip))
    (arguments
     (list
      ;; #:configure-flags
      ;; (list
      ;; )
      #:tests? #f))
    (home-page "https://github.com/commontk/PythonQt")
    (synopsis "CMake-ified version of PythonQt")
    (description
     "PythonQt is a dynamic Python binding for Qt. It offers an easy way to embed the Python scripting language into your Qt applications.")
    (license license:lgpl2.1)))
