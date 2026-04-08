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
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses)
                #:prefix license:))

;;;
;;; Factory
;;;

;; make-pythonqt-commontk generates a PythonQt package linked against a
;; specific Python interpreter.  The python-pkg argument must be the Guix
;; package object; python-version is the "X.Y" string used to construct
;; include/library paths (defaults match python 3.11).
;;
;; The python package is captured directly in the gexp via #$python-pkg
;; instead of this-package-input, which avoids hard-coding the input key
;; and makes variant generation trivial.
(define* (make-pythonqt-commontk
          #:key
          (name "pythonqt-commontk")
          (python-pkg python)
          (python-version "3.11"))
  (package
    (name name)
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/commontk/PythonQt/archive/0580304d8119caaa6c6a985d43f7109d180af880.tar.gz")
       (sha256
        (base32 "0d7kijid6rj9s2wrfbfx2d5552xxxbxpgvjn6iccsa35ssgsj1hz"))))
    (build-system cmake-build-system)
    ;; The commontk fork ships its own SIP generator — python-sip is NOT needed.
    (inputs (list qtbase-5
                  qtmultimedia-5
                  qttools-5
                  python-pkg))
    (arguments
     (list
      #:configure-flags
      #~(list
         "-DPythonQt_QT_VERSION:STRING=5"
         "-DPythonQt_INSTALL_NO_DEVELOPMENT:BOOL=OFF"
         "-DPythonQt_Wrap_Qtcore:BOOL=ON"
         "-DPythonQt_Wrap_Qtgui:BOOL=ON"
         "-DPythonQt_Wrap_Qtuitools:BOOL=ON"
         "-DPythonQt_Wrap_Qtnetwork:BOOL=ON"
         "-DPythonQt_Wrap_Qtmultimedia:BOOL=ON"
         ;; QtWebKit is absent from Qt 5.6+ → keep OFF
         (string-append "-DPython3_EXECUTABLE=" #$python-pkg "/bin/python3")
         (string-append "-DPython3_INCLUDE_DIR=" #$python-pkg "/include/python" #$python-version)
         (string-append "-DPython3_LIBRARY=" #$python-pkg "/lib/libpython" #$python-version ".so"))
      #:tests? #f))
    (home-page "https://github.com/commontk/PythonQt")
    (synopsis "CMake-ified version of PythonQt")
    (description
     "PythonQt is a dynamic Python binding for Qt. It offers an easy way to embed the Python scripting language into your Qt applications.")
    (license license:lgpl2.1)))

;;;
;;; Public instances
;;;

(define-public pythonqt-commontk
  (make-pythonqt-commontk))                     ; python 3.11 (Guix default)

(define-public pythonqt-commontk-for-slicer-5.10
  (make-pythonqt-commontk #:name "pythonqt-commontk-for-slicer-5.10"
                           #:python-pkg python-next
                           #:python-version "3.12"))
