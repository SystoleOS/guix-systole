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
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses)
                #:prefix license:))

;;;
;;; Factory
;;;

;; make-pythonqt-commontk generates a PythonQt package linked against a
;; specific Python interpreter.  The python-pkg argument must be the Guix
;; package object; the "X.Y" version string used to construct
;; include/library paths is DERIVED from that package, so the paths can
;; never disagree with the interpreter (the guix default python moving
;; from 3.11 to 3.12 broke exactly that hardcode on 2026-07-18).
;;
;; The python package is captured directly in the gexp via #$python-pkg
;; instead of this-package-input, which avoids hard-coding the input key
;; and makes variant generation trivial.
(define* (make-pythonqt-commontk
          #:key
          (name "pythonqt-commontk")
          (python-pkg python)
          (python-version (version-major+minor (package-version python-pkg)))
          (commit "0580304d8119caaa6c6a985d43f7109d180af880")
          (hash (base32 "0alm2lzg5pvckcaskjzvw8qrrcm07pp5hzy3sljc669gqv6pnaiy")))
  (package
    (name name)
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/commontk/PythonQt")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256 hash)))
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
  (make-pythonqt-commontk))                     ; Guix default python

(define-public pythonqt-commontk-for-slicer-5.10
  (make-pythonqt-commontk #:name "pythonqt-commontk-for-slicer-5.10"
                           #:python-pkg python-3.12))

;; Slicer 5.12 stack.  Pin from commontk/CTK 5056664a
;; (CMakeExternals/PythonQt.cmake: patched-v4.1.0-2026-06-05-9992368e9),
;; the CTK revision referenced by Slicer v5.12.2.
(define-public pythonqt-commontk-for-slicer-5.12
  (make-pythonqt-commontk #:name "pythonqt-commontk-for-slicer-5.12"
                           #:python-pkg python-3.12
                           #:python-version "3.12"
                           #:commit "74dcd675e1515324cd7467a328d63dd25d263679"
                           #:hash (base32 "11wkjv2yskpjhjv15bc2d7a891ybry9w8sadh81bllj0dywrhpqq")))
