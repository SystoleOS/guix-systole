;;
;; Copyright @ 2026 Oslo University Hospital
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

;;; Commentary:
;;;
;;; SlicerSOFA — a 3D Slicer extension that integrates the SOFA Framework
;;; for biomechanical simulation.  Built from the preview branch of
;;; https://github.com/Slicer/SlicerSOFA against slicer-5.8 and
;;; sofa-framework v25.12 (from (systole packages sofa)).
;;;
;;; All four modules are scripted (Python):
;;;   SlicerSofa, SoftTissueSimulation, SparseGridSimulation, SOFASceneLoader
;;;
;;; Code:

(define-module (systole packages slicer-sofa)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (systole packages slicer)
  #:use-module (systole packages pythonqt)
  #:use-module (systole packages sofa))

(define-public slicer-sofa
  (let ((commit "ff9f67fb8f318dcf02e9995d31f6a3278d529929")
        (revision "0"))
    (package
      (name "slicer-sofa")
      (version (git-version "25.06.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Slicer/SlicerSOFA")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "14l603kwfb03849kx5nm2bgxx28wpkr5fcx546d4q3d4xy5k8g3g"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f
        #:configure-flags
        #~(list "-DCMAKE_BUILD_TYPE=Release"
                "-DSlicerSOFA_SUPERBUILD=OFF"
                (string-append "-DSlicer_DIR="
                               #$(this-package-input "slicer-5.8")
                               "/lib/Slicer-5.8")
                (string-append "-DPYTHONQT_INSTALL_DIR="
                               #$(this-package-input "pythonqt-commontk")))
        #:phases
        #~(modify-phases %standard-phases
            ;; The inner build path in CMakeLists.txt adds the four
            ;; scripted module subdirs and then does CPack/install
            ;; handling for the SOFA libraries.  We only need the
            ;; module install — the SOFA libs are already in the
            ;; sofa-framework package.  Skip the CPack blocks that
            ;; reference ${Sofa_DIR} (which we don't set).
            (add-after 'unpack 'patch-cmakelists
              (lambda _
                ;; Remove everything after the add_subdirectory calls
                ;; that references Sofa_DIR / CPack.
                (substitute* "CMakeLists.txt"
                  (("set\\(EXTENSION_CPACK_INSTALL_CMAKE_PROJECTS\\)")
                   "return() # Guix: skip CPack/SOFA install")))))))
      (inputs
       (list slicer-5.8
             pythonqt-commontk
             sofa-framework))
      (propagated-inputs
       (list slicer-5.8
             sofa-framework))
      (home-page "https://github.com/Slicer/SlicerSOFA")
      (synopsis "3D Slicer extension for SOFA biomechanical simulation")
      (description
       "SlicerSOFA integrates the SOFA Framework (Simulation Open Framework
Architecture) into 3D Slicer, enabling real-time biomechanical simulation
for surgical planning, medical training, and biomedical research.

This package builds four scripted modules from the preview branch:
@code{SlicerSofa} (core bridge), @code{SoftTissueSimulation},
@code{SparseGridSimulation}, and @code{SOFASceneLoader}.")
      (license license:expat))))
