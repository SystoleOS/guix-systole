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
;;; https://github.com/Slicer/SlicerSOFA (identical to main once
;;; Slicer/SlicerSOFA#83 is merged; the ExtensionsIndex builds Slicer 5.12
;;; and Preview from it) against slicer-5.12 and sofa-framework v26.06
;;; (from (systole packages sofa)).
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
  #:use-module (gnu packages python)
  #:use-module (systole packages slicer)
  #:use-module (systole packages pythonqt)
  #:use-module (systole packages sofa)
  #:use-module (srfi srfi-1))

(define-public slicer-sofa
  (let ((commit "7f086b160704a62298f731f11a3ebcce8a023f19")
        (revision "0"))
    (package
      (name "slicer-sofa")
      (version (git-version "26.06.00" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Slicer/SlicerSOFA")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32 "10mppsvg72hwfs4j05zvhnal6rcfh6nc4hrs8fkrk80yxh08h13n"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f
        #:configure-flags
        #~(list "-DCMAKE_BUILD_TYPE=Release"
                "-DSlicerSOFA_SUPERBUILD=OFF"
                (string-append "-DSlicer_DIR="
                               #$(this-package-input "slicer-5.12")
                               "/lib/Slicer-5.12")
                (string-append "-DPYTHONQT_INSTALL_DIR="
                               #$(this-package-input
                                  "pythonqt-commontk-for-slicer-5.12")))
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
                (substitute* "CMakeLists.txt"
                  (("set\\(EXTENSION_CPACK_INSTALL_CMAKE_PROJECTS\\)")
                   "return() # Guix: skip CPack/SOFA install"))))
            (add-after 'unpack 'patch-sofa-environment
              (lambda _
                ;; Rewrite SofaEnvironment/__init__.py to use
                ;; SOFA_ROOT from the environment (set by Guix
                ;; search paths) instead of computing it relative
                ;; to the script location.
                (call-with-output-file
                    "SlicerSofa/SofaEnvironment/__init__.py"
                  (lambda (port)
                    (display
                     (string-append
                      "import os, sys\n"
                      "sofa_root = os.environ.get('SOFA_ROOT', '')\n"
                      "if sofa_root:\n"
                      "    for p in ['SofaPython3', 'STLIB', 'Cosserat',\n"
                      "              'BeamAdapter', 'Registration']:\n"
                      "        sp = os.path.join(sofa_root, 'plugins', p,\n"
                      "                          'lib', 'python3', 'site-packages')\n"
                      "        if os.path.isdir(sp) and sp not in sys.path:\n"
                      "            sys.path.insert(0, sp)\n"
                      "defaultExecHook = sys.excepthook\n"
                      "import Sofa\n"
                      "import SofaRuntime\n"
                      "sys.excepthook = defaultExecHook\n"
                      "__all__ = ['Sofa', 'SofaRuntime']\n")
                     port))))))))
      ;; UseSlicer.cmake transitively requires all of slicer-5.12's
      ;; build-time libraries at configure time.  Inherit the full
      ;; input set (same pattern as slicer-ros2-module-jazzy).
      (inputs
       (fold (lambda (pkg acc)
               (modify-inputs acc (prepend pkg)))
             (modify-inputs (package-inputs slicer-5.12)
               (prepend slicer-5.12))
             (list sofa-framework
                   python)))
      (propagated-inputs
       (list slicer-5.12
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
