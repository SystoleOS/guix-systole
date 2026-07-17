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

(define-module (systole packages slicer))

;;; Commentary:
;;;
;;; Facade module for the Slicer package stack.  The actual definitions
;;; live in per-concern modules:
;;;
;;;   (systole packages slicer-factory)  shared build infrastructure:
;;;       slicerexecutionmodel and the loadable/scripted/CLI module
;;;       factories, parameterized on the base Slicer package.
;;;   (systole packages slicer-5-8)      everything Slicer 5.8.
;;;   (systole packages slicer-5-10)     everything Slicer 5.10.
;;;   (systole packages slicer-5-12)     everything Slicer 5.12, plus the
;;;       slicer-next preview package tracking upstream main.
;;;
;;; The per-version module names use "-5-8"/"-5-10" rather than
;;; "-5.8"/"-5.10": Guile's load-path search treats a dot in the last
;;; module-name component as a file extension and refuses to append
;;; ".scm", so dotted module names can never be autoloaded.
;;;
;;; This module re-exports every public binding from all of these so that
;;; existing users -- `guix install -L … slicer-5.8`, other channel
;;; modules with #:use-module (systole packages slicer), and
;;; `(@ (systole packages slicer) …)` references -- keep working
;;; unchanged.  Adding a public binding to any of the three modules
;;; automatically re-exports it here.
;;;
;;; Code:

(eval-when (expand load eval)
  (let ((interface (module-public-interface (current-module))))
    (for-each (lambda (name)
                (module-use! interface (resolve-interface name)))
              '((systole packages slicer-factory)
                (systole packages slicer-5-8)
                (systole packages slicer-5-10)
                (systole packages slicer-5-12)))))
