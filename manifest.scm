;;; GNU Systole
;;; Copyright © 2026 Rafael Palomar <rafael.palomar@ous-research.no>
;;;
;;; This file is part of GNU Systole.
;;;
;;; GNU Systole is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Systole is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Systole.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Manifest of every public package in the guix-systole channel,
;;; discovered from the (systole packages ...) modules.  One target for
;;; humans, CI, Cuirass jobsets and substitute-coverage checks alike:
;;;
;;;   guix build -m manifest.scm                 # build everything
;;;   guix weather -m manifest.scm \
;;;     --substitute-urls=https://...            # substitute coverage
;;;
;;; Run it from a checkout; module discovery is relative to this file.
;;;
;;; Code:

(use-modules (gnu packages)
             (guix discovery)
             (guix packages)
             (guix profiles)
             (srfi srfi-1))

(define %channel-root
  ;; Directory holding this manifest; the channel's load-path root is
  ;; the systole/ subdirectory.  current-filename can be #f depending
  ;; on how the file is loaded (cf. the grub-themes incident), so fall
  ;; back to the working directory.
  (let ((here (or (current-filename)
                  (string-append (getcwd) "/manifest.scm"))))
    (string-append (dirname here) "/systole")))

(unless (member %channel-root %load-path)
  (add-to-load-path %channel-root))

(define %package-modules
  (scheme-modules %channel-root "systole/packages"))

(define %packages
  ;; fold-packages' default #:select? excludes hidden and superseded
  ;; packages, which is exactly what we want to publish.
  (delete-duplicates
   (fold-packages cons '() %package-modules)
   eq?))

(format (current-error-port)
        "guix-systole manifest: ~a packages from ~a modules~%"
        (length %packages)
        (length %package-modules))

(packages->manifest %packages)
