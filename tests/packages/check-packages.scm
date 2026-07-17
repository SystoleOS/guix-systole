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
;;; Channel sanity check, run in script mode so that failures propagate
;;; as a non-zero exit status (heredoc REPL sessions swallow errors):
;;;
;;;   guix repl -L systole -- tests/packages/check-packages.scm
;;;
;;; Three passes:
;;;   1. Discover every module under systole/ and load it.
;;;   2. Walk each module's public interface and touch every exported
;;;      package (name + version) so malformed definitions surface.
;;;   3. Verify a curated list of load-bearing public names still
;;;      exists -- the channel's API contract with its users.
;;;
;;; Code:

(use-modules (guix packages)
             (ice-9 ftw)
             (ice-9 format)
             (srfi srfi-1))

(define failures 0)

(define (fail! fmt . args)
  (set! failures (+ failures 1))
  (format #t "  ✗ ~a~%" (apply format #f fmt args)))

(define script-dir
  (dirname (canonicalize-path (car (command-line)))))

(define channel-root
  ;; tests/packages/ -> repo root -> systole/ (the channel load-path root).
  (canonicalize-path (string-append script-dir "/../../systole")))

(define (file->module-name relative)
  ;; "systole/packages/vtk.scm" -> (systole packages vtk)
  (map string->symbol
       (string-split (string-drop-right relative 4) #\/)))

(define modules
  (let ((names '()))
    (ftw channel-root
         (lambda (filename statinfo flag)
           (when (and (eq? flag 'regular)
                      (string-suffix? ".scm" filename))
             (set! names
                   (cons (file->module-name
                          (string-drop filename
                                       (+ 1 (string-length channel-root))))
                         names)))
           #t))
    (sort names (lambda (a b)
                  (string<? (object->string a) (object->string b))))))

(format #t "Loading ~a modules from ~a...~%" (length modules) channel-root)

(define interfaces
  (filter-map
   (lambda (mod)
     (catch #t
       (lambda ()
         (let ((iface (resolve-interface mod)))
           (format #t "  ✓ ~s~%" mod)
           (cons mod iface)))
       (lambda (key . args)
         (fail! "module ~s failed to load: ~s ~s" mod key args)
         #f)))
   modules))

(format #t "~%Checking exported packages...~%")

(define package-count 0)

(for-each
 (lambda (mod+iface)
   (module-for-each
    (lambda (sym var)
      (let ((val (and (variable-bound? var) (variable-ref var))))
        (when (package? val)
          (set! package-count (+ package-count 1))
          (catch #t
            (lambda ()
              (package-name val)
              (package-version val))
            (lambda (key . args)
              (fail! "package ~a in ~s is malformed: ~s ~s"
                     sym (car mod+iface) key args))))))
    (cdr mod+iface)))
 interfaces)

(format #t "  ~a exported packages OK~%" package-count)

(format #t "~%Checking API contract (load-bearing public names)...~%")

;; Renaming or unexporting any of these breaks channel users; the
;; discovery sweep above cannot catch that, so pin them explicitly.
(define %required-exports
  '(((systole packages slicer)
     slicer-5.8 slicer-all-5.8 slicer-5.10 slicer-all-5.10
     slicer-5.12 slicer-all-5.12 slicer-next)
    ((systole packages vtk) vtk-slicer vtkaddon)
    ((systole packages itk) itk-slicer)
    ((systole packages ctk) ctk ctkapplauncher)
    ((systole packages teem) teem-slicer)
    ((systole packages maths) netcdf-slicer)
    ((systole packages libarchive) libarchive-slicer)
    ((systole packages qrestapi) qrestapi)
    ((systole packages openigtlink) openigtlink slicer-openigtlink openigtlinkio)
    ((systole packages igsio) igsio slicer-igsio-common)
    ((systole packages plustoolkit) pluslib plusapp)
    ((systole packages slicer-igt) slicer-igt)
    ((systole packages sofa) sofa-framework)
    ((systole packages grub-themes) systole-grub-theme)
    ((systole packages ros2-helpers)
     python-catkin-pkg python-osrf-pycommon eclipse-cyclonedds
     console-bridge urdfdom-headers urdfdom)
    ((systole packages ros2 jazzy) ros-jazzy)))

(for-each
 (lambda (spec)
   (let* ((mod (car spec))
          (syms (cdr spec))
          (entry (assoc mod interfaces)))
     (if entry
         (for-each
          (lambda (sym)
            (if (module-variable (cdr entry) sym)
                (format #t "  ✓ ~a (~s)~%" sym mod)
                (fail! "~a is no longer exported from ~s" sym mod)))
          syms)
         (fail! "required module ~s did not load" mod))))
 %required-exports)

(format #t "~%~a~%"
        (if (zero? failures)
            "All checks passed."
            (format #f "~a check(s) FAILED." failures)))

(exit (if (zero? failures) 0 1))
