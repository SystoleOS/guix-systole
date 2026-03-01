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

(define-module (systole packages slicer-applications)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages imagemagick)
  #:use-module (systole packages slicer)
  #:use-module (systole packages slicer-igt)
  #:export (make-slicer-application))

;;;
;;; make-slicer-application — guix-native Slicer application profile factory
;;;

(define (module-transitive-packages modules)
  "Return all packages reachable via propagated-inputs from MODULES (BFS)."
  (let loop ((frontier modules) (visited '()))
    (if (null? frontier)
        (reverse visited)
        (let* ((pkg      (car frontier))
               (rest     (cdr frontier))
               (props    (package-propagated-inputs pkg))
               ;; Handle both new-style (pkg ...) and old-style ((label pkg) ...)
               (prop-pkgs
                (filter-map (lambda (p)
                              (cond ((package? p) p)
                                    ((and (pair? p) (package? (cadr p))) (cadr p))
                                    (else #f)))
                            props))
               ;; Filter against both visited and the remaining frontier so no
               ;; package is enqueued twice (standard BFS deduplication).
               (new-pkgs (filter (lambda (p)
                                   (and (not (memq p visited))
                                        (not (memq p rest))))
                                 prop-pkgs)))
          (loop (append rest new-pkgs) (cons pkg visited))))))

;;; A "slicer application" is a Guix meta-package that:
;;;   1. Propagates a curated set of Slicer modules into the user's profile
;;;   2. Installs a bin/<name> wrapper script that launches Slicer with
;;;      application-specific environment (SLICER_INIT_DIR) and forwards all args
;;;   3. Optionally installs theming assets (splash.png, style.qss, init.py)
;;;      under share/slicer-applications/<name>/ so SLICER_INIT_DIR picks them
;;;      up at startup
;;;
;;; No Slicer source is recompiled — this is pure profile composition.
;;;

(define* (make-slicer-application
          #:key
          name
          version
          synopsis
          description
          (modules      '())
          (extra-inputs '())
          (splash       #f)
          (stylesheet   #f)
          (init-script  #f))
  "Return a Guix package that installs MODULES and EXTRA-INPUTS into the
profile, installs a bin/NAME wrapper script that launches Slicer with the
application environment, and when any of SPLASH (file-like → splash.png),
STYLESHEET (file-like → style.qss), or INIT-SCRIPT (file-like → init.py)
are provided, copies them to share/slicer-applications/NAME/ and sets
SLICER_INIT_DIR in the wrapper (overridable at runtime via the environment)."
  (let* ((has-theme? (or splash stylesheet init-script))
         ;; Gexp fragment: install theme assets when present.
         ;; Theme assets land in share/slicer-applications/<name>/ so multiple
         ;; applications can coexist in the same Guix profile without their
         ;; directories colliding.
         (theme-dir-suffix (string-append "/share/slicer-applications/" name))
         (theme-installer
          (if has-theme?
              #~(begin
                  (mkdir (string-append #$output "/share"))
                  (mkdir (string-append #$output "/share/slicer-applications"))
                  (mkdir (string-append #$output #$theme-dir-suffix))
                  #$(if splash
                        #~(copy-file #$splash
                                     (string-append #$output #$theme-dir-suffix "/splash.png"))
                        #~(values))
                  #$(if stylesheet
                        #~(copy-file #$stylesheet
                                     (string-append #$output #$theme-dir-suffix "/style.qss"))
                        #~(values))
                  #$(if init-script
                        #~(copy-file #$init-script
                                     (string-append #$output #$theme-dir-suffix "/init.py"))
                        #~(values)))
              #~(values)))
         ;; Candidate store paths for the application's module set (computed at
         ;; expansion time).  Uses file-append so each item becomes a gexp
         ;; resolving to the store path string.  Both qt-loadable-modules and
         ;; qt-scripted-modules subdirs are included; non-existent ones are
         ;; filtered out at build time.
         (module-path-candidates
          (append-map (lambda (pkg)
                        (list (file-append pkg "/lib/Slicer-5.8/qt-loadable-modules")
                              (file-append pkg "/lib/Slicer-5.8/qt-scripted-modules")))
                      (module-transitive-packages modules)))
         ;; Gexp fragment: unconditionally override SLICER_ADDITIONAL_MODULE_PATHS
         ;; with only the store paths of this application's declared module set and
         ;; their transitive propagated deps.  Filters at build time so meta-packages
         ;; that lack a module directory are silently skipped.
         (module-paths-line
          (if (null? modules)
              #~(values)
              #~(let* ((paths    (filter file-exists?
                                        (list #$@module-path-candidates)))
                       (path-str (string-join paths ":")))
                  ;; Override SLICER_ADDITIONAL_MODULE_PATHS so Slicer scans
                  ;; only this application's curated module set.
                  (format port "SLICER_ADDITIONAL_MODULE_PATHS=~a~%\
export SLICER_ADDITIONAL_MODULE_PATHS~%" path-str)
                  ;; Also prepend all module dirs to LD_LIBRARY_PATH before
                  ;; exec so the kernel/dynamic linker sees them at process
                  ;; start.  patch-0046 calls setenv() at C++ init time, but
                  ;; glibc's ld.so caches LD_LIBRARY_PATH on process entry and
                  ;; does not re-read it from environ on dlopen — so the
                  ;; setenv() has no effect on module loading.
                  (format port "LD_LIBRARY_PATH=~a${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}~%\
export LD_LIBRARY_PATH~%" path-str)
                  ;; Prepend all module dirs to PYTHONPATH as well.
                  ;; SLICER_PYTHONPATH (assembled by Guix native-search-path)
                  ;; only scans lib/Slicer-5.8 and bin/Python — it misses
                  ;; Python packages installed under qt-scripted-modules/ (e.g.
                  ;; SubjectHierarchyLib).  Setting PYTHONPATH here ensures
                  ;; those packages are importable before Slicer's module
                  ;; factory has added the dirs to sys.path.
                  (format port "PYTHONPATH=~a${PYTHONPATH:+:${PYTHONPATH}}~%\
export PYTHONPATH~%" path-str))))
         ;; Gexp fragment: write SLICER_INIT_DIR line into the wrapper when
         ;; theme assets are present.  Uses ${VAR:=default} so the user can
         ;; override it at runtime by setting SLICER_INIT_DIR themselves.
         (init-dir-line
          (if has-theme?
              #~(format port ": \"${SLICER_INIT_DIR:=~a~a}\"~%\
export SLICER_INIT_DIR~%"
                        #$output #$theme-dir-suffix)
              #~(values))))
    (package
      (name name)
      (version version)
      (source #f)
      (build-system trivial-build-system)
      (arguments
       (list
        #:builder
        #~(begin
            (mkdir #$output)
            (mkdir (string-append #$output "/bin"))
            ;; Install theme assets (when provided).
            #$theme-installer
            ;; Write the launcher wrapper script.
            (let ((script (string-append #$output "/bin/" #$name)))
              (call-with-output-file script
                (lambda (port)
                  (display "#!/bin/sh\n" port)
                  (format port "# ~a: SystoleOS Slicer application launcher~%" #$name)
                  #$module-paths-line
                  #$init-dir-line
                  (display "exec Slicer \"$@\"\n" port)))
              (chmod script #o755)))))
      (propagated-inputs
       (append (list slicer-5.8) modules extra-inputs))
      (synopsis synopsis)
      (description description)
      (home-page (package-home-page slicer-5.8))
      (license (package-license slicer-5.8)))))


;;;
;;; Example Slicer application — demonstrates the factory and serves as an
;;; integration test
;;;

;; Splash screen generated at build time by ImageMagick.
;; 620×240 px — the standard Slicer splash dimensions.
(define %example-application-splash
  (computed-file
   "slicer-example-application-splash.png"
   #~(let* ((convert #$(file-append imagemagick "/bin/convert"))
            (bold    #$(file-append font-dejavu
                                    "/share/fonts/truetype/DejaVuSans-Bold.ttf"))
            (sans    #$(file-append font-dejavu
                                    "/share/fonts/truetype/DejaVuSans.ttf")))
       (setenv "HOME"   "/tmp")
       (setenv "TMPDIR" "/tmp")
       (let ((status
              (system* convert
                       ;; Canvas: dark gradient background
                       "-size" "620x240"
                       "gradient:#0d1117-#1a2236"
                       ;; Top blue accent bar
                       "-fill" "#1f6feb"
                       "-draw" "rectangle 0,0 619,4"
                       ;; Left blue accent bar
                       "-fill" "#1f6feb"
                       "-draw" "rectangle 0,0 3,239"
                       ;; Decorative concentric circles — right side
                       "-fill" "none" "-stroke" "#1a3a6e" "-strokewidth" "1"
                       "-draw" "circle 530,120 570,120"
                       "-draw" "circle 530,120 590,120"
                       "-draw" "circle 530,120 610,120"
                       ;; "SystoleOS" — main title
                       "-font" bold "-pointsize" "52"
                       "-fill" "#c9d1d9"
                       "-gravity" "NorthWest"
                       "-annotate" "+28+68" "SystoleOS"
                       ;; "Example Application" — subtitle
                       "-font" sans "-pointsize" "24"
                       "-fill" "#1f6feb"
                       "-annotate" "+30+138" "Example Application"
                       ;; Attribution line
                       "-font" sans "-pointsize" "13"
                       "-fill" "#484f58"
                       "-annotate" "+30+190" "Powered by 3D Slicer 5.8"
                       #$output)))
         (unless (zero? status)
           (error "ImageMagick convert failed with status" status))))))

(define-public slicer-example-application
  (make-slicer-application
   #:name        "slicer-example-application"
   #:version     "0.1"
   #:synopsis    "Example SystoleOS Slicer application with clinical dark theme"
   #:description
   "Full-fledged demonstration of the @code{make-slicer-application} factory.
Installs 3D Slicer with the IGT extension and Volumes/Markups modules,
applies a clinical dark-blue Qt stylesheet, and runs an application-specific
Python init script at startup.  Use this as a starting template for real
SystoleOS Slicer application profiles."
   #:modules     (list slicer-igt
                       slicer-volumes-5.8
                       slicer-markups-5.8)
   #:splash      %example-application-splash
   #:stylesheet  (local-file "slicer-applications/example/style.qss")
   #:init-script (local-file "slicer-applications/example/init.py")))
