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
;;; Non-free packages for the 3D Systems Touch (formerly Sensable Phantom
;;; Omni) haptic device.  Two tarballs are pulled from 3D Systems' public
;;; S3 bucket:
;;;
;;;   * openhaptics-sdk  — OpenHaptics 3.4 Developer Edition SDK
;;;                        (libHD, libHL, libQH, headers, .a files).
;;;   * touch-driver     — the Touch 2022 USB user-space driver providing
;;;                        libPhantomIOLib42.so (required at runtime by
;;;                        libHD) and the Touch_Diagnostic / Touch_Setup
;;;                        utilities + udev rules.
;;;
;;; Both are distributed under a proprietary EULA from 3D Systems; they
;;; are freely downloadable (no login) but not redistributable under a
;;; free license.  They exist here so the JHU cisst sawSensablePhantom
;;; stack — and through it Laura Connolly's SlicerROS2 Touch demo — can
;;; be built in a hermetic Guix environment.
;;;
;;; Code:

(define-module (systole packages openhaptics)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages base)          ; tar
  #:use-module (gnu packages compression)   ; gzip
  #:use-module (gnu packages elf)           ; patchelf
  #:use-module (gnu packages gcc)           ; gcc:lib
  #:use-module (nongnu packages ncurses)    ; ncurses-5 (nonguix channel)
  #:use-module ((systole licenses) #:prefix license:))

;;;
;;; 3D Systems Touch USB driver 2022-04-04 (proprietary).
;;; Defined first so openhaptics-sdk can propagate it.
;;;

(define-public touch-driver
  (package
    (name "touch-driver")
    (version "2022-04-04")
    (source
     (origin
       (method url-fetch)
       (uri "https://s3.amazonaws.com/dl.3dsystems.com/binaries/Sensable/Linux/TouchDriver2022_04_04.tgz")
       (sha256
        (base32 "0qygavn85mvaaawsy9swqn6y2snxdnl4bz4nam8nydzykz7qsxkv"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((out     #$output)
                 (tar     (string-append #$(this-package-native-input "tar")
                                         "/bin/tar"))
                 (gzip    (string-append #$(this-package-native-input "gzip")
                                         "/bin"))
                 (payload "TouchDriver2022_04_04"))
            (setenv "PATH" (string-append gzip ":" (getenv "PATH")))
            (invoke tar "xf" #$source)
            (mkdir-p (string-append out "/lib"))
            (mkdir-p (string-append out "/bin"))
            (mkdir-p (string-append out "/lib/udev/rules.d"))
            (install-file (string-append payload "/usr/lib/libPhantomIOLib42.so")
                          (string-append out "/lib"))
            (for-each
             (lambda (f)
               (let ((src (string-append payload "/" f)))
                 (when (file-exists? src)
                   (install-file src (string-append out "/bin")))))
             '("bin/Touch_Diagnostic"
               "bin/Touch_Setup"
               "ListUSBHapticDevices"))
            (for-each
             (lambda (rule)
               (let ((src (string-append payload "/" rule)))
                 (when (file-exists? src)
                   (install-file src (string-append out "/lib/udev/rules.d")))))
             '("lib/udev/rules.d/91-3dsystems-hid.rules"
               "lib/udev/rules.d/91-3dsystems-touch.rules"))
            ;; Patchelf the pre-built binaries so they use the Guix
            ;; dynamic linker + find their Qt5 / PhantomIO deps.
            (let* ((patchelf (string-append
                              #$(this-package-native-input "patchelf")
                              "/bin/patchelf"))
                   (ld-linux (string-append
                              #$(this-package-input "glibc")
                              "/lib/ld-linux-x86-64.so.2"))
                   (gcc-lib-path
                    ;; gcc "lib" output lives at a separate store path
                    ;; (-gcc-14.x-lib).  Resolve via %build-inputs.
                    (or (assoc-ref %build-inputs "gcc:lib")
                        (error "gcc:lib input not found")))
                   (rpath (string-append
                           out "/lib:"
                           gcc-lib-path "/lib:"
                           #$(this-package-input "glibc") "/lib")))
              (for-each
               (lambda (bin)
                 (let ((f (string-append out "/bin/" bin)))
                   (when (file-exists? f)
                     (invoke patchelf "--set-interpreter" ld-linux f)
                     (invoke patchelf "--set-rpath" rpath f))))
               '("Touch_Diagnostic" "Touch_Setup")))))))
    (native-inputs
     (list (@ (gnu packages base) tar)
           (@ (gnu packages compression) gzip)
           (@ (gnu packages elf) patchelf)))
    (inputs
     `(("glibc" ,(@ (gnu packages base) glibc))
       ("gcc:lib" ,(@ (gnu packages gcc) gcc) "lib")))
    (supported-systems '("x86_64-linux"))
    (home-page "https://www.3dsystems.com/haptics-devices/touch")
    (synopsis "3D Systems Touch USB user-space driver (proprietary)")
    (description
     "User-space USB driver for the 3D Systems Touch haptic device
(formerly the Sensable Phantom Omni).  Provides @code{libPhantomIOLib42}
— the runtime library that @code{libHD} from OpenHaptics dlopen()'s to
talk to the device over USB — plus the @code{Touch_Diagnostic} and
@code{Touch_Setup} calibration utilities and the @file{91-3dsystems-*}
udev rules that let non-root users open the device.

This package is non-free and is distributed directly by 3D Systems
from their public S3 bucket under a proprietary EULA.")
    (license license:3ds-touch-driver-eula)))

;;;
;;; OpenHaptics 3.4 Developer Edition SDK (proprietary).
;;;

(define-public openhaptics-sdk
  (package
    (name "openhaptics-sdk")
    (version "3.4-0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://s3.amazonaws.com/dl.3dsystems.com/binaries/"
             "support/downloads/KB+Files/Open+Haptics/"
             "openhaptics_" version "-developer-edition-amd64.tar.gz"))
       (sha256
        (base32 "07r7f9pgvqmzljwq32yp14apjkpy3p2a2wn6bsic1n54lhwy0s4h"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((out     #$output)
                 (tar     (string-append #$(this-package-native-input "tar")
                                         "/bin/tar"))
                 (gzip    (string-append #$(this-package-native-input "gzip")
                                         "/bin"))
                 (patchelf (string-append
                            #$(this-package-native-input "patchelf")
                            "/bin/patchelf"))
                 (payload "openhaptics_3.4-0-developer-edition-amd64"))
            (setenv "PATH" (string-append gzip ":" (getenv "PATH")))
            (invoke tar "xf" #$source)
            ;; SDK layout after extraction:
            ;;   <payload>/usr/include/{HD,HDU,HL,HLU,QH,SnapConstraints}
            ;;   <payload>/usr/lib/lib{HD,HL,QH,QHGLUTWrapper}.so.3.4.0
            ;;   <payload>/usr/lib/lib{HDU,HLU,SnapConstraints}.a
            ;;   <payload>/opt/OpenHaptics/Developer/3.4-0/{doc,examples}
            (mkdir-p (string-append out "/include"))
            (mkdir-p (string-append out "/lib"))
            (mkdir-p (string-append out "/share/doc/openhaptics-3.4"))
            (for-each
             (lambda (d)
               (copy-recursively
                (string-append payload "/usr/include/" d)
                (string-append out "/include/" d)))
             '("HD" "HDU" "HL" "HLU" "QH" "SnapConstraints"))
            (for-each
             (lambda (f)
               (install-file (string-append payload "/usr/lib/" f)
                             (string-append out "/lib")))
             '("libHD.so.3.4.0"
               "libHL.so.3.4.0"
               "libQH.so.3.4.0"
               "libQHGLUTWrapper.so.3.4.0"
               "libHDU.a"
               "libHLU.a"
               "libSnapConstraints.a"))
            ;; Create the .so and .so.3.4 symlinks libHL expects.
            (with-directory-excursion (string-append out "/lib")
              (for-each
               (lambda (base)
                 (symlink (string-append base ".so.3.4.0")
                          (string-append base ".so.3.4"))
                 (symlink (string-append base ".so.3.4.0")
                          (string-append base ".so")))
               '("libHD" "libHL" "libQH" "libQHGLUTWrapper")))
            ;; Patch RUNPATH on the .so files so libHD finds its
            ;; dependencies (libPhantomIOLib42 from touch-driver,
            ;; libncurses.so.5/libtinfo.so.5 from ncurses-5, and the
            ;; usual libstdc++/libgcc_s from gcc:lib) without the
            ;; user having to set LD_LIBRARY_PATH manually.  The
            ;; touch-driver path is injected as a propagated input,
            ;; so its $out/lib must appear in RUNPATH too — we add
            ;; it as a relative $ORIGIN fallback and rely on
            ;; LD_LIBRARY_PATH plumbing via native-search-paths.
            (let ((rpath
                   (string-append
                    "$ORIGIN:"
                    #$(this-package-input "ncurses") "/lib:"
                    #$(this-package-input "gcc:lib") "/lib")))
              (for-each
               (lambda (f)
                 (invoke patchelf "--set-rpath" rpath
                         (string-append out "/lib/" f)))
               '("libHD.so.3.4.0"
                 "libHL.so.3.4.0"
                 "libQH.so.3.4.0"
                 "libQHGLUTWrapper.so.3.4.0")))
            ;; Copy examples and docs (small).
            (copy-recursively
             (string-append payload "/opt/OpenHaptics/Developer/3.4-0/doc")
             (string-append out "/share/doc/openhaptics-3.4/doc"))
            (copy-recursively
             (string-append payload
                            "/opt/OpenHaptics/Developer/3.4-0/examples")
             (string-append out "/share/doc/openhaptics-3.4/examples"))
            ;; Symbolic Developer tree some build systems look for via
            ;; $OH_SDK_BASE; mirror the canonical /opt/OpenHaptics layout.
            (mkdir-p (string-append out "/opt/OpenHaptics/Developer/3.4-0"))
            (symlink (string-append out "/include")
                     (string-append out
                                    "/opt/OpenHaptics/Developer/3.4-0/include"))
            (symlink (string-append out "/lib")
                     (string-append out
                                    "/opt/OpenHaptics/Developer/3.4-0/lib"))))))
    (native-inputs
     (list (@ (gnu packages base) tar)
           (@ (gnu packages compression) gzip)
           (@ (gnu packages elf) patchelf)))
    (inputs
     ;; libHD needs libncurses.so.5 + libtinfo.so.5 (legacy ABI).
     ;; Must use ncurses/tinfo-5 (not ncurses-5) so libtinfo.so.5 is
     ;; split out as a separate shared library — libHD DT_NEEDs it
     ;; independently.  gcc:lib gives libstdc++/libgcc_s.
     `(("ncurses" ,(@ (nongnu packages ncurses) ncurses/tinfo-5))
       ("gcc:lib" ,(@ (gnu packages gcc) gcc) "lib")))
    ;; libHD DT_NEEDED's libPhantomIOLib42.so which lives in
    ;; touch-driver; propagate so anything that installs
    ;; openhaptics-sdk also gets the driver on LD_LIBRARY_PATH.
    (propagated-inputs (list touch-driver))
    (supported-systems '("x86_64-linux"))
    (home-page "https://www.3dsystems.com/haptics-devices/openhaptics")
    (synopsis "OpenHaptics 3.4 Developer Edition SDK (proprietary)")
    (description
     "OpenHaptics is the C/C++ SDK from 3D Systems used to program the
Touch (Sensable Phantom Omni) family of haptic devices.  This package
unpacks the Linux Developer Edition 3.4 tarball from 3D Systems' public
S3 bucket and installs @file{libHD}, @file{libHL}, @file{libQH} and the
matching @file{HD}, @file{HDU}, @file{HL}, @file{HLU} headers into the
store under a standard @file{include}/@file{lib} layout.

This package is non-free.  At runtime @code{libHD.so} dlopen()'s
@file{libPhantomIOLib42.so} from the companion @code{touch-driver}
package, which must be installed and a Touch device physically present
before @code{hdInitDevice} will succeed.")
    (license license:openhaptics-eula)))
