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

(define-module (systole)
  #:use-module (ice-9 regex)
  #:export (%systole-root %systole-version %systole-codename))

;; Determine the repository root directory robustly.
;;
;; Priority:
;;   1. SYSTOLE_ROOT environment variable (for testing/custom setups)
;;   2. Module-relative: derive from THIS file's `current-filename'.  These
;;      OS/installer modules live under <root>/system/, so the root is one
;;      `dirname' above the directory holding this file.  This is correct
;;      regardless of the caller's `getcwd' -- unlike the old upward
;;      .guix-channel search, which depended on the working directory.
;;   3. Search upward from getcwd() for .guix-channel file (legacy fallback)
;;   4. getcwd() (last-resort fallback)
;;
;; Note: these `system/' modules are NOT shipped as part of the
;; `guix-systole' channel (the channel ships only the `systole/' directory);
;; they are loaded with `-L system' from a checkout when building the
;; installer ISO, so the repo-root `assets/' they reference is always present.
(define %systole-root
  (let ((env-root (getenv "SYSTOLE_ROOT"))
        (self (current-filename)))
    (cond
     ;; Environment variable explicitly set
     (env-root
      (if (string-suffix? "/" env-root)
          env-root
          (string-append env-root "/")))
     ;; Module-relative resolution (cwd-independent)
     (self
      (string-append (dirname (dirname self)) "/"))
     ;; Search for .guix-channel file starting from current directory
     (else
      (let loop ((dir (getcwd)))
        (cond
         ;; Found .guix-channel file - this is the root
         ((file-exists? (string-append dir "/.guix-channel"))
          (string-append dir "/"))
         ;; Reached filesystem root
         ((string=? dir "/")
          (string-append (getcwd) "/"))
         ;; Try parent directory
         (else
          (loop (dirname dir)))))))))

(define %systole-version "1.0")
(define %systole-codename "magnet")
