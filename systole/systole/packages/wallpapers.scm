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

(define-module (systole packages wallpapers)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:export (systole-wallpapers))

(define systole-wallpapers
  (package
    (name "systole-wallpapers")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/systoleos/systole-artwork.git")
             (commit "d8203496e0a54e51ba74824196d9adebaee14197")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1whc64lzyvss8s4vjyspmhzl72v0v1w2q09qx7wkxfvnv6spbxhs"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules `((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((source (assoc-ref %build-inputs "source"))
                 (target (string-append #$output "/share/backgrounds/systole")))
            (mkdir-p target)
            (copy-recursively source target)))))
    (home-page "https://github.com/systoleos/systole-artwork")
    (synopsis "Systole Wallpapers")
    (description "A collection of wallpapers for the Systole project.")
    (license license:cc0)))
