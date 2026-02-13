;; Locked channel versions for reproducible Systole installer builds
;; Copyright © 2026 Rafael Palomar <rafael.palomar@ous-research.no>
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file pins specific channel commits to ensure:
;; 1. Reproducible installer ISO builds
;; 2. Consistent deployment target states
;; 3. Known-good channel combinations
;;
;; To update these versions:
;; 1. Test your local system with desired channel versions
;; 2. Run: guix describe -f channels > channels-lock.scm
;; 3. Edit to add this header and update commit message
;; 4. Rebuild installer and test deployment
;;
;; Last updated: 2026-02-12
;; Tested with: Guix System, nonguix kernel support, Systole packages

(list (channel
        (name 'guix)
        (url "https://codeberg.org/guix/guix.git")
        (branch "master")
        (commit
          "f75080db69716891c46d5795f5d29ad8cc54700e")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "6c0ea215e0bd089bf3b2097e5c59dd726fbbe304")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
        (name 'guix-xlibre)
        (url "https://codeberg.org/rafaelpalomar/guix-xlibre.git")
        (branch "master")
        (commit
          "02b15dba1951803649c95bdc17feca2b544fc4ee"))
      (channel
        (name 'tailscale)
        (url "https://github.com/umanwizard/guix-tailscale")
        (branch "main")
        (commit
          "58bc8b05520b8565a3230e21388e97f00b886e4b"))
      (channel
        (name 'guix-systole)
        (url "https://github.com/systoleos/guix-systole")
        (branch "main")
        (commit
          "c1bf0d4b739caf5d1b6bc1036c4d87319bff90b5"))
      (channel
        (name 'systole-artwork)
        (url "https://github.com/systoleos/guix-systole-artwork")
        (branch "main")
        (commit
          "26e4f71bf518a03c646d42d7c65ec8529f3c63a6")))
