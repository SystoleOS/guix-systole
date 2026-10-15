(list (channel
       (name 'guix)
       (url "https://codeberg.org/guix/guix.git")
       (branch "master")
       (commit "6d10a594ac75999036747796036cda26e857ea02")
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
       (name 'nonguix)
       (url "https://gitlab.com/nonguix/nonguix")
       (branch "master")
       (commit "accdba77be815bbe00868145dd66d68056eecf7a")
       (introduction
        (make-channel-introduction
         "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
         (openpgp-fingerprint
          "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
       (name 'guix-xlibre)
       (url "https://codeberg.org/rafaelpalomar/guix-xlibre.git")
       (branch "master")
       (commit "3ce2f91f12b21dc5ac15ad4ad11eb5807856c266"))
      (channel
       (name 'tailscale)
       ;; Converged back to upstream 2026-07-24: 1.96.4 builds with the
       ;; pinned guix, making the fork's go-1.24 bump obsolete.
       (url "https://github.com/umanwizard/guix-tailscale")
       (branch "main")
       (commit "4fd673d378f1e82ded518c93624257eaefa5abcc"))
      (channel
       (name 'guix-systole)
       (url "https://github.com/systoleos/guix-systole")
       (branch "main")
       ;; Must postdate the introduction commit for authentication.
       (commit "5e5b6d381afb4cb1c9866a5df3cdb10e3c578574")
       (introduction
        (make-channel-introduction
         "0898d41446ecbd6eddcbf5d78beae63f7e8a4069"
         (openpgp-fingerprint
          "53A2 D043 E0DC 9249 DF75  ABB6 65D8 96E0 0C10 1DDF"))))
      (channel
       (name 'systole-artwork)
       (url "https://github.com/systoleos/guix-systole-artwork")
       (branch "main")
       (commit "26e4f71bf518a03c646d42d7c65ec8529f3c63a6"))
      (channel
       (name 'guix-hermes)
       (url "https://github.com/OUH-MESHLab/guix-hermes")
       (branch "main")
       ;; Not 9116b6c: that commit references python-pygments through a
       ;; module path that has since moved in upstream Guix (see
       ;; ivs-infrastructure channels-lock notes).
       (commit "082bed56557c9e3e07245762c25e3eaa0d7f0066"))
      (channel
       (name 'sops-guix)
       (url "https://github.com/fishinthecalculator/sops-guix.git")
       (branch "main")
       (commit "c53e27e533836ea8595626ba6796dee5362f8c4a")
       (introduction
        (make-channel-introduction
         "0bbaf1fdd25266c7df790f65640aaa01e6d2dbc9"
         (openpgp-fingerprint
          "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2")))))
