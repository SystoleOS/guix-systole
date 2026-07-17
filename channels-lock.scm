(list (channel
       (name 'guix)
       (url "https://codeberg.org/guix/guix.git")
       (branch "master")
       (commit "21898c0a1aae913fe732ad81f01328e34acb5721")
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
       (name 'nonguix)
       (url "https://gitlab.com/nonguix/nonguix")
       (branch "master")
       (commit "a8326a5b325400f25c0520c8ef9127fff6d4796d")
       (introduction
        (make-channel-introduction
         "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
         (openpgp-fingerprint
          "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
       (name 'guix-xlibre)
       (url "https://codeberg.org/rafaelpalomar/guix-xlibre.git")
       (branch "master")
       (commit "876828a590c28881c649450588eb2e3eb2bc1f99"))
      (channel
       (name 'tailscale)
       (url "https://codeberg.org/rafaelpalomar/guix-tailscale.git")
       (branch "main")
       (commit "bee82d89aa8b77eb25273398325c8f54e4fcd936"))
      (channel
       (name 'guix-systole)
       (url "https://github.com/systoleos/guix-systole")
       (branch "main")
       (commit "def9078237a144d69fb58278f74b858409bf4108"))
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
       (commit "e93f670a1de32f5e7568ef6e4ad09b82add068bd"))
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
