(list (channel
        (name 'guix)
        (url "https://codeberg.org/guix/guix.git")
        (branch "master")
        (commit
          "46793f96cdfe55b695d996c0ba367e5fd37b81e4")
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
          "48a8706d44040cc7014f36873dbd834c048aadd3")
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
