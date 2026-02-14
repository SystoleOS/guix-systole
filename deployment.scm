;; Guix Deployment Configuration for Systole
;; This file defines a deployment target for 'guix deploy'

(use-modules (gnu)
             (gnu machine)
             (gnu machine ssh)
             (gnu services base)
             (gnu services desktop)
             (gnu services networking)
             (gnu services ssh)
             (gnu system)
             (gnu packages nss)
	     (rnrs io ports)
             (guix gexp)
             (guix channels)
             (systole transformations)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(use-service-modules cups desktop networking ssh xorg)
(use-package-modules bootloaders certs ssh)

(define %deploy-key
  (plain-file "deploy-key"
              (call-with-input-file "test-key.pub"
                get-string-all)))

(define %system-channels
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
  )

(define base-os


  ((compose (systole-transformation-guix #:guix-source? #t)
            (systole-transformation-linux #:initrd base-initrd))
   (operating-system
    (locale "en_AG.utf8")
    (timezone "Africa/Abidjan")
    (keyboard-layout (keyboard-layout "al"))
    (host-name "t")

    ;; The list of user accounts ('root' is implicit).
    (users (cons* (user-account
                   (name "t")
                   (comment "T")
                   (group "users")
                   (home-directory "/home/t")
                   (supplementary-groups '("wheel" "netdev" "audio" "video")))
                  %base-user-accounts))

    ;; Below is the list of system services.  To search for available
    ;; services, run 'guix system search KEYWORD' in a terminal.
    (services
     (modify-services (append (list

                               ;; To configure OpenSSH, pass an 'openssh-configuration'
                               ;; record as a second argument to 'service' below.
                               (service openssh-service-type
                                        (openssh-configuration (permit-root-login 'prohibit-password)
                                                               (authorized-keys `
                                                                (("root" ,
                                                                  (plain-file
                                                                   "deploy-key.pub"
                                                                   "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH/vJDKIjDVjtYwQeOSK7mMij9kRyNKNdyM8zfF8/U5t auto-installer-test"))))))
                               (service network-manager-service-type)
                               (service wpa-supplicant-service-type))


                              ;; This is the default list of services we
                              ;; are appending to.
                              %base-services)
                      (guix-service-type config =>
                                         (guix-configuration (inherit config)
                                                             (channels
                                                              %system-channels)
                                                             (authorized-keys (cons (plain-file
                                                                                     "systole-signing-key.pub"
                                                                                     "(public-key
 (ecc
  (curve Ed25519)
  (q #4EB06D3040B7AC87026B998030225A9E14DE383FFAD6FAAA87F0B9267321E7BC#)
  )
 )")
                                                                                    (guix-configuration-authorized-keys
                                                                                     config)))))


                      ))
    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (targets (list "/dev/vda"))
                 (keyboard-layout keyboard-layout)))
    (swap-devices (list (swap-space
(target (uuid
                                  "6bb1c1eb-b7f5-466a-a6fc-d0ffee3f3d25"))
                         )))

    ;; The list of file systems that get "mounted".  The unique
    ;; file system identifiers there ("UUIDs") can be obtained
    ;; by running 'blkid' in a terminal.
    (file-systems (cons* (file-system
                          (mount-point "/")
(device (uuid
                                   "af3423c0-89f9-4ab9-9b9c-f73d248562c6"
                                   'ext4))
                          (type "ext4")) %base-file-systems))))


  )

;; Deployment machine list
;; base-os is already transformed, no need to transform again
(list (machine
       (operating-system base-os)
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "localhost")
                       (system "x86_64-linux")
                       (port 2223)
                       (user "root")
                       (identity "./test-deploy-key")))))
