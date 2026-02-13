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
             (systole transformations)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(use-service-modules cups desktop networking ssh xorg)
(use-package-modules bootloaders certs ssh)

(define %deploy-key
  (plain-file "deploy-key"
              (call-with-input-file "test-key.pub"
                get-string-all)))

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
                                            (openssh-configuration (permit-root-login 'without-password)
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
      ))
   (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (targets (list "/dev/vda"))
                 (keyboard-layout keyboard-layout)))
   (swap-devices (list (swap-space
                         (target (uuid
                                  "e6564580-2a72-4d8b-83f5-ba14b2ac4069")))))

   ;; The list of file systems that get "mounted".  The unique
   ;; file system identifiers there ("UUIDs") can be obtained
   ;; by running 'blkid' in a terminal.
   (file-systems (cons* (file-system
                          (mount-point "/")
                          (device (uuid
                                   "e11b28fd-48c7-4354-af70-5a4d257832f2"
                                   'ext4))
                          (type "ext4")) %base-file-systems))))


  )

;; Apply Systole transformations
(define transformed-os
  ((compose (systole-transformation-guix #:guix-source? #t)
            (systole-transformation-linux #:initrd base-initrd))
   base-os))

;; Deployment machine list
(list (machine
       (operating-system transformed-os)
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "localhost")
                       (system "x86_64-linux")
                       (port 2223)
                       (user "root")
                       (identity "./test-key")))))
