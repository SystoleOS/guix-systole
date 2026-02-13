 (use-modules (gnu) (guix))
  (use-service-modules networking ssh)

  (list
   (machine
    (operating-system
     (operating-system
      (host-name "systole-test")
      (timezone "UTC")
      (locale "en_US.utf8")
      (keyboard-layout (keyboard-layout "us"))

      (bootloader
       (bootloader-configuration
        (bootloader grub-efi-bootloader)
        (targets '("/boot/efi"))))

      (file-systems
       (cons* (file-system
               (device (file-system-label "systole-root"))
               (mount-point "/")
               (type "ext4"))
              (file-system
               (device (file-system-label "EFI"))
               (mount-point "/boot/efi")
               (type "vfat"))
              %base-file-systems))

      (users
       (cons* (user-account
               (name "systole")
               (comment "Systole User")
               (group "users")
               (supplementary-groups '("wheel" "netdev")))
              %base-user-accounts))

      (packages (cons screen %base-packages))

      (services
       (cons* (service openssh-service-type
                       (openssh-configuration
                        (permit-root-login 'prohibit-password)
                        (password-authentication? #f)))
              (service dhcpcd-service-type)
              %base-services))))

    (environment managed-host-environment-type)
    (configuration (machine-ssh-configuration
                    (host-name "localhost")
                    (port 2223)
                    (user "root")
                    (identity "/tmp/test-key")))))
