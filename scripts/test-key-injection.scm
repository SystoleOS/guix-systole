#!/usr/bin/env guile
!#
;;; Test script for SSH deploy key injection
;;; Tests the injection logic without building full installer

(use-modules (ice-9 textual-ports)
             (ice-9 format)
             (srfi srfi-13))

;; Sample config that mimics what the Guix installer generates
(define sample-config
  "(operating-system
  (host-name \"guix-system\")
  (timezone \"UTC\")
  (locale \"en_US.utf8\")

  (bootloader
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets '(\"/boot/efi\"))))

  (file-systems
   (cons* (file-system
           (device (file-system-label \"root\"))
           (mount-point \"/\")
           (type \"ext4\"))
          %base-file-systems))

  (users
   (cons* (user-account
           (name \"user\")
           (group \"users\"))
          %base-user-accounts))

  (packages
   (cons screen %base-packages))

  (services
   (cons* (service dhcp-client-service-type)
          %base-services)))")

;; Test deploy key
(define test-deploy-key
  "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBtESyRjo6GOaeM0YjbtOQ8L3qZpQ8eFple/nt0tkfRd test@example")

;; SSH service config to inject
(define (make-ssh-service-config key)
  (string-append
   "          (service openssh-service-type\n"
   "                   (openssh-configuration\n"
   "                    (permit-root-login 'prohibit-password)\n"
   "                    (password-authentication? #f)\n"
   "                    (authorized-keys\n"
   "                     `((\"root\" ,(plain-file \"deploy-key.pub\"\n"
   "                                           \"" key "\"))))))\n"))

;; Injection logic - try multiple patterns
(define (inject-key config key)
  (let ((ssh-config (make-ssh-service-config key))
        (patterns '("%base-services)))"
                   "%base-services))"
                   "%base-services)"
                   "%base-services")))
    (let loop ((pats patterns))
      (if (null? pats)
          (begin
            (format #t "❌ No matching pattern found!~%")
            (format #t "Config services section:~%")
            (display (substring config
                               (max 0 (- (string-length config) 200))
                               (string-length config)))
            (newline)
            #f)
          (let ((pattern (car pats)))
            (format #t "Trying pattern: ~s ... " pattern)
            (if (string-contains config pattern)
                (let* ((pos (string-contains config pattern))
                       (before (substring config 0 pos))
                       (after (substring config pos)))
                  (format #t "✓ MATCH at position ~a~%" pos)
                  (string-append before ssh-config after))
                (begin
                  (format #t "✗ no match~%")
                  (loop (cdr pats)))))))))

;; Main test
(format #t "~%=== Testing SSH Deploy Key Injection ===~%~%")

(format #t "Sample config length: ~a chars~%" (string-length sample-config))
(format #t "Deploy key: ~a~%~%" test-deploy-key)

(let ((result (inject-key sample-config test-deploy-key)))
  (if result
      (begin
        (format #t "~%✅ SUCCESS! Modified config:~%~%")
        (display result)
        (newline)

        ;; Verify it has SSH config
        (format #t "~%=== Verification ===~%")
        (format #t "Contains openssh-service-type: ~a~%"
                (if (string-contains result "openssh-service-type") "✓ YES" "✗ NO"))
        (format #t "Contains deploy key: ~a~%"
                (if (string-contains result test-deploy-key) "✓ YES" "✗ NO"))
        (format #t "Contains authorized-keys: ~a~%"
                (if (string-contains result "authorized-keys") "✓ YES" "✗ NO")))
      (begin
        (format #t "~%❌ FAILED to inject key~%")
        (exit 1))))
