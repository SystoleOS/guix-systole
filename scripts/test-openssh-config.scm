#!/usr/bin/env guile
!#

(use-modules (gnu services ssh)
             (ice-9 pretty-print))

(format #t "Testing openssh-configuration creation...~%~%")

;; Test 1: Minimal config
(format #t "Test 1: Minimal openssh-configuration~%")
(catch #t
  (lambda ()
    (let ((config (openssh-configuration)))
      (format #t "✓ Default config works~%")
      (pretty-print config)
      (newline)))
  (lambda (key . args)
    (format #t "✗ ERROR: ~a ~a~%~%" key args)))

;; Test 2: With permit-root-login
(format #t "Test 2: With permit-root-login~%")
(catch #t
  (lambda ()
    (let ((config (openssh-configuration
                   (permit-root-login 'prohibit-password))))
      (format #t "✓ permit-root-login works~%")
      (pretty-print config)
      (newline)))
  (lambda (key . args)
    (format #t "✗ ERROR: ~a ~a~%~%" key args)))

;; Test 3: With password-authentication?
(format #t "Test 3: With password-authentication?~%")
(catch #t
  (lambda ()
    (let ((config (openssh-configuration
                   (password-authentication? #f))))
      (format #t "✓ password-authentication? works~%")
      (pretty-print config)
      (newline)))
  (lambda (key . args)
    (format #t "✗ ERROR: ~a ~a~%~%" key args)))

;; Test 4: With authorized-keys
(format #t "Test 4: With authorized-keys~%")
(catch #t
  (lambda ()
    (let ((config (openssh-configuration
                   (authorized-keys
                    `(("root" ,(plain-file "test-key.pub" "ssh-ed25519 AAAA...test")))))))
      (format #t "✓ authorized-keys works~%")
      (pretty-print config)
      (newline)))
  (lambda (key . args)
    (format #t "✗ ERROR: ~a ~a~%~%" key args)))

;; Test 5: All together
(format #t "Test 5: All fields together~%")
(catch #t
  (lambda ()
    (let ((config (openssh-configuration
                   (permit-root-login 'prohibit-password)
                   (password-authentication? #f)
                   (authorized-keys
                    `(("root" ,(plain-file "test-key.pub" "ssh-ed25519 AAAA...test")))))))
      (format #t "✓ Complete config works!~%")
      (pretty-print config)
      (newline)))
  (lambda (key . args)
    (format #t "✗ ERROR: ~a ~a~%~%" key args)))
