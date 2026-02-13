#!/usr/bin/env guile
!#

(use-modules (ice-9 pretty-print))

(define test-key "ssh-ed25519 AAAAC3Nza...test")

(format #t "=== Testing SSH Service S-expression ===~%~%")

;; What we want the final code to look like:
(format #t "Target output (what should be in config.scm):~%")
(display "(service openssh-service-type
         (openssh-configuration
          (permit-root-login 'prohibit-password)
          (password-authentication? #f)
          (authorized-keys
           `((\"root\" ,(plain-file \"deploy-key.pub\" \"ssh-ed25519 AAAAC3Nza...test\"))))))
")
(newline)
(newline)

;; Approach 1: Using backquote template
(format #t "Approach 1: Backquote with splice for key~%")
(define approach-1
  `(service openssh-service-type
            (openssh-configuration
             (permit-root-login 'prohibit-password)
             (password-authentication? #f)
             (authorized-keys
              (quasiquote (("root" (unquote (plain-file "deploy-key.pub" ,test-key)))))))))

(pretty-print approach-1)
(newline)

;; Approach 2: Pure data construction
(format #t "Approach 2: Build as list structure~%")
(define approach-2
  (list 'service 'openssh-service-type
        (list 'openssh-configuration
              '(permit-root-login 'prohibit-password)
              '(password-authentication? #f)
              (list 'authorized-keys
                    (list 'quasiquote
                          (list (list "root"
                                      (list 'unquote
                                            (list 'plain-file "deploy-key.pub" test-key)))))))))

(pretty-print approach-2)
(newline)

;; Test with actual openssh module
(format #t "Checking if we can load openssh module...~%")
(catch #t
  (lambda ()
    (use-modules (gnu services ssh))
    (format #t "✓ Successfully loaded (gnu services ssh)~%"))
  (lambda (key . args)
    (format #t "✗ Failed to load: ~a~%" key)))
