#!/usr/bin/env guile
!#
;;; Test SSH service S-expression generation
;;; Tests just the SSH service generation logic

(use-modules (ice-9 pretty-print)
             (ice-9 textual-ports)
             (srfi srfi-13)
             (guix read-print))

(define test-deploy-key
  "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBtESyRjo6GOaeM0YjbtOQ8L3qZpQ8eFple/nt0tkfRd test@example")

;; Test different ways to generate the SSH service S-expression

(define (test-approach-1 deploy-key)
  "Using nested lists with quote"
  (list
   `(service openssh-service-type
             (openssh-configuration
              (permit-root-login 'prohibit-password)
              (password-authentication? #f)
              (authorized-keys
               (quasiquote (("root" ,(plain-file "deploy-key.pub"
                                                 ,deploy-key)))))))))

(define (test-approach-2 deploy-key)
  "Using proper quasiquote with unquote-splicing"
  `((service openssh-service-type
             (openssh-configuration
              (permit-root-login 'prohibit-password)
              (password-authentication? #f)
              (authorized-keys
               (quasiquote (("root" (unquote (plain-file "deploy-key.pub" ,deploy-key))))))))))

(define (test-approach-3 deploy-key)
  "Build as pure data (no evaluation)"
  (list
   (list 'service 'openssh-service-type
         (list 'openssh-configuration
               (list 'permit-root-login (list 'quote 'prohibit-password))
               (list 'password-authentication? #f)
               (list 'authorized-keys
                     (list 'quasiquote
                           (list (list "root"
                                       (list 'unquote
                                             (list 'plain-file "deploy-key.pub" deploy-key))))))))))))

(define (test-approach-4 deploy-key)
  "Manually construct with symbols"
  (list
   (list 'service 'openssh-service-type
         (list 'openssh-configuration
               '(permit-root-login 'prohibit-password)
               '(password-authentication? #f)
               (list 'authorized-keys
                     (list 'quasiquote
                           (list (list "root"
                                       (list 'unquote
                                             (list 'plain-file "deploy-key.pub" deploy-key))))))))))

(format #t "=== Testing SSH Service S-expression Generation ===~%~%")

(format #t "Deploy key: ~a~%~%" test-deploy-key)

(format #t "--- Approach 1: Nested lists with embedded quasiquote ---~%")
(pretty-print (car (test-approach-1 test-deploy-key)))
(newline)

(format #t "--- Approach 2: Quasiquote with explicit unquote ---~%")
(pretty-print (car (test-approach-2 test-deploy-key)))
(newline)

(format #t "--- Approach 3: Nested quasiquotes ---~%")
(catch #t
  (lambda ()
    (pretty-print (car (test-approach-3 test-deploy-key))))
  (lambda (key . args)
    (format #t "ERROR: ~a ~a~%" key args)))
(newline)

(format #t "--- Approach 4: Manual symbol construction ---~%")
(pretty-print (car (test-approach-4 test-deploy-key)))
(newline)

;; Now show what it should look like as printed code
(format #t "--- Reference: How it should look in config.scm ---~%")
(format #t "(service openssh-service-type~%")
(format #t "         (openssh-configuration~%")
(format #t "          (permit-root-login 'prohibit-password)~%")
(format #t "          (password-authentication? #f)~%")
(format #t "          (authorized-keys~%")
(format #t "           `((\"root\" ,(plain-file \"deploy-key.pub\" \"~a\"))))))~%~%" test-deploy-key)

;; Test if we can create a valid openssh-configuration
(format #t "--- Testing if openssh-configuration is accessible ---~%")
(catch #t
  (lambda ()
    (use-modules (gnu services ssh))
    (format #t "✓ Can import (gnu services ssh)~%")

    ;; Try to create a minimal config
    (let ((test-config
           (openssh-configuration
            (permit-root-login 'prohibit-password)
            (password-authentication? #f))))
      (format #t "✓ Can create openssh-configuration record~%")
      (format #t "Config: ~a~%" test-config)))
  (lambda (key . args)
    (format #t "✗ ERROR: ~a ~a~%" key args)))
