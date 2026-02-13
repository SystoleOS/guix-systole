#!/usr/bin/env guile
!#
;;; Test config generation without running full installer
;;; Simulates what the installer does and outputs the generated config.scm

(add-to-load-path "/home/rafael/src/guix-systole/system")
(add-to-load-path "/home/rafael/src/guix-systole/systole")

(use-modules (installer steps)
             (gnu installer steps)
             (gnu installer record)
             (guix read-print)
             (ice-9 textual-ports))

;; Create a deploy key file for testing
(unless (file-exists? "/tmp/test-deploy-key.pub")
  (call-with-output-file "/tmp/test-deploy-key.pub"
    (lambda (port)
      (display "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBtESyRjo6GOaeM0YjbtOQ8L3qZpQ8eFple/nt0tkfRd test@test" port))))

;; Simulate having the key in /etc (where the installer would have it)
(system "mkdir -p /tmp/fake-etc && cp /tmp/test-deploy-key.pub /tmp/fake-etc/systole-deploy-key.pub")

;; Mock the installer steps (simplified - just enough to test)
(define mock-steps
  (list
   (installer-step
    (id 'locale)
    (compute (lambda () "en_US.utf8"))
    (configuration-formatter
     (lambda (result) '((locale "en_US.utf8")))))

   (installer-step
    (id 'hostname)
    (compute (lambda () "test-system"))
    (configuration-formatter
     (lambda (result) '((host-name "test-system")))))

   (installer-step
    (id 'timezone)
    (compute (lambda () "UTC"))
    (configuration-formatter
     (lambda (result) '((timezone "UTC")))))))

(define mock-results
  '((locale . "en_US.utf8")
    (hostname . "test-system")
    (timezone . "UTC")))

(format #t "=== Testing Config Generation with Deploy Key ===~%~%")

;; Override /etc path for testing
(let ((original-deploy-key-file "/etc/systole-deploy-key.pub"))
  ;; Temporarily patch the file path in the function
  (catch #t
    (lambda ()
      (format #t "Generating configuration...~%~%")

      ;; Call the actual systole-format-configuration
      (let ((config (systole-format-configuration mock-steps mock-results)))

        (format #t "=== Generated Configuration ===~%~%")
        (pretty-print-with-comments (current-output-port) config)
        (newline)

        ;; Write to file
        (call-with-output-file "/tmp/test-generated-installer-config.scm"
          (lambda (port)
            (pretty-print-with-comments port config)))

        (format #t "~%✓ Configuration written to /tmp/test-generated-installer-config.scm~%~%")

        ;; Try to validate it
        (format #t "Attempting to validate generated config...~%")
        (catch #t
          (lambda ()
            (use-modules (gnu))
            (let* ((config-sexp (call-with-input-file "/tmp/test-generated-installer-config.scm" read))
                   ;; Extract the operating-system part
                   (os-part (caddr (caddr config-sexp))))
              (format #t "✓ Config can be read as S-expression~%")
              (format #t "~%Checking for SSH service...~%")
              (if (string-contains (call-with-input-file "/tmp/test-generated-installer-config.scm" get-string-all)
                                   "openssh-service-type")
                  (format #t "✓ SSH service found in config~%")
                  (format #t "✗ SSH service NOT found in config~%"))))
          (lambda (key . args)
            (format #t "✗ Validation error: ~a ~a~%"key args)))))

    (lambda (key . args)
      (format #t "✗ ERROR generating config: ~a~%" key)
      (format #t "   Details: ~a~%" args)
      (exit 1))))
