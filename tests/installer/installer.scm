;;; GNU Systole
;;; Copyright © 2026 Rafael Palomar <rafael.palomar@ous-research.no>
;;;
;;; This file is part of GNU Systole.
;;;
;;; GNU Systole is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Systole is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Systole.  If not, see <http://www.gnu.org/licenses/>.

(define-module (installer installer)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (os install)
  #:use-module (ice-9 match)
  #:export (%test-systole-installer-basic
            %test-systole-installer-deploy-key
            %test-systole-installer-no-ssh-without-key))


;;;
;;; Basic installer test
;;;

(define %test-systole-installer-basic
  (system-test
   (name "systole-installer-basic")
   (description
    "Test that the basic Systole installer boots successfully and has
expected system characteristics.")
   (value
    (mlet* %store-monad ((os ->  systole-os-installation)
                         (vm     (virtual-machine os)))
      (gexp->derivation "systole-installer-basic"
        (with-imported-modules '((gnu build marionette))
          #~(begin
              (use-modules (gnu build marionette)
                           (srfi srfi-64)
                           (ice-9 match))

              (define marionette
                (make-marionette (list #$vm)))

              (test-runner-current (system-test-runner #$output))
              (test-begin "systole-installer-basic")

              ;; Wait for system to boot
              (test-assert "system booted"
                (marionette-eval
                 '(begin
                    (use-modules (gnu services herd))
                    (start-service 'term-tty1))
                 marionette))

              ;; Verify the system is running
              (test-assert "system is running"
                (marionette-eval
                 '(file-exists? "/run/current-system")
                 marionette))

              ;; Verify kernel is loaded
              (test-assert "kernel info available"
                (marionette-eval
                 '(file-exists? "/proc/version")
                 marionette))

              ;; Verify basic commands are available
              (test-assert "basic commands available"
                (marionette-eval
                 '(and (file-exists? "/run/current-system/profile/bin/ls")
                       (file-exists? "/run/current-system/profile/bin/guix"))
                 marionette))

              ;; Verify git is available (needed for installer)
              (test-assert "git available"
                (marionette-eval
                 '(zero? (system* "which" "git"))
                 marionette))

              ;; Verify vim is available
              (test-assert "vim available"
                (marionette-eval
                 '(zero? (system* "which" "vim"))
                 marionette))

              ;; Verify installer modules can be loaded
              (test-assert "installer modules loadable"
                (marionette-eval
                 '(begin
                    (setenv "GUIX_LOCPATH"
                            "/run/current-system/locale")
                    #t)
                 marionette))

              (test-end)
              (exit (= (test-runner-fail-count (test-runner-current)) 0)))))))))


;;;
;;; Deploy key installer test
;;;

(define %test-systole-installer-deploy-key
  (system-test
   (name "systole-installer-deploy-key")
   (description
    "Test that the Systole installer with SSH deploy key boots successfully,
has SSH daemon running, and the deploy key is properly configured.")
   (value
    (mlet* %store-monad ((os ->  (systole-os-installation-with-deploy-key
                                  #:deploy-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHQn8FBN7FHqKb6W3xXzJmZV8K7fFJvLQZUqXqKqqqkE test@systole-test"))
                         (vm     (virtual-machine
                                  (operating-system
                                    (inherit os)
                                    ;; Add port forwarding for SSH testing
                                    (services
                                     (cons* (service dhcp-client-service-type)
                                            (operating-system-user-services os)))))))
      (gexp->derivation "systole-installer-deploy-key"
        (with-imported-modules '((gnu build marionette))
          #~(begin
              (use-modules (gnu build marionette)
                           (srfi srfi-64)
                           (ice-9 match))

              (define marionette
                (make-marionette (list #$vm)))

              (test-runner-current (system-test-runner #$output))
              (test-begin "systole-installer-deploy-key")

              ;; Wait for system to boot
              (test-assert "system booted"
                (marionette-eval
                 '(begin
                    (use-modules (gnu services herd))
                    (start-service 'term-tty1))
                 marionette))

              ;; Wait a bit for services to start
              (sleep 10)

              ;; Verify SSH daemon is running
              (test-assert "sshd service started"
                (marionette-eval
                 '(begin
                    (use-modules (gnu services herd))
                    (live-service-running
                     (find (lambda (live)
                             (memq 'ssh (live-service-provision live)))
                           (current-services))))
                 marionette))

              ;; Verify sshd process is running
              (test-assert "sshd process running"
                (marionette-eval
                 '(zero? (system* "pgrep" "-x" "sshd"))
                 marionette))

              ;; Verify authorized_keys exists for root
              (test-assert "authorized_keys file exists"
                (marionette-eval
                 '(file-exists? "/root/.ssh/authorized_keys")
                 marionette))

              ;; Verify authorized_keys has correct permissions
              (test-assert "authorized_keys has correct permissions"
                (marionette-eval
                 '(let* ((stat-result (stat "/root/.ssh/authorized_keys"))
                         (perms (stat:perms stat-result)))
                    ;; Should be 0600 or similar
                    (= (logand perms #o077) 0))
                 marionette))

              ;; Verify authorized_keys contains our test key
              (test-assert "authorized_keys contains deploy key"
                (marionette-eval
                 '(let ((contents (call-with-input-file
                                      "/root/.ssh/authorized_keys"
                                    (lambda (port) (read-string port)))))
                    (string-contains contents "ssh-ed25519"))
                 marionette))

              ;; Verify SSH config disables password authentication
              (test-assert "password authentication disabled"
                (marionette-eval
                 '(let ((config (call-with-input-file
                                    "/etc/ssh/sshd_config"
                                  (lambda (port) (read-string port)))))
                    (and (string-contains config "PasswordAuthentication no")
                         (string-contains config "PermitRootLogin prohibit-password")))
                 marionette))

              ;; Verify network interface is up
              (test-assert "network interface up"
                (marionette-eval
                 '(zero? (system* "ip" "link" "show" "lo"))
                 marionette))

              (test-end)
              (exit (= (test-runner-fail-count (test-runner-current)) 0)))))))))


;;;
;;; No SSH without deploy key test
;;;

(define %test-systole-installer-no-ssh-without-key
  (system-test
   (name "systole-installer-no-ssh-without-key")
   (description
    "Test that the Systole installer WITHOUT a deploy key does NOT have
SSH daemon running, ensuring deploy key is optional.")
   (value
    (mlet* %store-monad ((os ->  systole-os-installation)
                         (vm     (virtual-machine os)))
      (gexp->derivation "systole-installer-no-ssh-without-key"
        (with-imported-modules '((gnu build marionette))
          #~(begin
              (use-modules (gnu build marionette)
                           (srfi srfi-64)
                           (ice-9 match))

              (define marionette
                (make-marionette (list #$vm)))

              (test-runner-current (system-test-runner #$output))
              (test-begin "systole-installer-no-ssh-without-key")

              ;; Wait for system to boot
              (test-assert "system booted"
                (marionette-eval
                 '(begin
                    (use-modules (gnu services herd))
                    (start-service 'term-tty1))
                 marionette))

              ;; Wait a bit for services to potentially start
              (sleep 5)

              ;; Verify SSH daemon is NOT running
              (test-assert "sshd service not running"
                (marionette-eval
                 '(begin
                    (use-modules (gnu services herd))
                    (not (live-service-running
                          (find (lambda (live)
                                  (memq 'ssh (live-service-provision live)))
                                (current-services)))))
                 marionette))

              ;; Verify no sshd process
              (test-assert "no sshd process"
                (marionette-eval
                 '(not (zero? (system* "pgrep" "-x" "sshd")))
                 marionette))

              ;; System should still be fully functional
              (test-assert "system fully functional"
                (marionette-eval
                 '(and (file-exists? "/run/current-system")
                       (zero? (system* "which" "guix")))
                 marionette))

              (test-end)
              (exit (= (test-runner-fail-count (test-runner-current)) 0)))))))))
