;;
;; Copyright @ 2025 Oslo University Hospital
;;
;; This file is part of SystoleOS.
;;
;; SystoleOS is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later version.
;;
;; SystoleOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with SystoleOS. If not, see <https://www.gnu.org/licenses/>.
;;

(define-module (systole services dicomd-service)
  #:autoload   (guix least-authority) (least-authority-wrapper)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module ((gnu system file-systems) #:select (file-system-mapping))
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (gnu build linux-container)
  #:export (dicomd-configuration
            dicomd-configuration?
            dicomd-service-type))

(define %dicomd-account-service
  (list (user-account
         (name "dicomd")
         (group "dicom")
         (system? #t)
         (comment "DICOMD Service Account")
         (supplementary-groups '("dicom"))
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))
        (user-group
         (name "dicom")
         (id 1031)
         (system? #t))))

(define-configuration/no-serialization dicomd-configuration
  (package
    (package dcmtk)
    "The DCMTK package providing the @command{storescp} executable.")
  (port
   (integer 1104)
   "TCP port on which the DICOM store SCP listens.")
  (aetitle
   (string "DICOMD")
   "DICOM Application Entity title announced by the daemon.")
  (output-directory
   (string "/var/dicom-store")
   "Directory where received DICOM objects are stored.  It is created at
activation time and owned by the service account.")
  (account
   (user-account (car %dicomd-account-service))
   "System account the daemon runs as.")
  (group
   (user-group (cadr %dicomd-account-service))
   "System group of the service account."))

(define (dicomd-shepherd-service config)
  (let* ((package (dicomd-configuration-package config))
         (port (dicomd-configuration-port config))
         (aetitle (dicomd-configuration-aetitle config))
         (output-directory (dicomd-configuration-output-directory config))
         (account (dicomd-configuration-account config))
         (dicomd (least-authority-wrapper
                  (file-append package "/bin/storescp")
                  #:name "dicomd"
                  #:namespaces
                  (fold delq %namespaces '(net))
                  #:mappings (list (file-system-mapping
                                    (source output-directory)
                                    (target output-directory)
                                    (writable? #t))))))
    (shepherd-service
     (provision '(dicom-daemon))
     ;; storescp binds a TCP port; wait for the network to be up.
     (requirement '(user-processes networking))
     (documentation
      "Run storescp, a DICOM Store SCP, inside a least-authority wrapper.")
     (auto-start? #t)
     (start #~(make-forkexec-constructor
               (list #$dicomd
                     "--aetitle" #$aetitle
                     "--output-directory" #$output-directory
                     "-sp"
                     #$(number->string port))
               #:user #$(user-account-name account)
               #:group #$(user-account-group account)
               #:file-creation-mask #o002))
     (stop #~(make-kill-destructor)))))

(define (dicomd-activation config)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (let* ((user (getpw #$(user-account-name
                               (dicomd-configuration-account config))))
               (directory #$(dicomd-configuration-output-directory config)))
          ;; dicomd creates a Unix-domain socket in DIRECTORY.
          (mkdir-p directory)
          (chown directory (passwd:uid user) (passwd:gid user))
          (chmod directory #o775)))))

(define dicomd-service-type
  (service-type
   (name 'dicomd)
   (description
    "Run @command{storescp} from DCMTK as a namespace-isolated DICOM Store
SCP daemon: it listens on a TCP port for incoming DICOM associations and
writes received objects to a dedicated store directory.")
   (extensions
    (list (service-extension account-service-type
                             (lambda (config)
                               (list
                                (dicomd-configuration-account config)
                                (dicomd-configuration-group config))))
          (service-extension activation-service-type
                             dicomd-activation)
          (service-extension shepherd-root-service-type
                             (compose list dicomd-shepherd-service))))
   (default-value (dicomd-configuration))))
