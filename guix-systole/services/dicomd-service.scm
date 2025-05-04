(define-module (guix-systole services dicomd-service)
  #:autoload   (guix least-authority) (least-authority-wrapper)
  #:use-module (ice-9 match)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu services)
  #:use-module (gnu services networking)
  #:use-module ((gnu system file-systems) #:select (file-system-mapping))
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module (guix records)
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

(define-record-type* <dicomd-configuration>
  dicomd-configuration make-dicomd-configuration dicomd-configuration?
  (package dicomd-configuration-package
           (default dcmtk))
  (port dicomd-configuration-port
        (default 1104))
  (aetitle dicomd-configuration-aetitle
           (default "DICOMD"))
  (output-directory dicomd-configuration-output-directory
                    (default "/var/dicom-store"))
  (account dicomd-configuration-account
           (default (car %dicomd-account-service)))
  (group dicomd-configuration-group
         (default (cadr %dicomd-account-service))))

(define dicomd-shepherd-service
  (match-lambda
    (($ <dicomd-configuration> package port aetitle output-directory account)
     (let ((dicomd (least-authority-wrapper
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
        (requirement '(user-processes))
        (documentation "DICOMD Service")
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
        (stop #~(make-kill-destructor)))))))

(define (dicomd-activation config)
  (with-imported-modules '((guix build utils))
                         #~(begin
                             (use-modules (guix build utils))
                             (let* ((user (getpw #$(user-account-name (dicomd-configuration-account config))))
                                    (directory #$(dicomd-configuration-output-directory config)))
                               ;; dicomd creates a Unix-domain socket in DIRECTORY.
                               (mkdir-p directory)
                               (chown directory (passwd:uid user) (passwd:gid user))
                               (chmod directory #o775)))))

(define dicomd-service-type
  (service-type
   (name 'dicomd)
   (description "DICOMD Service")
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
