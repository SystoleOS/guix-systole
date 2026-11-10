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

;;; Commentary:
;;;
;;; An origin method for the NCI Imaging Data Commons (IDC), modelled on
;;; git-fetch.  A cohort is named by a SQL query over a pinned release of
;;; the IDC index (the intent) and identified by the sha256 of what it
;;; downloads (the content):
;;;
;;;   (origin
;;;     (method idc-fetch)
;;;     (uri (idc-reference
;;;           (release "25.0.5")
;;;           (query "SELECT SeriesInstanceUID FROM idc_index
;;;                   WHERE collection_id = 'lidc_idri' AND Modality = 'CT'
;;;                   ORDER BY SeriesInstanceUID LIMIT 10")))
;;;     (file-name "lidc-ct-cohort")
;;;     (sha256 (base32 "...")))
;;;
;;; The query runs offline, inside the build, against the idc-index-data
;;; wheel of that release (itself a fixed-output origin).  It must return
;;; a SeriesInstanceUID column; table idc_index is the wheel's
;;; idc_index.parquet.  The output holds one directory per series, named
;;; by SeriesInstanceUID, plus manifest.csv with each series' UIDs, UUID,
;;; collection, licence and source DOI.
;;;
;;; Outputs are not substitutable by default: a substitute server would
;;; redistribute patient imaging, which raises licence questions
;;; (CC BY-NC collections) and could keep serving data that IDC later
;;; retracted.  Set (substitutable? #t) only for data you may redistribute.
;;;
;;; Code:

(define-module (systole idc-download)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (guix download)
  #:use-module (guix base16)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages duckdb)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages nss)
  #:use-module (ice-9 match)
  #:export (idc-reference
            idc-reference?
            idc-reference-release
            idc-reference-query
            idc-reference-substitutable?
            idc-fetch
            %idc-index-releases))

(define-record-type* <idc-reference>
  idc-reference make-idc-reference
  idc-reference?
  (release        idc-reference-release)          ;string, idc-index-data version
  (query          idc-reference-query)            ;string, SQL over idc_index
  (substitutable? idc-reference-substitutable?    ;boolean
                  (default #f)))

;; The IDC index releases this channel knows, as published on PyPI by
;; the IDC team (https://pypi.org/project/idc-index-data/).  The major
;; version is the IDC data release.  Add an entry per new release.
(define %idc-index-releases
  '(("25.0.5"
     "https://files.pythonhosted.org/packages/bd/10/c6391b44d1b2f6c744e1cb4018250cb1a5d80947d47ed945172d9165a3b4/idc_index_data-25.0.5-py3-none-any.whl"
     "1ea4ec812688759b71798249b6488eb2bce7178aabb3dccb00366b7a555de25c")))

(define (idc-index-origin release)
  (match (assoc release %idc-index-releases)
    ((_ url hex)
     (origin
       (method url-fetch)
       (uri url)
       (sha256 (base16-string->bytevector hex))))
    (#f
     (error "idc-fetch: unknown IDC index release" release
            (map car %idc-index-releases)))))

(define (idc-fetch-builder index)
  ;; Shared by every cohort of a release: the query reaches the builder
  ;; through #:env-vars, not through the gexp, as git-fetch does.
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 match)
                     (ice-9 rdelim))

        (define out #$output)
        (define query (getenv "idc query"))

        (setenv "SSL_CERT_DIR" #$(file-append nss-certs "/etc/ssl/certs"))
        (setenv "HOME" "/tmp")
        (mkdir-p out)

        ;; Resolve the query offline against the pinned index.
        (invoke #$(file-append unzip "/bin/unzip") "-q" #$index)
        (invoke #$(file-append duckdb "/bin/duckdb") "-c"
                (string-append
                 "CREATE VIEW idc_index AS"
                 " SELECT * FROM 'idc_index_data/idc_index.parquet';"
                 " CREATE TABLE cohort AS"
                 " SELECT SeriesInstanceUID, crdc_series_uuid, aws_bucket,"
                 " collection_id, license_short_name, source_DOI"
                 " FROM idc_index WHERE SeriesInstanceUID IN (" query ")"
                 " ORDER BY SeriesInstanceUID;"
                 " COPY cohort TO '" out "/manifest.csv' (HEADER);"
                 " COPY (SELECT aws_bucket, crdc_series_uuid, SeriesInstanceUID"
                 " FROM cohort) TO 'series.txt' (HEADER false, DELIMITER ' ');"))

        ;; Download each series from IDC's public bucket.
        (call-with-input-file "series.txt"
          (lambda (port)
            (let loop ((line (read-line port)))
              (unless (eof-object? line)
                (match (string-split line #\space)
                  ((bucket uuid uid)
                   (invoke #$(file-append s5cmd "/bin/s5cmd")
                           "--no-sign-request" "cp"
                           (string-append "s3://" bucket "/" uuid "/*")
                           (string-append out "/" uid "/"))))
                (loop (read-line port)))))))))

(define* (idc-fetch ref hash-algo hash
                    #:optional name
                    #:key (system (%current-system))
                    (guile (default-guile)))
  "Return a fixed-output derivation that fetches the IDC cohort described
by REF, an <idc-reference>, and whose recursive HASH-ALGO hash is HASH."
  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system)))
    (gexp->derivation (or name "idc-cohort")
                      (idc-fetch-builder
                       (idc-index-origin (idc-reference-release ref)))
                      #:script-name "idc-download"
                      #:env-vars `(("idc query" . ,(idc-reference-query ref)))
                      #:leaked-env-vars '("http_proxy" "https_proxy"
                                          "LC_ALL" "LC_MESSAGES" "LANG")
                      #:system system
                      #:local-build? #t   ;don't offload downloads
                      #:substitutable? (idc-reference-substitutable? ref)
                      #:hash-algo hash-algo
                      #:hash hash
                      #:recursive? #t
                      #:guile-for-build guile)))

;;; idc-download.scm ends here
