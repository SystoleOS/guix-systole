;;; GNU Systole
;;; Copyright © 2026 Omar El Hajj
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
;;; End-to-end fixture for idc-fetch, after the example package Omar El
;;; Hajj posted on PR #122.  It pins the smallest LIDC-IDRI series in IDC
;;; index 25.0.5: a single chest radiograph (DX, 1 instance, ~6.9 MB,
;;; CC BY 3.0) instead of a 194-slice CT (~102 MB), so the test downloads
;;; little while still exercising the whole path (pinned index, offline
;;; DuckDB query, s5cmd download, recursive hash).
;;;
;;; Run with `./scripts/run-tests.sh data' (needs network), or:
;;;   guix build -L systole -f tests/data/lidc-dx-dataset.scm
;;;
;;; Deliberately not a channel package: manifest.scm and Cuirass would
;;; otherwise fetch patient imaging on every evaluation.
;;;
;;; Code:

(use-modules (guix packages)
             (guix gexp)
             (guix build-system trivial)
             ((guix licenses) #:prefix license:)
             (systole idc-download))

(package
  (name "lidc-dx-dataset")
  (version "1")
  (source
   (origin
     (method idc-fetch)
     (uri
      (idc-reference
       (release "25.0.5")
       (query
        "SELECT SeriesInstanceUID
         FROM idc_index
         WHERE collection_id = 'lidc_idri'
           AND Modality = 'DX'
         ORDER BY series_size_MB, SeriesInstanceUID
         LIMIT 1")))
     (file-name "lidc-dx-dataset")
     (sha256
      (base32
       "1ji8r1js0wil9h1wkqvwkhyi1d2x4fqha0vay9zz36i8jdxh2z81"))))
  (build-system trivial-build-system)
  (arguments
   (list
    #:modules '((guix build utils))
    #:builder
    #~(begin
        (use-modules (guix build utils))
        (copy-recursively #$source #$output))))
  (synopsis "Single LIDC-IDRI chest radiograph (idc-fetch test fixture)")
  (description
   "One DX series (LIDC-IDRI-0002) of the LIDC-IDRI collection, fetched
reproducibly from the NCI Imaging Data Commons with idc-fetch.")
  (home-page "https://www.cancerimagingarchive.net/collection/lidc-idri/")
  (license license:cc-by3.0))
