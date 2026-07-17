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

(define-module (systole packages libarchive)
  #:use-module (guix build-system cmake)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public libarchive-slicer
  ;; Slicer/libarchive branch slicer-v3.8.1-2025-06-01, required by
  ;; Slicer >= 5.10.  Slicer feeds libarchive remote content (sample data,
  ;; extension archives), and 3.8.1 carries the security fixes accumulated
  ;; since the previous 3.6.1 pin (CVE-2022-36227, the 2025 RAR series
  ;; CVE-2025-5914..5918, and friends).
  (let ((commit "30a9657b6c12f675fd5057cea017b2c9bd924dda")
        (revision "0"))
    (package
      (name "libarchive-slicer")
      (version (git-version "3.8.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Slicer/libarchive")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ihf1g7hbg85wvsivzxl264kkkgvq667y8jvnswracv1dlcrcgmf"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f
        #:configure-flags #~(list "-DCMAKE_C_COMPILER:FILEPATH=gcc"
                                  "-DBUILD_SHARED_LIBS:BOOL=ON"
                                  "-DBUILD_TESTING:BOOL=OFF"
                                  "-DENABLE_ACL:BOOL=OFF"
                                  "-DENABLE_BZip2:BOOL=OFF"
                                  "-DENABLE_CAT:BOOL=OFF"
                                  "-DENABLE_CNG:BOOL=OFF"
                                  "-DENABLE_CPIO:BOOL=OFF"
                                  "-DENABLE_EXPAT:BOOL=OFF"
                                  "-DENABLE_ICONV:BOOL=OFF"
                                  "-DENABLE_LIBB2:BOOL=OFF"
                                  "-DENABLE_LibGCC:BOOL=OFF"
                                  "-DENABLE_LIBXML2:BOOL=OFF"
                                  "-DENABLE_LZ4:BOOL=OFF"
                                  "-DENABLE_LZMA:BOOL=OFF"
                                  "-DENABLE_LZO:BOOL=OFF"
                                  "-DENABLE_MBEDTLS:BOOL=OFF"
                                  "-DENABLE_NETTLE:BOOL=OFF"
                                  "-DENABLE_OPENSSL:BOOL=OFF"
                                  ;; 3.8 renamed the PCRE switch (PCRE2); keep
                                  ;; the old spelling too, it is ignored.
                                  "-DENABLE_PCREPOSIX:BOOL=OFF"
                                  "-DENABLE_PCRE2POSIX:BOOL=OFF"
                                  "-DENABLE_TAR:BOOL=OFF"
                                  "-DENABLE_TEST:BOOL=OFF"
                                  "-DENABLE_XATTR:BOOL=OFF"
                                  "-DENABLE_ZSTD:BOOL=OFF"
                                  "-DARCHIVE_CRYPTO_MD5_LIBSYSTEM:BOOL=OFF")))
      (home-page "https://libarchive.org/")
      (synopsis "Multi-format archive and compression library (Slicer fork)")
      (description
       "Libarchive provides a flexible interface for reading and writing
archives in various formats such as tar and cpio, and reading and writing
archives compressed with filters such as gzip and bzip2.  The library is
inherently stream-oriented; readers serially iterate through the archive,
writers serially add things to the archive.  This package builds Slicer's
fork, configured as 3D Slicer requires it.")
      (license license:bsd-2))))
