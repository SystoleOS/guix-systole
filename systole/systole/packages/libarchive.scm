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
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public libarchive-slicer
  (package
    (name "libarchive-slicer")
    (version "3.6.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/Slicer/libarchive/archive/14ec55f065e31fbbca23d3d96d43e07f21c6fb6d.tar.gz")
       (sha256
        (base32 "1l8a6x0dmvl1q664dzg1f3m92mhbwk8qyf92i4ab8yj720knazvv"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags (list "-DCMAKE_C_COMPILER:FILEPATH=gcc"
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
                               "-DENABLE_PCREPOSIX:BOOL=OFF"
                               "-DENABLE_TAR:BOOL=OFF"
                               "-DENABLE_TEST:BOOL=OFF"
                               "-DENABLE_XATTR:BOOL=OFF"
                               "-DENABLE_ZSTD:BOOL=OFF"
                               "-DARCHIVE_CRYPTO_MD5_LIBSYSTEM:BOOL=OFF")))
    (home-page "https://libarchive.org/")
    (synopsis "Multi-format archive and compression library.")
    (description
     "Libarchive provides a flexible interface for reading and writing archives in various formats such as tar and cpio.  Libarchive also supports reading and writing archives compressed using various compression filters such as gzip and bzip2. The library is inherently stream-oriented; readers serially iterate through the archive, writers serially add things to the archive. In particular, note that there is currently no built-in support for random access nor for in-place modification.  This package provides the `bsdcat', `bsdcpio' and `bsdtar' commands.")
    (license license:bsd-2)))
