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

(define-module (guix-systole packages maths)
  #:use-module (gnu packages maths)
  #:use-module (guix packages))

;; As of 26th of May 2025, Netcdf in the GNU Savannah project uses HDF5 version 
;; 1.14
(define-public netcdf-slicer
  (package
    (inherit netcdf)
    (name "netcdf-slicer")
    (inputs (modify-inputs (package-inputs netcdf)
            (replace "hdf5" hdf5-1.10)))))
