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
