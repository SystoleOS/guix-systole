(define-module (guix-systole packages qt)
	       #:use-module (guix packages)
	       #:use-module (gnu packages qt))

(define-public qtwebkit-slicer
	       (package
		 (inherit qtwebkit)
		 (name "qtwebkit-slicer")
		 (supported-systems (list "x86_64-linux" "aarch64-linux"))))

(define-public python-pyqt-slicer
	       (package
		 (inherit python-pyqt)
		 (name "python-pyqt-slicer")
		    (inputs (modify-inputs (package-inputs python-pyqt)
					   (replace "qtwebkit" qtwebkit-slicer)))
		 (supported-systems (list "x86_64-linux" "aarch64-linux"))))
