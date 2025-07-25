(define-module (guix-systole packages monk)
  #:use-module (guix build-system python)
  #:use-module (guix build-system cmake)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages django)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd))

(define-public monk-backend
  (package
    (name "monk-backend")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://github.com/asiangoldfish/MONK-system/archive/db5385e512f2a4fa96c49e6c632ce02f9de9caed.tar.gz")
       (sha256
        (base32 "0s9ryygm19b6m3j4bmpg63d3qrxb0din0vp09pq6m9ydh8s8ivki"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (copy-recursively "." out)
                        (mkdir-p (string-append out "/bin"))
                        (symlink (string-append out "/manage.py")
                                 (string-append out "/bin/manage.py")) #t)))
                  (add-after 'install 'mod-permissions
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (chmod (string-append out "/bin/manage.py") #o755) #t))))))
    (inputs (list git))
    (native-inputs `(("python" ,python-3.11)
                     ("python-pip" ,python-pip)))
    (propagated-inputs (list monk-library
                             python-asgiref
                             python-astroid
                             python-attrs
                             python-black
                             python-certifi
                             python-cffi
                             python-click
                             python-colorama
                             python-coverage
                             python-dill
                             python-django
                             python-h11
                             python-idna
                             python-iniconfig
                             python-isort
                             python-mccabe
                             python-mypy-extensions
                             python-numpy
                             python-outcome
                             python-packaging
                             python-pandas
                             python-pathspec
                             python-platformdirs
                             python-plotly
                             python-pluggy
                             python-pycparser
                             python-pylint
                             python-pysocks
                             python-pytest
                             python-pytest-cov
                             python-pytest-django
                             python-dateutil
                             python-pytz
                             python-selenium
                             python-six
                             python-sniffio
                             python-sortedcontainers
                             python-sqlparse
                             python-tenacity
                             python-tomlkit
                             python-trio
                             python-trio-websocket
                             python-typing-extensions
                             python-tzdata
                             python-urllib3
                             python-wsproto
                             python-zope-interface))
    (synopsis "Django web application")
    (description "Custom Django web application packaged for Guix")
    (home-page "https://example.com")
    (license #f)))

(define-public monk-library
  (package
    (name "monk-library")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (recursive? #t)
             (url "https://github.com/asiangoldfish/MONK-library/")
             (commit "df10d8ecbb28669e02d1e60b547a6042986c6fce")))
       (sha256
        (base32 "1s13qadp0yyjak169viw3v6mg66w4lyaxpxrkk7rwxf5fkxlp3ic"))))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f))
    (native-inputs (list cmake))
    (propagated-inputs (list python-3.11))
    (synopsis "Django web application")
    (description "Custom Django web application packaged for Guix")
    (home-page "https://example.com")
    (license #f)))
