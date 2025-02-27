; -----------------------------------------------------------;
;                                                            ;
;    CTK (Common Toolkit) Guile Scheme package definition    ;
;                                                            ;
; -----------------------------------------------------------;

; Defining module
(define-module (ctk)
    #: use-module ()
    #: use-module ()
)

; Packages should be listed under input;
;   - DCMTK
;       - gnu packages image-processing

; Defining package definition for CTK (Common Toolkit).
(define-public systole-common-toolkit
    (package                                ; Creating new package.
        (inherit common-toolkit)            
        (version "slicer-5.4.0")            ; Using a specific slicer version for this package definition.

        (source                             ; Specifying source.
            (origin                         ; Setting origin for source definition.
                (method git-fetch)          ; Using git fetch to retrieve source.

                (uri (git-reference         ; Specifying URI (location of origin source).
                    (url "")
                    (commit "")))

                (sha256(base32 ""))))

        (arguments
            (#: tests? #f
                #: configure flags
                    (list
                        "-"
                        )))

        ;;  Home page might not be correct as slicer might have a self configured one.
        ;   Hence the once listed is the original Github one.
        (home-page "github.com/commontk/CTK")        
    )
)
