;;; Copyright © 2024 Justin Bishop <mail@dissoc.me>

(define-module (dissoc gnu packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages wm)
  #:use-module (guix packages))

(define-public sbcl-stumpwm-clipboard-history
  (package
   (inherit (@@ (gnu packages wm) stumpwm-contrib))
   (name "sbcl-stumpwm-clipboard-history")
   (arguments
    '(#:asd-systems '("clipboard-history")
      #:tests? #f
      #:phases
      (modify-phases %standard-phases
                     (add-after 'unpack 'chdir
                                (lambda _
                                  (chdir "util/clipboard-history"))))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib")
   (synopsis "Integrates clipboard history with StumpWM")
   (description "This package provides an interface which integrates
clipboard history into StumpWM.")
   (license (list license:gpl2+ license:gpl3+ license:bsd-2))))

(define-public sbcl-stumpwm-mpd
  (package
   (inherit (@@ (gnu packages wm) stumpwm-contrib))
   (name "sbcl-stumpwm-mpd")
   (arguments
    '(#:asd-systems '("mpd")
      #:tests? #f
      #:phases
      (modify-phases %standard-phases
                     (add-after 'unpack 'chdir
                                (lambda _
                                  (chdir "minor-mode/mpd"))))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib")
   (synopsis "Integrates mpd with StumpWM")
   (description "This package provides an interface which integrates
mpd into StumpWM.")
   (license (list license:gpl2+ license:gpl3+ license:bsd-2))))
