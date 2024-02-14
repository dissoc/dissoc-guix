;;; Copyright © 2024 Justin Bishop <mail@dissoc.me>

(define-module (dissoc gnu packages tablet)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages linux)
  #:use-module (guix build-system linux-module)
  #:use-module (guix git-download)
  #:use-module (guix packages))

;; If user wants to generate package for a specific
;; kernel use the (make-digimend my-kernel) function
;; to create package. The provided digimend package
;; will use linux-libre as the kernel.

(define (make-digimend kernel)
  (package
   (name "digimend-module")
   (version "master")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/DIGImend/digimend-kernel-drivers.git")
                  (commit "eca6e1b701bffb80a293234a485ebf6b4bc85562")))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0dl0662839j1dav816m2qv8w2brdixr5l0lpqbshky57ranzvrkw"))))
   (build-system linux-module-build-system)
   (arguments
    `(#:tests? #f
      #:linux kernel))
   (home-page "https://digimend.github.io/")
   (synopsis
    "DIGImend graphics tablet drivers for the Linux kernel")
   (description
    "This is a collection of graphics tablet drivers for the Linux
kernel, produced and maintained by the DIGImend project. We maintain this
package to provide newer drivers for older kernel versions which don't have
them, and to allow users to test new drivers before we contribute them to
the mainline kernel.")
   (license license:gpl2+)))

(define-public digimend-module (make-digimend linux-libre))
