;;; Copyright © 2024 Justin Bishop <mail@dissoc.me>

(define-module (dissoc gnu packages linux)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public dwarves
  (package
   (name "dwarves")
   (version "1.24")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/acmel/dwarves.git")
                  (recursive? #t)
                  (commit (string-append "v" version))))
            (sha256
             (base32
              "1a3w1an3g3506sqlnrzv9za14qxdrbkzlyjg5fh8543rmda1xyrq"))))
   (build-system cmake-build-system)
   (arguments `(#:tests? #f
                #:validate-runpath? #f
                #:configure-flags '("-D__LIB=lib")))
   (native-inputs (list elfutils zlib libdwarf))
   (home-page "https://github.com/acmel/dwarves")
   (synopsis "Pahole and the dwarves")
   (description "dwarves is a set of tools that use the debugging information
inserted in ELF binaries by compilers such as GCC, used by well known debuggers
such as GDB, and more recent ones such as systemtap.")
   (license license:gpl2)))
