(define-module (dissoc gnu packages crypto)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages wxwidgets)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp))

(define-public veracrypt
  (package
   (name "veracrypt")
   (version "1.26.7")
   (source (origin
            (method git-fetch)
            (uri (git-reference (url "https://github.com/veracrypt/VeraCrypt.git")
                                (commit (string-append "VeraCrypt_" version))))
            (sha256
             (base32
              "1xb5awfky7l536k77p075k55rq1n503987l5zrnhynjrgbv13370"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (delete 'check)
       (add-before 'build 'chdir
                   (lambda _
                     (setenv "CC" "gcc")
                     (chdir "src")))
       (replace 'install
                (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (bin (string-append out "/bin")))
                    (install-file "Main/veracrypt" bin)))))))
   (inputs
    `(("fuse" ,fuse-2)
      ("lvm2" ,lvm2)
      ("pcsc-lite", pcsc-lite)
      ("wxwidgets" ,wxwidgets)))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("yasm" ,yasm)))
   (synopsis "VeraCrypt is a free open source disk encryption software")
   (description "VeraCrypt is a free open source disk encryption software for
Windows, Mac OSX and Linux. Brought to you by IDRIX (https://www.idrix.fr) and
based on TrueCrypt 7.1a.")
   (home-page "https://www.veracrypt.fr/en/Home.html")
   (license license:asl2.0)))
