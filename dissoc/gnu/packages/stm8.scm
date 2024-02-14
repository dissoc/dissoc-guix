;;; Copyright © 2024 Justin Bishop <mail@dissoc.me>

(define-module (dissoc gnu packages stm8)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public stm8flash
  (package
   (name "stm8flash")
   (version "c44f0d4")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference (url "https://github.com/vdudouyt/stm8flash.git")
                         (commit "c44f0d4f03e7f5bd30938b708248c331e92482de")))
     (sha256
      (base32 "1z0d23lzm0bgjbppyix6v2gpv6llkx2wpyx64znsw6sjjsaqy1c3"))))
   (build-system gnu-build-system)
   (arguments
    `(#:make-flags '("CC=gcc")
      #:phases
      (modify-phases %standard-phases
                     (delete 'configure)
                     (delete 'check)
                     (replace 'install
                              (lambda* (#:key outputs #:allow-other-keys)
                                (let* ((out  (assoc-ref outputs "out"))
                                       (bin (string-append out "/bin")))
                                  (install-file "stm8flash" bin)))))))
   (native-inputs `(("pkg-config" ,pkg-config)
                    ("libusb" ,libusb)))
   (synopsis "STM8 Flasher")
   (description "Utility for flashing STM8 MCU family via ST-LINK (V1 and V2)")
   (home-page "https://github.com/vdudouyt/stm8flash")
   (license license:gpl2+)))
