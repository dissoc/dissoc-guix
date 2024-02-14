(define-module (dissoc packages forensics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public sleuthkit
  (package
   (name "sleuthkit")
   (version "4.12.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference (url "https://github.com/sleuthkit/sleuthkit.git")
                                (commit (string-append "sleuthkit-" version))))
            (sha256
             (base32
              "13hpiwiv6y7kl0vzg6gmkyk1f19323dannn3r6ivhw28d5im97db"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f)) ;;tests try to wget files
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("libtool" ,libtool)))
   (home-page "https://www.sleuthkit.org/sleuthkit/")
   (synopsis "Disk image forensic tools")
   (description "The Sleuth Kit® (TSK) is a library and collection of command
line tools that allow you to investigate disk images. The core functionality of
TSK allows you to analyze volume and file system data. The library can be
incorporated into larger digital forensics tools and the command line tools can
be directly used to find evidence.")
   (license (list license:asl2.0
                  license:gpl2
                  license:gpl3
                  license:ibmpl1.0
                  license:bsd-3
                  license:cpl1.0
                  license:expat))))
