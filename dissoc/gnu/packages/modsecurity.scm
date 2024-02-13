(define-module (dissoc gnu packages modsecurity)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public libmodsecurity
  (package
   (name "libmodsecurity")
   (version "3.0.12")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/SpiderLabs/ModSecurity.git")
                  (commit (string-append "v" version))
                  (recursive? #t)))
            (sha256
             (base32
              "0f90ffwyryj2q176m9jbipk4v9pvyadfvi1j31b6v95rm1ikpa2y"))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags
      (list (string-append "--with-libxml="
                           (assoc-ref %build-inputs "libxml2"))
            (string-append "--with-curl="
                           (assoc-ref %build-inputs "curl"))
            (string-append "--with-lua="
                           (assoc-ref %build-inputs "lua"))
            (string-append "--with-pcre="
                           (assoc-ref %build-inputs "pcre:bin")))))
   (native-inputs
    `(("autoconfig" ,autoconf)
      ("automake" ,automake)
      ("libtool" ,libtool)))
   (inputs
    `(("curl" ,curl)
      ("lua" ,lua)
      ("pcre:bin" ,pcre "bin")
      ("libxml2" ,libxml2)))
   (synopsis "ModSecurity v3 library component.")
   (description "Libmodsecurity is one component of the ModSecurity v3 project.
 The library codebase serves as an interface to ModSecurity Connectors taking in
web traffic and applying traditional ModSecurity processing. In general, it
provides the capability to load/interpret rules written in the ModSecurity
SecRules format and apply them to HTTP content provided by your application
via Connectors.")
   (license license:asl2.0)
   (home-page "https://www.modsecurity.org/")))
