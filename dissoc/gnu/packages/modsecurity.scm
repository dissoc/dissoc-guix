;;; Copyright © 2024 Justin Bishop <mail@dissoc.me>

(define-module (dissoc gnu packages modsecurity)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
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

(define-public modsecurity-nginx
  (package
   (name "modsecurity-nginx")
   (version "1.0.3")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/SpiderLabs/ModSecurity-nginx.git")
                  (commit (string-append "v" version))))
            (sha256
             (base32
              "0cbb3g3g4v6q5zc6an212ia5kjjad62bidnkm8b70i4qv1615pzf"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases
       %standard-phases
       (replace 'unpack
                (lambda* (#:key source #:allow-other-keys)
                  (display "source")
                  (display source)
                  (if (file-is-directory? source)
                      (begin
                        (mkdir "source")
                        (chdir "source")
                        (mkdir "modsecurity-nginx")
                        (copy-recursively source "modsecurity-nginx"
                                          #:keep-mtime? #t)))))
       (add-after 'unpack 'unpack-nginx-sources
                  (lambda* (#:key inputs native-inputs #:allow-other-keys)
                    (begin
                      ;; The nginx source code is part of the module’s source.
                      (format #t "decompressing nginx source code~%")
                      (let ((tar (assoc-ref inputs "tar"))
                            (nginx-srcs (assoc-ref inputs "nginx-sources")))
                        (invoke (string-append tar "/bin/tar")
                                "xvf" nginx-srcs "--strip-components=1")))))
       (add-before 'configure 'patch-/bin/sh
                   (lambda _
                     (substitute* "auto/feature"
                                  (("/bin/sh") (which "sh")))))
       (replace 'configure
                ;; The configure script is hand-written, not from GNU autotools.
                (lambda* (#:key configure-flags outputs #:allow-other-keys)
                  (let ((flags
                         (append (list (string-append
                                        "--prefix="
                                        (assoc-ref outputs "out"))
                                       "--with-http_ssl_module"
                                       "--with-http_v2_module"
                                       "--with-pcre-jit"
                                       "--with-debug"
                                       "--add-module=./modsecurity-nginx")
                                 configure-flags)))
                    (setenv "CC" "gcc")
                    (format #t "configure flags: ~s~%" flags)
                    (apply invoke "./configure" flags))))
       (delete 'check)
       (add-after 'install 'install-man-page
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (man (string-append out "/share/man")))
                      (install-file "objs/nginx.8" (string-append man "/man8")))))
       (add-after 'install 'fix-root-dirs
                  (lambda* (#:key outputs #:allow-other-keys)
                    ;; 'make install' puts things in strange places, so we need to
                    ;; clean it up ourselves.
                    (let* ((out (assoc-ref outputs "out"))
                           (share (string-append out "/share/nginx")))
                      ;; This directory is empty, so get rid of it.
                      (rmdir (string-append out "/logs"))
                      ;; Example configuration and HTML files belong in
                      ;; /share.
                      (mkdir-p share)
                      (rename-file (string-append out "/conf")
                                   (string-append share "/conf"))
                      (rename-file (string-append out "/html")
                                   (string-append share "/html"))))))))
   (native-inputs
    `(("autoconfig" ,autoconf)
      ("automake" ,automake)
      ("nginx" ,nginx)
      ("libtool" ,libtool)))
   (inputs
    `(("libmodsecurity" ,libmodsecurity)
      ("pcre" ,pcre)
      ("openssl" ,openssl)
      ("zlib" ,zlib)
      ("nginx-sources" ,(package-source nginx))))
   (synopsis "ModSecurity v3 library component.")
   (description "Libmodsecurity is one component of the ModSecurity v3 project. The
      library codebase serves as an interface to ModSecurity Connectors taking
      in web traffic and applying traditional ModSecurity processing. In
      general, it provides the capability to load/interpret rules written in
      the ModSecurity SecRules format and apply them to HTTP content provided
      by your application via Connectors.")
   (license (list license:bsd-2
                  license:expat
                  license:bsd-3
                  license:bsd-4
                  license:asl2.0))
   (home-page "https://www.modsecurity.org/")))
