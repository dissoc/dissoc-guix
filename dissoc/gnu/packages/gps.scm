;;; Copyright © 2024 Justin Bishop <mail@dissoc.me>

(define-module (dissoc gnu packages gps)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public shapelib
  (package
   (name "shapelib")
   (version "1.6.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference (url "https://github.com/OSGeo/shapelib.git")
                                (commit (string-append "v" version))))
            (sha256
             (base32
              "07v1apckjqv5kjdikicm7n0x6qyrlk94ksh8ihfq4rn3ym8ayhy3"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("libtool" ,libtool)))
   (synopsis "C Library for reading, writing and updating ESRI Shapefiles")
   (description "The Shapefile C Library provides the ability to write simple C
programs for reading, writing and updating (to a limited extent) ESRI
Shapefiles, and the associated attribute file (.dbf).")
   (home-page "http://shapelib.maptools.org/")
   (license (list license:lgpl2.0 license:expat))))
