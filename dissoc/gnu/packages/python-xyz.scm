;;; Copyright © 2024 Justin Bishop <mail@dissoc.me>

(define-module (dissoc gnu packages python-xzy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (guix build utils)
  #:use-module (guix build-system python)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public python-timeago
  (package
   (name "python-timeago")
   (version "1.0.16")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/hustcc/timeago.git")
           (commit version)))
     (sha256
      (base32 "1qzw4imgmqhc34vk4v18ryq5ggc1nx6dm3zwafrkgbhml0j938ry"))))
   (build-system python-build-system)
   (arguments `(#:tests? #f)) ; fails to import test modules
   (home-page "https://github.com/hustcc/timeago")
   (synopsis "Simple library used to format datetime")
   (description
    "A very simple python lib, used to format datetime with *** time ago
statement.")
   (license license:expat)))

(define-public python-pypubsub
  (package
   (name "python-pypubsub")
   (version "4.0.3")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/schollii/pypubsub.git")
           (commit (string-append "v" version))))
     (sha256
      (base32 "02j74w28wzmdvxkk8i561ywjgizjifq3hgcl080yj0rvkd3wivlb"))))
   (build-system python-build-system)
   (arguments `(#:tests? #f)) ; fails to import test modules
   (home-page "https://github.com/schollii/pypubsub")
   (synopsis "A Python publish-subcribe library")
   (description
    "Provides a publish-subscribe API to facilitate event-based or message-based
architecture in a single-process application.")
   (license license:bsd-2)))
