;;; Copyright © 2024 Justin Bishop <mail@dissoc.me>

(define-module (dissoc gnu packages python-xzy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
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

(define-public pygatt
  (package
   (name "pygatt")
   (version "4.0.5")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/peplin/pygatt.git")
           (commit (string-append "v" version))))
     (sha256
      (base32 "1zdfxidiw0l8n498sy0l33n90lz49n25x889cx6jamjr7frlcihd"))))
   (build-system python-build-system)
   (arguments `(#:tests? #f
                #:phases
                (modify-phases
                 %standard-phases
                 (add-before 'build 'patch-setup-py
                             (lambda _
                               (substitute* "setup.py"
                                            ;; no longer need enum-compat
                                            ;; compatible module is  included with python 3.8.2
                                            (("'enum-compat'") "#'enum-compat'")))))))
   (inputs
    `(("python-nose" ,python-nose)
      ("python-coverage" ,python-coverage)))
   (home-page "https://github.com/peplin/pygatt")
   (synopsis "Python wrapper for gatttool")
   (description
    "The module allows reading and writing to GATT descriptors on devices such
as fitness trackers, sensors, and anything implementing standard GATT Descriptor
behavior.")
   (license license:asl2.0)))

(define-public gatt-python
  (package
   (name "gatt-python")
   (version "0.2.6")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/getsenic/gatt-python.git")
           (commit version)))
     (sha256
      (base32 "140790w0z0inxcpyhy5h6n4s30s2lbi0gjdnhlyswhr3v91ymhhq"))))
   (build-system python-build-system)
   (arguments `(#:tests? #f)) ; fails to import test modules
   (native-inputs
    `(("python-dbus" ,python-dbus)))
   (home-page "https://github.com/getsenic/gatt-python")
   (synopsis "Bluetooth GATT SDK for Python")
   (description
    "The Bluetooth GATT SDK for Python helps you implementing and communicating
with any Bluetooth Low Energy device that has a GATT profile.")
   (license license:expat)))
