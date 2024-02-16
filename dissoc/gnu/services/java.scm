;;; Copyright © 2024 Justin Bishop <mail@dissoc.me>

(define-module (dissoc gnu services java)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records))

(define-record-type* <jar-configuration>
  jar-configuration make-jar-configuration
  jar-configuration?
  (application-name jar-configuration-application-name
                    (default "jar-application"))
  (maximum-heap-size jar-configuration-maximum-heap-size
                     (default "512m"))
  (initial-heap-size jar-configuration-initial-heap-size
                     (default "64m"))
  ;; (maximum-metaspace-size jar-configuration-maximum-metaspace-size
  ;;                         (default "512m"))
  (jar-file jar-configuration-jar-file)
  (jdk jar-configuration-jdk
       (default openjdk)))

(define (jar-account config)
  "Return the user accounts and user groups for CONFIG."
  (list (user-group (name (jar-configuration-application-name config))
                    (system? #f))
        (user-account
         (name (jar-configuration-application-name config))
         (system? #f)
         (group (jar-configuration-application-name config))
         (comment "jar-service privilege separation user")
         (home-directory (string-append "/var/empty"))
         (shell (file-append shadow "/sbin/nologin")))))

(define (jar-activation config)
  "Set up directories for the jar service"
  #~(begin
      (let ((user (getpw #$(jar-configuration-application-name config)))
            (application-directory
             #$(string-append "/var/lib/"
                              (jar-configuration-application-name config))))
        (mkdir-p application-directory)
        (chown application-directory (passwd:uid user) (passwd:gid user))
        (copy-file #$(jar-configuration-jar-file config)
                   (string-append application-directory
                                  "/"
                                  (string-drop
                                   #$(jar-configuration-jar-file config) 44)))
        (chmod (string-append application-directory
                              "/"
                              (string-drop
                               #$(jar-configuration-jar-file config) 44))
               #o400)
        (chown (string-append application-directory
                              "/"
                              (string-drop
                               #$(jar-configuration-jar-file config) 44))
               (passwd:uid user) (passwd:gid user)))))

(define (jar-shepherd-service config)
  (list (shepherd-service
         (documentation "Runs the jar service")
         (provision (list
                     (symbol-append
                      'jar
                      (string->symbol
                       (string-append
                        "-"
                        (jar-configuration-application-name config))))))
         (start
          #~(make-forkexec-constructor
             (list (string-append #$(jar-configuration-jdk config)  "/bin/java")
                   ;; set the user directory to a path in /var/lib
                   (string-append "-Duser.dir="
                                  "/var/lib/"
                                  #$(jar-configuration-application-name config))
                   "-jar"
                   ;; maximum heap size
                   (string-append
                    "-Xmx"
                    #$(jar-configuration-maximum-heap-size config))
                   ;; initial heap size
                   (string-append
                    "-Xms"
                    #$(jar-configuration-initial-heap-size config))
                   (string-append "/var/lib/"
                                  #$(jar-configuration-application-name config)
                                  "/"
                                  (string-drop
                                   #$(jar-configuration-jar-file config) 44)))
             #:log-file (string-append
                         "/var/log/"
                         #$(jar-configuration-application-name config) ".log")
             #:user  #$(jar-configuration-application-name config)
             #:group #$(jar-configuration-application-name config)))
         (stop #~(make-kill-destructor)))))

(define jar-service-type
  (service-type (name 'jar)
                (description "Runs a jar program")
                (extensions
                 (list
                  (service-extension shepherd-root-service-type
                                     jar-shepherd-service)
                  (service-extension account-service-type
                                     jar-account)
                  (service-extension activation-service-type
                                     jar-activation)))))
