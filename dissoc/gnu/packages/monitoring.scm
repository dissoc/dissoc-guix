;;; Copyright © 2024 Justin Bishop <mail@dissoc.me>

(define-module (dissoc gnu packages monitoring)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages java)
  #:use-module (gnu packages monitoring)
  #:use-module (guix packages)
  #:use-module (guix utils))

;; NOTE: the output contains a startup script that sets properties:
;; -Dzabbix.pidFile=$PID_FILE
;; -Dzabbix.listenIP=$LISTEN_IP
;; -Dzabbix.listenPort=$LISTEN_PORT"
;; -Dzabbix.startPollers=$START_POLLERS"
;; -Dzabbix.timeout=$TIMEOUT
;; -Dzabbix.propertiesFile=$PROPERTIES_FILE
;;
;; to use package with a service it is better to set these
;; properties during the service definition rather than using the
;; startup script by having service configuration set the
;; runtime properties.

;; COMMAND_LINE="$JAVA $JAVA_OPTIONS -classpath $CLASSPATH $ZABBIX_OPTIONS com.zabbix.gateway.JavaGateway"

(define-public zabbix-jmx-agent
  (package
   (inherit zabbix-agentd)
   (name "zabbix-jmx-agent")
   (arguments
    `(#:configure-flags
      (list "--enable-java")
      #:phases
      (modify-phases
       %standard-phases
       (add-after 'install 'patch-java
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (java-bin (string-append (assoc-ref inputs "openjdk:jdk")
                                                    "/bin/java")))
                      (substitute* (string-append out "/sbin/zabbix_java/startup.sh")
                                   (("cat")
                                    (string-append (assoc-ref inputs "coreutils")
                                                   "/bin/cat"))
                                   (("rm -f")
                                    (string-append (assoc-ref inputs "coreutils")
                                                   "/bin/rm -f"))
                                   (("touch")
                                    (string-append (assoc-ref inputs "coreutils")
                                                   "/bin/touch")))
                      (wrap-program (string-append out "/sbin/zabbix_java/startup.sh")
                                    `("JAVA" ":" = (,java-bin)))
                      #t))))))
   (inputs
    `(("openjdk:jdk" ,openjdk "jdk")
      ("coreutils" ,coreutils)))
   (home-page "https://www.zabbix.com/")
   (synopsis "Zabbix jmx agent")
   (description "This package provides a distributed monitoring solution
(client-side agent)")))
