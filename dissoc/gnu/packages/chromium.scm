;;; Copyright © 2024 Justin Bishop <mail@dissoc.me>

(define-module (dissoc gnu packages chromium)
  #:use-module (gnu packages chromium)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public ungoogled-chromium/custom
  (package
   (inherit ungoogled-chromium)
   (name "ungoogled-chromium-custom")
   (arguments
    (substitute-keyword-arguments
     (package-arguments ungoogled-chromium)
     ((#:phases phases)
      #~(modify-phases
         #$phases
         (add-after 'add-absolute-references 'patch-undetectable
                    (lambda _
                      (substitute*
                       "third_party/blink/renderer/core/frame/navigator.h"
                       (("return true;")
                        "return false;"))
                      (substitute*
                       "third_party/blink/renderer/core/frame/navigator.idl"
                       (("Navigator includes NavigatorAutomationInformation;")
                        "//Navigator includes NavigatorAutomationInformation;"))
                      (substitute*
                       "chrome/test/chromedriver/chrome/devtools_client_impl.cc"
                       (("cdc_adoQpoasnfa76pfcZLmcfl")
                        "somethingsomething"))
                      (substitute*
                       "chrome/test/chromedriver/js/test.js"
                       (("cdc_adoQpoasnfa76pfcZLmcfl")
                        "somethingsomething"))
                      (substitute*
                       "chrome/test/chromedriver/js/execute_async_script.js"
                       (("cdc_adoQpoasnfa76pfcZLmcfl")
                        "somethingsomething"))
                      (substitute*
                       "chrome/test/chromedriver/js/execute_script.js"
                       (("cdc_adoQpoasnfa76pfcZLmcfl")
                        "somethingsomething"))
                      (substitute*
                       "chrome/test/chromedriver/js/call_function.js"
                       (("cdc_adoQpoasnfa76pfcZLmcfl")
                        "somethingsomething"))))))))))
