;;;; cl-ntp-client-test.asd
;;;;
;;;; Copyright (c) 2017-2026 Eugene Zaikonnikov

(asdf:defsystem #:cl-ntp-client-test
  :description "Test suite for cl-ntp-client"
  :author "Eugene Zaikonnikov"
  :license "BSD"
  :depends-on (#:cl-ntp-client
               #:rove)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "cl-ntp-client"))))
  :perform (test-op (op c)
             (declare (ignore op))
             (uiop:symbol-call :rove :run c)))
