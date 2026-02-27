;;;; cl-ntp-client.asd
;;;;
;;;; Copyright (c) 2017-2026 Eugene Zaikonnikov

(asdf:defsystem #:cl-ntp-client
  :description "A simple NTP (Network Time Protocol) client in Common Lisp"
  :author "Eugene Zaikonnikov"
  :license "BSD"
  :depends-on (#:alexandria
               #:usocket)
  :in-order-to ((test-op (test-op "cl-ntp-client-test")))
  :serial t
  :components ((:file "package")
               (:file "cl-ntp-client")))
