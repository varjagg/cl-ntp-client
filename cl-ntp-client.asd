;;;; cl-ntp-client.asd
;;;;
;;;; Copyright (c) 2017 Eugene Zaikonnikov

(asdf:defsystem #:cl-ntp-client
  :description "A simple NTP (Network Time Protocol) client in Common Lisp"
  :author "Eugene Zaikonnikov"
  :license "BSD"
  :depends-on (#:alexandria
               #:usocket)
  :serial t
  :components ((:file "package")
               (:file "cl-ntp-client")))

