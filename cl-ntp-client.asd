;;;; cl-ntp-client.asd
;;;;
;;;; Copyright (c) 2017 Eugene Zaikonnikov

(asdf:defsystem #:cl-ntp-client
  :description "Describe cl-ntp-client here"
  :author "Eugene Zaikonnikov"
  :license "BSD"
  :depends-on (#:alexandria
               #:usocket)
  :serial t
  :components ((:file "package")
               (:file "cl-ntp-client")))

