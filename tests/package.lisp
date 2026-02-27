;;;; tests/package.lisp
;;;;
;;;; Copyright (c) 2017-2026 Eugene Zaikonnikov

(defpackage #:cl-ntp-client-test
  (:use #:cl #:rove)
  (:import-from #:cl-ntp-client
                #:ntp
                #:synchronize
                #:local-stratum
                #:ntp-server-timeout-error))
