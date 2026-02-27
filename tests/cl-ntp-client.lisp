;;;; tests/cl-ntp-client.lisp
;;;;
;;;; Copyright (c) 2017-2026 Eugene Zaikonnikov

(in-package #:cl-ntp-client-test)

(deftest synchronize-against-github-pool
  (let ((client (make-instance 'ntp)))
    (ok (integerp (synchronize client "github.pool.ntp.org")))
    (ok (<= 1 (local-stratum client) 16))))

(deftest synchronize-timeout-on-unreachable-host
  (let ((client (make-instance 'ntp)))
    (ok (signals (synchronize client "192.0.2.1")
                 'ntp-server-timeout-error))))
