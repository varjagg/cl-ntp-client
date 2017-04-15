;;;; package.lisp
;;;;
;;;; Copyright (c) 2017 Eugene Zaikonnikov

(defpackage #:cl-ntp-client
  (:use #:cl)
  (:export #:synchronize #:get-adjusted-universal-time #:local-stratum #:small-time #:big-time
	   #:adjusted-big-time #:ntp #:fraction-to-internal #:internal-to-fraction))

