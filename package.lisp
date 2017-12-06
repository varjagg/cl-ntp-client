;;;; package.lisp
;;;;
;;;; Copyright (c) 2017 Eugene Zaikonnikov

(defpackage #:cl-ntp-client
  (:use #:cl)
  (:nicknames #:ntp)
  (:export #:synchronize #:get-adjusted-universal-time #:local-stratum #:small-time #:big-time
	   #:adjusted-big-time #:ntp #:fraction-to-internal #:internal-to-fraction #:to-fraction #:from-fraction
	   #:+millis+ #:+micros+ #:+nanos+ #:ntp-address #:sub-internal #:usec-to-fraction #:fraction-to-usec
	   #:seconds-to-fraction #:fraction-to-seconds))

