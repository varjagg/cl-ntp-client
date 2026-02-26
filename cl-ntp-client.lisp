;;;; A simple Common Lisp NTP client
;;;; Should provide a way to get real time with decent fractional precision in a Lisp program, assuming you re-adjust periodically
;;;; Does not adjust system clock, PLL drift or do any other platform specific work
;;;; Nor does any statistical evaluation of time source or system clock jitter
;;;; Copyright (c) 2017-2026 Eugene Zaikonnikov

(in-package #:cl-ntp-client)

(alexandria:define-constant +millis+ 1000)
(alexandria:define-constant +micros+ 1000000)
(alexandria:define-constant +nanos+  1000000000)

;;; We don't define any specialized arithmetic for NTP timestamps but rathter provide integer conversions
;;; and rely on Common Lisp bignum arithmetic to do the job
(defmacro big-time (values)
  "Convert the NTP second/fraction value pair into a (large) integer"
  (alexandria:with-gensyms (s f)
    `(multiple-value-bind (,s ,f) ,values
       (+ (ash ,s 32) ,f))))

(defmacro small-time (time)
  "Convert the integer time representation back into NTP value pair domain"
  `(values (ash ,time -32) (logand ,time #xffffffff)))

(defclass ntp ()
  ((buffer :reader buffer :initform (make-array 48 :element-type '(unsigned-byte 8) :initial-element 0)
	   :type (simple-array (unsigned-byte 8) (48)))
   (offset :accessor offset :type integer :initform (- (big-time (values (get-universal-time) 0))
							(real-big-time)))
   (local-stratum :accessor local-stratum :type integer :initform 8)
   (address :accessor ntp-address :initform :ntp-address)))

(define-condition ntp-server-timeout-error (error)
  ((address :initarg :address :reader ntp-server-timeout-address))
  (:report (lambda (condition stream)
	     (format stream "Timed out waiting for NTP reply from ~A"
		     (ntp-server-timeout-address condition)))))

(defun read32 (array pos)
  (logior (ash (aref array pos) 24)
	  (ash (aref array (1+ pos)) 16)
	  (ash (aref array (+ pos 2)) 8)
	  (aref array (+ pos 3))))

(defun write32 (array pos val)
  (setf (aref array pos) (ldb (byte 8 24) val)
	(aref array (1+ pos)) (ldb (byte 8 16) val)
	(aref array (+ pos 2)) (ldb (byte 8 8) val)
	(aref array (+ pos 3)) (ldb (byte 8 0) val)))

(defmethod leap-indicator ((o ntp))
  (ldb (byte 2 6) (aref (buffer o) 0)))

(defmethod version-number ((o ntp))
  (ldb (byte 3 3) (aref (buffer o) 0)))

(defmethod (setf version-number) (new-value (o ntp))
  (setf (ldb (byte 3 3) (aref (buffer o) 0)) new-value))

(defmethod mode ((o ntp))
  (ldb (byte 3 0) (aref (buffer o) 0)))

(defmethod (setf mode) (new-value (o ntp))
  (setf (ldb (byte 3 0) (aref (buffer o) 0)) new-value))

(defmethod stratum ((o ntp))
  (aref (buffer o) 1))

(defmethod poll ((o ntp))
  (aref (buffer o) 2))

(defmethod precision ((o ntp))
  (aref (buffer o) 3))

(defmethod root-delay ((o ntp))
  (read32 (buffer o) 4))

(defmethod root-dispersion ((o ntp))
  (read32 (buffer o) 8))

(defmethod ref-id ((o ntp))
  (read32 (buffer o) 12))

(defmethod reftm-s ((o ntp))
  (read32 (buffer o) 16))

(defmethod reftm-f ((o ntp))
  (read32 (buffer o) 20))

(defmethod origtm-s ((o ntp))
  (read32 (buffer o) 24))

(defmethod (setf origtm-s) (stamp (o ntp))
  (write32 (buffer o) 24 stamp))

(defmethod origtm-f ((o ntp))
  (read32 (buffer o) 28))

(defmethod (setf origtm-f) (stamp (o ntp))
  (write32 (buffer o) 28 stamp))

(defmethod rxtm-s ((o ntp))
  (read32 (buffer o) 32))

(defmethod rxtm-f ((o ntp))
  (read32 (buffer o) 36))

(defmethod txtm-s ((o ntp))
  (read32 (buffer o) 40))

(defmethod (setf txtm-s) (stamp (o ntp))
  (write32 (buffer o) 40 stamp))

(defmethod txtm-f ((o ntp))
  (read32 (buffer o) 44))

(defmethod (setf txtm-f) (stamp (o ntp))
  (write32 (buffer o) 44 stamp))

(defun fraction-to-internal (fraction)
  (ash (* fraction internal-time-units-per-second) -32))

(defun sub-internal (fraction)
  "Returns the remainder of ntp value less than internal-time-units-per-second"
  (mod fraction internal-time-units-per-second))

(defun from-fraction (fraction unit)
  (ash (* fraction unit) -32))

(defun fraction-to-usec (fraction)
  "Fixed point conversion to microseconds"
  (- (ash fraction -12) (* 759 (ash (+ (ash fraction -10) 32768) -16))))

(defun usec-to-fraction (usec)
  "Fixed point conversion from microseconds"
  (+ (* 4294 usec) (ash (* 1981 usec) -11) (ash (* 2911 usec) -28)))

(defun to-fraction (time divisor-unit)
  (truncate (ash time 32) divisor-unit))

(defun delay-to-usec (delay)
  "Scaled delay to microseconds conversion"
  (* 15.2587890625 delay))

(defun fraction-to-seconds (fraction)
  (/ (fraction-to-usec fraction) +micros+))

(defun seconds-to-fraction (seconds)
  (usec-to-fraction (round (* seconds +micros+))))

(defun real-big-time ()
  #+sbcl(multiple-value-bind (truth sec usec)
	    (sb-unix:unix-gettimeofday) ;;non-monotonic clock, but lacking alternatives..
	  (declare (ignore truth))
	  (big-time (values sec (usec-to-fraction usec))))
  #+ccl(let ((time (ccl:current-time-in-nanoseconds)))
	 (big-time (values (truncate time +nanos+)
			   (to-fraction (rem time +nanos+) +nanos+))))
  #-(or sbcl ccl)(let* ((time (get-internal-real-time))
	       (seconds (truncate time internal-time-units-per-second))
	       (fractions (- time (* seconds internal-time-units-per-second))))
	  (big-time (values seconds (to-fraction fractions internal-time-units-per-second)))))

(defmethod adjusted-big-time ((o ntp))
  (+ (offset o) (real-big-time)))

(defmethod get-adjusted-universal-time ((o ntp))
  (small-time (adjusted-big-time o)))

(defmethod run-server-exchange ((o ntp) address)
  "Communicates with remote server to return time offset from the local clock"
  (let* ((timeout 2)
	 (socket (usocket:socket-connect address 123 :protocol :datagram
						     :element-type '(unsigned-byte 8)
						     :timeout timeout))
	 (dgram-length (length (buffer o))))
    (fill (buffer o) 0)
    (setf (ntp-address o) address
	  (version-number o) 3
	  (mode o) 3)
    (unwind-protect
	 (multiple-value-bind (seconds fraction) (get-adjusted-universal-time o)
	   (setf (txtm-s o) seconds
		 (txtm-f o) fraction)
	   (usocket:socket-send socket (buffer o) dgram-length)
	   (unless (usocket:wait-for-input socket :timeout timeout :ready-only t)
	     (error 'ntp-server-timeout-error :address address))
	   (usocket:socket-receive socket (buffer o) dgram-length)
	   (let* ((receive-stamp (adjusted-big-time o))
		  (transmit-time (big-time (values (txtm-s o) (txtm-f o))))
		  (elapsed (- receive-stamp (big-time (values seconds fraction))))
		  (stall (- transmit-time (big-time (values (rxtm-s o) (rxtm-f o)))))
		  (delay (floor (- elapsed stall) 2))
		  (deduced (+ transmit-time delay))
		  (delta (- deduced receive-stamp)))
	     (setf (local-stratum o) (1+ (stratum o)))
	     delta))
      (usocket:socket-close socket))))

(defmethod synchronize ((o ntp) &optional (server "pool.ntp.org"))
  (incf (offset o) (run-server-exchange o server)))
