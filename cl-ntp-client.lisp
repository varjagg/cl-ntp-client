;;;; A simple Common Lisp NTP client
;;;; Should provide a way to get real time with decent fractional precision in a Lisp program, assuming you re-adjust periodically
;;;; Does not adjust system clock, PLL drift or do any other platform specific work 
;;;; Copyright (c) 2017 Eugene Zaikonnikov

(in-package #:cl-ntp-client)

(alexandria:define-constant +epoch-timestamp-delta+ 2208988800)

(defclass ntp ()
  ((buffer :reader buffer :initform (make-array 48 :element-type '(unsigned-byte 8) :initial-element 0)
	   :type '(simple-array (unsigned-byte 8) (48)))
   (offset-s :accessor offset-s :type 'integer :initform 0)
   (offset-f :accessor offset-f :type 'integer :initform 0)
   (local-stratum :accessor local-stratum :type 'integer :initform 8)))

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

(defmethod txtm-f ((o ntp))
  (read32 (buffer o) 44))

(defun internal-to-fraction (internal)
  (truncate (ash internal 32) internal-time-units-per-second))

(defun fraction-to-internal (fraction)
  (ash (* fraction internal-time-units-per-second) -32))

;;; here we take arbitrary subsecond fraction of internal run time for lack of 'real' universal subsecond
;;; which is fine as long as it's consistent for duraction of program
;;; the calculated offset ensures it adjusted properly
(defmethod get-adjusted-universal-time ((o ntp))
  (values (+ (get-universal-time) (offset-s o))
	  (+ (internal-to-fraction (rem (get-internal-real-time) internal-time-units-per-second))
	     (offset-f o))))

(defmethod run-server-exchange ((o ntp) address)
  "Communicates with remote server to return time offset from the local clock"
  (let ((socket (usocket:socket-connect address 123 :protocol :datagram
					:element-type '(unsigned-byte 8)
					:timeout 2))
	(dgram-length (length (buffer o))))
    (setf (version-number o) 3
	  (mode o) 3)
    (unwind-protect
	 (multiple-value-bind (seconds fraction) (get-adjusted-universal-time o)
	   (setf (origtm-s o) seconds
		 (origtm-f o) fraction)
	   (usocket:socket-send socket (buffer o) dgram-length)
	   (usocket:socket-receive socket (buffer o) dgram-length)
	   (let* ((receive-stamp (big-time (get-adjusted-universal-time o)))
		  (transmit-time (big-time (values (txtm-s o) (txtm-f o))))
		  (delay (floor (- (- receive-stamp (big-time (values seconds fraction)))
				   (- transmit-time (big-time (values (rxtm-s o) (rxtm-f o)))))
				2))
		  (deduced (+ transmit-time delay))
		  (delta (- deduced receive-stamp)))
	     (setf (local-stratum o) (1+ (stratum o)))
	     (small-time delta)))
      (usocket:socket-close socket))))

(defmethod synchronize ((o ntp) &optional (server "time.nist.gov"))
  (multiple-value-bind (ds df) (run-server-exchange o server)
    (incf (offset-s o) ds)
    (incf (offset-f o) df)))
