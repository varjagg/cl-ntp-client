;;;; cl-ntp-client.lisp
;;;;
;;;; Copyright (c) 2017 Eugene Zaikonnikov

(in-package #:cl-ntp-client)

(define-constant +epoch-timestamp-delta+ 2208988800)

(defclass ntp ()
  ((buffer :reader buffer :initform (make-array 48 :element-type '(unsigned-byte 8)) :type '(simple-array (unsigned-byte 8) (48)))
   (servers :accessor servers :initform '())))

(defun read32 (array pos)
  (logior (ash (aref array pos) 24)
	  (ash (aref array (1+ pos)) 16)
	  (ash (aref array (+ pos 2)) 8)
	  (aref array (+ pos 3))))

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
  (aref (buffer 0) 1))

(defmethod (setf stratum) (stratum (o ntp))
  (setf (aref (buffer 0) 1) stratum))

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

(defmethod origtm-f ((o ntp))
  (read32 (buffer o) 28))

(defmethod rxtm-s ((o ntp))
  (read32 (buffer o) 32))

(defmethod rxtm-f ((o ntp))
  (read32 (buffer o) 36))

(defmethod txtm-s ((o ntp))
  (read32 (buffer o) 40))

(defmethod txtm-f ((o ntp))
  (read32 (buffer o) 44))

(defmethod initialize-instance :after ((o ntp) &key &allow-other-keys)
  (setf (version-number o) 3
	(mode o) 3))

(defmethod get-adjusted-universal-time ((o ntp))
  (values (+ (get-universal-time) (offset-s o)) (offset-f o)))

(defmethod run-server-exchange ((o ntp) address)
  (multiple-value-bind (seconds fraction) (get-adjusted-universal-time o)
    ))

(defmethod synchronize ((o ntp))
  (loop for server in (servers o) do
       (run-server-exchange o server)))
