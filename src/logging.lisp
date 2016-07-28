;;;; Logging infrastructure
;;; We only need something extremely simple

(in-package #:syscat)

(defvar *loglevels*
  '(:crit 4
    :error 3
    :warn 2
    :info 1
    :debug 0))

;; Set the threshold logging level
(defparameter *loglevel* :debug)

(defparameter *log-stream*
  (make-synonym-stream 'cl:*standard-output*))

(defun make-timestamp ()
  (multiple-value-bind (sec minute hour date month year)
    (get-decoded-time)
    (format nil "[~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d]"
    year month date hour minute sec)))

(defun log-message (severity &rest args)
  (when (>= (getf *loglevels* severity)
            (getf *loglevels* *loglevel*))
    (format *log-stream* "~A ~A ~A~%"
            (make-timestamp)
            severity
            (apply #'format (append '(()) args)))))
