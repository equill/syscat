;;;; Functions for converting IPv4 and IPv6 addresses
;;;; to/from formats convenient for storage and comparison
;;;;
;;;; Performance is likely to be abysmal at this point.
;;;; This is at the "get it working correctly" stage; if and when this is shown
;;;; to be a performance bottleneck, then I'll invest time into optimising it.
;;;; That said, I'm happy to accept any patches that make it more efficient
;;;; without impairing correctness.

(in-package #:syscat)

;;; For IPv4 and IPv6 addresses, I'm using the sb-bsd-sockets conversions
;;; to vectors of unsigned bytes, largely because it's already written by
;;; somebody smarter than me, and it's close enough to a format useful for
;;; comparisons.
;;;
;;; Portability would be nice, but a)I'm writing this for myself, and SBCL is my
;;; platform of choice, and b)the nature of graph databases is such that
;;; migrating to another format is feasible if required later.
(defun canonicalise-ipv4-addr (address)
  "Ensure that the IPv4 address we're handling is in the canonical format."
  (vector-to-dotted-quad
    (sb-bsd-sockets:make-inet-address address)))

(defun vector-to-dotted-quad (vec)
  "Prints a vector of unsigned octets to a dotted-quad IPv4 address"
  (format nil "~d.~d.~d.~d" (elt vec 0) (elt vec 1) (elt vec 2) (elt vec 3)))

(defun reformat-hex-pair (h1 h2)
  "Takes two unsigned octets, combines them into a two-byte hexadecimal number, and formats them without the leading zero."
  (format nil "~x" (parse-integer (format nil "~x~2,'0x" h1 h2) :radix 16)))

(defun vector-to-standard-ipv6 (vec)
  "Prints a vector of unsigned octets to a standard-format representation of an IPv6 address.
   This version does not collapse consecutive sections of zeroes, but does omit leading zeroes."
  (let ((s1 (reformat-hex-pair (elt vec 0) (elt vec 1)))
        (s2 (reformat-hex-pair (elt vec 2) (elt vec 3)))
        (s3 (reformat-hex-pair (elt vec 4) (elt vec 5)))
        (s4 (reformat-hex-pair (elt vec 6) (elt vec 7)))
        (s5 (reformat-hex-pair (elt vec 8) (elt vec 9)))
        (s6 (reformat-hex-pair (elt vec 10) (elt vec 11)))
        (s7 (reformat-hex-pair (elt vec 12) (elt vec 13)))
        (s8 (reformat-hex-pair (elt vec 14) (elt vec 15))))
    ;; The ~(...~) forces the alphabetic characters to lower-case.
    ;; Nicer to read, and more like what people expect.
    (format nil "~(~x~):~(~x~):~(~x~):~(~x~):~(~x~):~(~x~):~(~x~):~(~x~)"
            s1 s2 s3 s4 s5 s6 s7 s8)))
