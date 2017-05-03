(in-package #:syscat)

(defun ipv4-subnet-p (subnet)
  "Determine whether the supplied string is a CIDR-formatted IPv4 subnet"
  (cl-ppcre:all-matches "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\/[0-9]{1,2}" subnet))

(defun cidr-to-ints (cidrstr)
  "Converts a CIDR representation of a subnet, e.g. 127.0.0.1/8 into a list of two integers.
  If just given an IP address, e.g. 127.0.0.1, it will auto-assign a prefix-length of 32."
  (let ((parts (cl-ppcre:split "/" cidrstr)))
    (restagraph:log-message
      :debug
      (format nil "cidr-to-ints processing subnet ~A as ~A/~A."
              cidrstr (first parts) (second parts)))
    (list
      (cl-cidr-notation:parse-ip (first parts))
      (if (second parts)
        (parse-integer (second parts) :junk-allowed nil)
        32))))

(defun ipv4-network-address (cidr &key broadcast)
  "Takes a CIDR subnet spec, e.g. 192.168.0.1/24, and returns its network address.
   If :broadcast is specified as non-null, calculates the broadcast address for the subnet."
  (let* ((mask-bit (if broadcast 1 0))
         (address-parts (cl-ppcre:split "/" cidr))
         (address-int (cl-cidr-notation:parse-ip (first address-parts)))
         (prefix-length (parse-integer (second address-parts) :junk-allowed nil)))
    (cond
      ;; Sanity-check
      ((not (and (integerp prefix-length)
                 (>= prefix-length 0)
                 (<= prefix-length 32)))
       (error "Prefix-length must be an integer between 0 and 32"))
      ;; If the prefix-length is 32, the address _is_ the network address
      ((equal prefix-length 32)
       (first address-parts))
      ;; Otherwise, calculate the network address
      (t
       (progn
         (loop for i from 0 to (- 31 prefix-length)
               do (setf (ldb (byte 1 i) address-int) mask-bit))
         (cl-cidr-notation:ip-string
           address-int))))))

(defun parent-ipv4-subnet-p (netaddr1 netaddr2)
  "Checks whether netaddr1 is a supernet of netaddr2, where both are strings in CIDR format.
  Currently only works for IPv4."
  (restagraph:log-message
    :debug
    (format nil "Checking whether ~A is a supernet of ~A." netaddr1 netaddr2))
  (let*
    ((result t)
     (parts1 (cidr-to-ints netaddr1))
     (net1 (first parts1))
     (prefix1 (second parts1))
     (parts2 (cidr-to-ints netaddr2))
     (net2 (first parts2))
     (prefix2 (second parts2)))
    ;; First sanity check: is the second prefix longer than the first?
    (if
      (>= prefix1 prefix2)
      ;; First prefix is longer; reject this and return nil.
      (progn
        (restagraph:log-message
          :debug
          (format nil "Prefix 1 (~A) is not shorter than prefix 2 (~A)" netaddr1 netaddr2))
        (setf result nil))
      ;; Prefixes are OK.
      ;; Now compare each bit from least- to most-significant,
      ;; over the length of the candidate parent's prefix.
      ;; If any of them differ, set `result` to nil.
      (loop for i from (- 32 prefix1) to 31
            do (if (= (ldb (byte 1 i) net1)
                      (ldb (byte 1 i) net2))
                 (restagraph:log-message :debug (format nil "Bit ~D is the same" i))
                 (progn
                   (restagraph:log-message :debug (format nil "Bit ~D differs" i))
                   (setf result nil)))))
    ;; Return the result.
    result))
