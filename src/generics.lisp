(in-package #:syscat)

(defgeneric find-subnet (db org vrf subnet &optional path)
  (:documentation "Try to locate a subnet, given the organisation and VRF to check under.
                   If VRF is NIL, assume the subnet is in the default VRF and thus directly attached to the organisation."))

(defgeneric find-parent-subnet (db subnet org vrfgroup path)
  (:documentation "Find the subnet with the longest prefix-length that could plausibly be a parent subnet to the one supplied."))

(defgeneric insert-subnet (db org vrf subnet)
  (:documentation "Insert a subnet under the specified organisation and VRF.
                   If the vrf argument is the empty string, create it directly, implying the default VRF."))

(defgeneric delete-subnet (db org vrf subnet)
  (:documentation "Remove a subnet from the IPAM section.
                   Merge its subnets and addresses into its subnet.
                   Note: it assumes you got the path right, most likely via find-subnet.
                   Note: it silently deletes and other kinds of relationship between the target subnet and other resources - if you want those reassigned, you need to do it yourself first.
                   If you want to recursively delete a subnet and everything under it, use restagraph::delete-resource in recursive mode."))

(defgeneric find-ipv4address (db address org vrf)
            (:documentation "Find the path to an IPv4 address in the IPAM section"))

(defgeneric insert-ipv4address (db address org vrf)
            (:documentation "Add an IPv4 address to the IPAM section"))

(defgeneric delete-ipv4address (db address org vrf)
            (:documentation "Remove an IPv4 address from the IPAM section"))
