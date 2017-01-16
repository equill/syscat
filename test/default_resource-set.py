#!/usr/bin/python

import requests

URL='http://localhost:4950/'
REST='%sapi/v1' % URL

# Us
requests.post('%s/asn' % REST , data={'uid': 'default', 'name': 'Default ASN (us)'})

# The Unknown / the Other
requests.post('%s/asn' % REST , data={'uid': 'unknown', 'name': 'Somebody else'})
requests.post('%s/devices' % REST, data={'uid': 'unknown'})
requests.post('%s/devices/unknown/Interfaces/' % REST, data={'type': 'interfaces', 'uid': 'unknown'})
requests.post('%s/devices/unknown/MemberOf/' % REST, data={'target': '/asn/unknown'})

# The Internet
requests.post('%s/asn' % REST , data={'uid': 'internet', 'name': 'The Internet'})
requests.post('%s/asn/internet/subnets' % REST, data={'type': 'ipv4Addresses', 'uid': '0.0.0.0'})

# Generic stuff
requests.post('%s/deviceRoles' % REST, data={'uid': 'router'})
requests.post('%s/deviceRoles' % REST, data={'uid': 'switch'})
requests.post('%s/deviceRoles' % REST, data={'uid': 'host'})
