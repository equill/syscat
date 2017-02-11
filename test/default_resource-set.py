#!/usr/bin/python

import requests

URL='http://localhost:4950'
REST='%s/raw/v1' % URL

def post(uri, data):
    response = requests.post('%s/%s' % (REST, uri), data=data)
    print '%s: %s' % (uri, response.status_code)

# Us
post('asn', {'uid': 'default', 'name': 'Default ASN (us)'})
post('organisations', {'uid': 'us', 'description': 'Our own organisation.'})

# The Unknown / the Other
post('devices', {'uid': 'unknown'})
post('devices/unknown/Interfaces/', {'type': 'interfaces', 'uid': 'unknown'})
post('organisations', {'uid': 'unknown', 'description': 'Unknown organisation. Not us.'})
post('devices/unknown/BusinessOwner/', {'target': '/organisations/unknown'})

# The Internet
post('asn', {'uid': 'internet', 'name': 'The Internet'})
post('asn/internet/Subnets', {'type': 'ipv4Subnets', 'uid': '0.0.0.0/0'})

# Generic stuff
#
post('deviceTypes', {'uid': 'physical'})
post('deviceTypes', {'uid': 'cluster'})
post('deviceTypes', {'uid': 'virtual'})
#
post('deviceRoles', {'uid': 'router'})
post('deviceRoles', {'uid': 'switch'})
post('deviceRoles', {'uid': 'host'})
