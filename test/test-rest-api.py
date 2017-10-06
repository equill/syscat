#!/usr/bin/python3

# Test package for Syscat's REST API.


#   Copyright 2017 James Fleming <james@electronic-quill.net>
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.


# Included batteries
import unittest
import re

# Third-party libraries
import requests


# Config variables
PROTOCOL = 'http'
SERVER_URL = 'localhost:4950'
RAWPREFIX = 'raw/v1'
IPAMPREFIX = 'ipam/v1'

BASE_URL = '%s://%s' % (PROTOCOL, SERVER_URL)


# Utilities
def sanitise_uid(uid):
    '''
    Sanitise a UID string in the same way Restagraph does
    '''
    return re.sub('[/ ]', '_', uid)


# Tests

class TestSubnetsBasicNoVrf(unittest.TestCase):
    '''
    Basic CRD functions for subnets
    '''
    asn = 'testco'
    subnet1 = '172.16.0.0/12'
    def test_create_and_delete_single_subnet(self):
        print('Test: test_create_and_delete_single_subnet')
        # Create the ASN that it goes under
        self.assertEqual(requests.post('%s/%s/asns' % (BASE_URL, RAWPREFIX), data={'uid': self.asn}).status_code, 201)
        # Add the subnet
        self.assertEqual(requests.post('%s/%s/subnets' % (BASE_URL, IPAMPREFIX), data={'asns': self.asn, 'subnet': self.subnet1}).status_code, 201)
        # Confirm it's there
        self.assertEqual(requests.get('%s/%s/subnets?subnet=%s&asns=%s' % (BASE_URL, IPAMPREFIX, self.subnet1, self.asn)).status_code, 200)
        # Delete it
        self.assertEqual(requests.delete('%s/%s/subnets' % (BASE_URL, IPAMPREFIX), data={'asns': self.asn, 'subnet': self.subnet1}).status_code, 204)
        # Ensure it's gone
        self.assertEqual(requests.get('%s/%s/subnets?subnet=%s&asns=%s' % (BASE_URL, IPAMPREFIX, self.subnet1, self.asn)).status_code, 404)
        # Remove the ASN
        self.assertEqual(requests.delete('%s/%s/asns/%s' % (BASE_URL, RAWPREFIX, self.asn)).status_code, 204)

class TestIpv4AddressesBasicNoVrf(unittest.TestCase):
    '''
    Basic CRD functions for Ipv4 addresses
    '''
    asn = 'testco'
    subnet1 = '172.16.0.0/12'
    address1 = '172.16.23.4'
    def test_create_and_delete_single_address(self):
        print('Test: test_create_and_delete_single_address')
        # Create the ASN that it goes under
        self.assertEqual(requests.post('%s/%s/asns' % (BASE_URL, RAWPREFIX), data={'uid': self.asn}).status_code, 201)
        # Add the subnet
        self.assertEqual(requests.post('%s/%s/subnets' % (BASE_URL, IPAMPREFIX), data={'asns': self.asn, 'subnet': self.subnet1}).status_code, 201)
        # Add the address
        self.assertEqual(requests.post('%s/%s/addresses' % (BASE_URL, IPAMPREFIX), data={'asns': self.asn, 'address': self.address1}).status_code, 201)
        # Confirm it's there
        self.assertEqual(requests.get('%s/%s/addresses?address=%s&asns=%s' % (BASE_URL, IPAMPREFIX, self.address1, self.asn)).status_code, 200)
        # Delete it
        self.assertEqual(requests.delete('%s/%s/addresses' % (BASE_URL, IPAMPREFIX), data={'asns': self.asn, 'address': self.address1}).status_code, 204)
        # Ensure it's gone
        self.assertEqual(requests.get('%s/%s/addresses?address=%s&asns=%s' % (BASE_URL, IPAMPREFIX, self.address1, self.asn)).status_code, 404)
        # Remove the ASN and subnet
        self.assertEqual(requests.delete('%s/%s/asns/%s' % (BASE_URL, RAWPREFIX, self.asn), data={'recursive': 'true'}).status_code, 204)


# Make it happen
if __name__ == '__main__':
    unittest.main()
