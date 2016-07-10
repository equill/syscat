#!/usr/bin/python3

# Test package for Syscat's REST API.
#
# Written in Python because that's the language I expect most users to implement
# their code in.
# Using Python3 because it's about time we all moved forward, and Docker means
# we no longer need to stick with obsolete stuff.


# Included batteries
import unittest

# Third-party libraries
import requests


# Config variables
PROTOCOL = 'http'
SERVER_URL = 'localhost:4949'
PREFIX = '/v1'

BASE_URL = '%s://%s%s' % (PROTOCOL, SERVER_URL, PREFIX)


# Tests

class TestIPAM(unittest.TestCase):
    test_address = '127.0.0.5'
    def test_create_and_delete_ipv4_address(self):
        self.assertEqual(requests.get('%s/ipv4-addresses/%s' % (BASE_URL, self.test_address)).json(), {})
        self.assertEqual(
                requests.post('%s/ipv4-addresses/' % (BASE_URL), data = {'address': self.test_address}).text,
                '%s/ipv4-addresses/%s' % (BASE_URL, self.test_address)
                )
        self.assertEqual(
                requests.get('%s/ipv4-addresses/%s' % (BASE_URL, self.test_address)).json(),
                {'address': self.test_address}
                )
        self.assertEqual(
                requests.delete('%s/ipv4-addresses/%s' % (BASE_URL, self.test_address), data = {'address': self.test_address}).text,
                'Success')
        self.assertEqual(requests.get('%s/ipv4-addresses/%s' % (BASE_URL, self.test_address)).text, '{}')


# Make it happen

if __name__ == '__main__':
    unittest.main()
