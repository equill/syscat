#!/usr/bin/python3

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

"""
Installs a default set of resources.
Assumes that the standard schema has already been applied.
"""


# Third-party modules
import requests

# Build-in modules
import re


# Config variables
PROTOCOL = 'http'
SERVER_URL = 'localhost:4950'
RAWPREFIX = 'raw/v1'
IPAMPREFIX = 'ipam/v1'

BASE_URL = '%s://%s' % (PROTOCOL, SERVER_URL)


# Utilities

def post_raw(uri, data):
    """
    Post data to Syscat's raw API, and report the result to STDOUT for feedback to the user
    """
    response = requests.post('%s/%s%s' % (BASE_URL, RAWPREFIX, uri), data=data)
    print('%s: %s' % (uri, response.status_code))

def post_ipam(uri, data):
    """
    Post data to Syscat's IPAM API, and report the result to STDOUT for feedback to the user
    """
    response = requests.post('%s/%s/%s' % (BASE_URL, IPAMPREFIX, uri), data=data)
    print('%s: %s' % (uri, response.status_code))

def sanitise_uid(uid):
    '''
    Sanitise a UID string in the same way Restagraph does
    '''
    return re.sub('[/ ]', '_', uid)


def insert_model():
    "Insert the actual model into Syscat."
    # Organisations
    #
    post_raw('/organisations', data={'uid': 'ICANN', 'comments': 'The Internet Corporation for Assigned Names and Numbers. They manage DNS at its top level, and allocate AS numbers for use in BGP.'})
    post_raw('/organisations', data={'uid': 'IANA', 'comments': 'The Internet Assigned Numbers Authority. They allocate new subnets  and IP addresses to other organisations.'})
    post_raw('/organisations', data={'uid': 'Internet', 'comments': 'A notional entity representing the Terra Incognita outside the detailed areas of this model'})

    # ASNs
    # Associate them with their owning organisations - in both directions.
    # Why both directions? To enable search in both directions via this API.
    # Note that the UIDs for the organisations have been canonicalised,
    # with underscores replacing the original spaces.
    #
    post_raw('/asns', data={'uid': '0', 'comments': "The Internet."})

    # IPAM
    #
    # The internet at large
    post_ipam('subnets', data={'organisations': 'Internet', 'subnet': '0.0.0.0/0'})

if __name__ == '__main__':
    insert_model()
