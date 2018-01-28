#!/usr/bin/env python3

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
import yaml


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

def install_resources(resources):
    '''
    Install a set of resources.
    Expected to be the 'resources' subset of a YAML model.
    '''
    session = requests.Session()
    for resource in resources:
        resourcetype = resource['type']
        details = resource['details']
        response = session.post('{url}/{raw}/{type}'.format(url=BASE_URL,
                                                            raw=RAWPREFIX,
                                                            type=resourcetype),
                                data=details)
        if response.status_code != 201:
            print('ERROR: {code} {text} for /{resourcetype} with details {details}'.format(
                code=response.status_code,
                text=response.text,
                resourcetype=resourcetype,
                details=details))

def read_model(path):
    """
    Read the data model from the specified filepath.
    """
    infile = open(path, 'r')
    model = yaml.load(infile)
    infile.close()
    return model

def main(path):
    model = read_model(path)
    install_resources(model['raw'])

if __name__ == '__main__':
    main('./default_dataset.yaml')
