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
Set up the Syscat schema in the database, via the REST API.
"""


# Third-party modules
import requests

# Build-in modules
import json


# Global variables - edit these to match your local setup
URL = 'http://localhost:4950/schema/v1'
SCHEMAPATH = 'schema.json'


def inject_schema():
    '''
    Take the contents of SCHEMA and use them to, well, build the schema via the REST API.
    '''
    # Load the schema
    infile = open(SCHEMAPATH, 'r')
    schema = json.load(infile)
    infile.close()
    # First, create the resourcetypes
    for resourcetype, details in schema['resourcetypes'].items():
        # Accumulate the resourcetype's attributes
        payload = {}
        for attribute in ['notes', 'dependent', 'attributes']:
            if attribute in details:
                payload[attribute] = details[attribute]
        # Create the resourcetype
        result = requests.post('%s/resourcetype/%s' % (URL, resourcetype), data=payload)
        # Report any failed requests
        if result.status_code != 201:
            print('ERROR %s - %s: /resourcetype/%s' % (
                result.status_code,
                result.text,
                resourcetype
                ))
    # Now create the relationships between the types
    for details in schema['relationships']:
        # Accumulate the relationship's attributes
        payload = {}
        for attribute in ['cardinality', 'dependent']:
            if attribute in details:
                payload[attribute] = details[attribute]
        # Create the relationship
        result = requests.post('%s/relationship%s' % (URL, details['uri']), data=payload)
        if result.status_code != 201:
            print('ERROR %s - %s: %s' % (result.status_code, result.text, details['uri']))

# Run automatically, if invoked as a CLI script
if __name__ == '__main__':
    inject_schema()
