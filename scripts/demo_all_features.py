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
Demonstration of the kinds of thing Syscat can model.
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
    post_raw('/organisations', data={'uid': 'Internet', 'comments': 'A notional entity, representing everything outside the borders of this detailed map.'})
    post_raw('/organisations', data={'uid': 'Marsh Refinery'})
    post_raw('/organisations', data={'uid': 'Bolton Mills'})
    post_raw('/organisations', data={'uid': 'Dillinger Associates', 'comments': 'Outsourced R&D for Marsh Refinery.'})
    post_raw('/organisations/Dillinger_Associates/Parent', data={'target': '/organisations/Marsh_Refinery'})
    post_raw('/organisations/Marsh_Refinery/Subsidiary', data={'target': '/organisations/Dillinger_Associates'})
    post_raw('/organisations', data={'uid': 'Dunwich ISP'})
    post_raw('/organisations', data={'uid': 'Red Hook ISP'})

    # Sites
    #
    # Marsh Refinery
    post_raw('/sites', data={'uid': 'MarshRef1', 'longname': 'Marsh Refinery no.1'})
    post_raw('/sites/MarshRef1/Organisation', data={'target': '/organisations/Marsh_Refinery'})
    post_raw('/organisations/Marsh_Refinery/Site', data={'target': '/sites/MarshRef1'})
    # Dillinger
    post_raw('/sites', data={'uid': 'Site Able', 'longname': 'Dillinger Associates main business campus'})
    post_raw('/sites/Site_Able/Organisation', data={'target': '/organisations/Dillinger_Associates'})
    post_raw('/organisations/Dillinger_Associates/Site', data={'target': '/sites/Site_Able'})
    # Bolton Mills
    post_raw('/sites', data={'uid': 'BM1', 'longname': 'Main site for Bolton Mills'})
    post_raw('/sites/BM1/Organisation', data={'target': '/organisations/Bolton_Mills'})
    post_raw('/organisations/Bolton_Mills/Site', data={'target': '/sites/BM1'})

    # Buildings
    #
    post_raw('/buildings', data={'uid': 'MarshRefOffice1', 'comments': 'Admin office for the Marsh Refinery'})
    post_raw('/sites/MarshRef1/Building', data={'target': '/buildings/MarshRefOffice1'})
    post_raw('/buildings/MarshRefOffice1/Site', data={'target': '/sites/MarshRef1'})
    #
    post_raw('/buildings', data={'uid': 'MarshMaint1', 'comments': 'Maintenance shed for the Marsh Refinery'})
    post_raw('/sites/MarshRef1/Building', data={'target': '/buildings/MarshMaint1'})
    post_raw('/buildings/MarshMaint1/Site', data={'target': '/sites/MarshRef1'})

    # ASNs
    # Associate them with their owning organisations - in both directions.
    # Why both directions? To enable search in both directions via this API.
    # Note that the UIDs for the organisations have been canonicalised,
    # with underscores replacing the original spaces.
    #
    post_raw('/asns', data={'uid': '64496', 'comments': 'Marsh Refinery'})
    post_raw('/asns/64496/AllocatedTo', data={'target': '/organisations/Marsh_Refinery'})
    post_raw('/organisations/Marsh_Refinery/Asns', data={'target': '/asns/64496'})
    #
    post_raw('/asns', data={'uid': '64510', 'comments': 'Dillinger_Associates'})
    post_raw('/asns/64510/AllocatedTo', data={'target': '/organisations/Bolton_Mills'})
    post_raw('/organisations/Bolton_Mills/Asns', data={'target': '/asns/64510'})
    #
    post_raw('/asns', data={'uid': '616', 'comments': 'Dunwich ISP'})
    post_raw('/asns/616/AllocatedTo', data={'target': '/organisations/Dunwich_ISP'})
    post_raw('/organisations/Dunwich_ISP/Asns', data={'target': '/asns/616'})
    #
    post_raw('/asns', data={'uid': '217', 'comments': 'Red Hook ISP'})
    post_raw('/asns/217/AllocatedTo', data={'target': '/organisations/Red_Hook_ISP'})
    post_raw('/organisations/Red_Hook_ISP/Asns', data={'target': '/asns/217'})

    # IPAM
    #
    # The internet at large
    post_ipam('subnets', data={'org': 'Internet', 'subnet': '0.0.0.0/0'})
    #
    # Marsh Refinery supernet
    post_ipam('subnets', data={'org': 'Marsh_Refinery', 'subnet': '10.86.0.0/16'})
    #
    # Red Hook ISP transit subnets range
    post_ipam('subnets', data={'org': 'Red_Hook_ISP', 'subnet': '198.51.100.0/24'})
    #
    # Dunwich ISP transit subnets range
    post_ipam('subnets', data={'org': 'Dunwich_ISP', 'subnet': '203.0.113.0/24'})
    #
    # Dillinger Associates Site Able
    ORG='Dillinger_Associates'
    post_ipam('subnets', data={
        'org': ORG,
        'subnet': '192.168.0.0/16',
        'comments': "Doesn't conflict with the same subnet allocated by Bolton, because it's in a different organisation."
        })
    post_ipam('subnets', data={
        'org': ORG,
        'subnet': '192.0.2.0/28',
        'comments': '"Routed subnet", allocated by Dunwich ISP.'
        })
    # Delegate that routed subnet from the ISP, and trace the path backward as well
    SUBNET_URL = "%s/%s/subnets?org=%s&subnet=%s" % (BASE_URL, IPAMPREFIX, ORG, "192.0.2.0/28")
    SUBNET_PATH = requests.get("%s/%s/subnets?org=%s&subnet=%s" % (BASE_URL, IPAMPREFIX, ORG, "192.0.2.0/28")).text
    print("Received subnet path for %s: %s" % (SUBNET_URL, SUBNET_PATH))
    post_raw("/organisations/Dunwich_ISP/Allocated", data={"target": SUBNET_PATH})
    post_raw("%s/AllocatedTo" % SUBNET_PATH, data={"target": "/organisations/Dillinger_Associates"})
    #
    # Bolton Mills subnets
    post_ipam('subnets', data={'org': 'Bolton_Mills', 'subnet': '192.168.0.0/16'})
    post_ipam('subnets', data={'org': 'Bolton_Mills', 'subnet': '192.168.255.0/24', 'comments': 'Infra subnet'})
    post_ipam('addresses', data={'org': 'Bolton_Mills', 'address': '192.168.255.1'})

    # Devices
    #
    # Bolton
    post_raw('/devices', data={'uid': 'router1.bolton.com'})
    post_raw('/devices/router1.bolton.com/Location', data={'target': '/sites/BM1'})
    post_raw('/devices/router1.bolton.com/Interfaces/networkInterfaces', data={'uid': 'eth0'})
    post_raw("%s/AllocatedTo" % requests.get("%s/%s/addresses?org=%s&address=%s" % (BASE_URL, IPAMPREFIX, "Bolton_Mills", "192.168.255.1")).text,
        data={'target': '/devices/router1.bolton.com'})
    post_raw('/devices/router1.bolton.com/Interfaces/networkInterfaces/eth0/Addresses/ipv4Addresses', data={'uid': '192.168.255.1'})
    post_raw('/devices/router1.bolton.com/Interfaces/networkInterfaces', data={'uid': 'eth1'})
    #
    # Dillinger
    post_raw('/brands', data={'uid': 'Cisco'})
    post_raw('/brands/Cisco/Produces/models', data={'uid': '1841'})
    post_raw('/devices', data={'uid': 'router1.dillinger.com'})
    post_raw('/devices/router1.dillinger.com/Model', data={'target': '/brands/Cisco/Produces/models/1841'})
    post_raw('/devices/router1.dillinger.com/BusinessOwner', data={'target': '/organisations/Dillinger_Associates'})
    post_raw('/devices/router1.dillinger.com/Interfaces/networkInterfaces', data={'uid': 'eth0'})
    post_raw('/devices/router1.dillinger.com/Interfaces/networkInterfaces', data={'uid': 'eth1'})
    post_raw('/devices', data={'uid': 'switch1.dillinger.com'})
    post_raw('/devices/switch1.dillinger.com/BusinessOwner', data={'target': '/organisations/Dillinger_Associates'})
    post_raw('/devices/switch1.dillinger.com/Interfaces/networkInterfaces', data={'uid': 'eth1_1'})
    post_raw('/devices/switch1.dillinger.com/Interfaces/networkInterfaces', data={'uid': 'eth1_2'})
    post_raw('/devices/switch1.dillinger.com/Interfaces/networkInterfaces', data={'uid': 'eth1_3'})
    post_raw('/devices/switch1.dillinger.com/Interfaces/networkInterfaces', data={'uid': 'eth1_4'})
    #
    # Marsh
    post_raw('/devices', data={'uid': 'r1.marsh'})
    post_raw('/devices/r1.marsh/Interfaces/networkInterfaces', data={'uid': 'eth0'})
    post_raw('/devices/r1.marsh/Interfaces/networkInterfaces', data={'uid': 'eth2'})
    post_raw('/devices/r1.marsh/BusinessOwner', data={'target': '/organisations/Marsh_Refinery'})

    # Network interconnections
    #
    # Bolton Mills to the internet
    post_raw('/devices/router1.bolton.com/Interfaces/networkInterfaces/eth1/ConnectsTo', data={'target': '/organisations/Internet/Subnets/ipv4Subnets/0.0.0.0'})
    #
    # Dillinger R&D to Marsh refinery, provided by Dunwich ISP
    post_raw('/l1LinkTypes', data={'uid': 'dark_fibre'})
    post_raw('/l1Circuits', data={'uid': 'DUN257'})
    post_raw('/l1Links', data={'uid': 'DUNDF1086'})
    post_raw('/l1Links/DUNDF1086/LinkType', data={'target': '/l1LinkTypes/dark_fibre'})
    post_raw('/l1Links/DUNDF1086/Member', data={'target': '/l1Circuits/DUN257'})
    post_raw('/organisations/Dunwich_ISP/Suupplies', data={'target': '/l1Circuits/DUN257'})
    post_raw('/l1Circuits/DUN257/SuppliedBy', data={'target': '/organisations/Dunwich_ISP'})
    post_raw('/devices/router1.dillinger.com/Interfaces/networkInterfaces/eth0/ConnectsTo', data={'target': '/l1Links/DUNDF1086'})
    post_raw('/devices/r1.marsh/Interfaces/networkInterfaces/eth0/ConnectsTo', data={'target': '/l1Links/DUNDF1086'})
    #
    # Dillinger R&D to the internet
    post_raw('/l2Circuits', data={'uid': 'DIA_1'})
    post_raw('/l2Circuits/DIA_1/SuppliedBy', data={'target': '/organisations/Dunwich_ISP'})
    post_raw('/organisations/Dunwich_ISP/Supplies', data={'target': '/l2Circuits/DIA_1'})
    post_raw('/devices/router1.dillinger.com/Interfaces/networkInterfaces/eth1/ConnectsTo', data={'target': '/l2Circuits/DIA_1'})
    post_raw('/l2Circuits/DIA_1/ConnectsTo', data={'target': '/organisations/Internet/Subnets/ipv4Subnets/0.0.0.0'})
    #
    # Marsh Refinery to the internet
    post_raw('/l2Circuits', data={'uid': 'DIA_2'})
    post_raw('/l2Circuits/DIA_2/SuppliedBy', data={'target': '/organisations/Red_Hook_ISP'})
    post_raw('/organisations/Red_Hook_ISP/Supplies', data={'target': '/l2Circuits/DIA_2'})
    post_raw('/devices/r1.marsh/Interfaces/networkInterfaces/eth2/ConnectsTo', data={'target': '/l2Circuits/DIA_2'})
    post_raw('/l2Circuits/DIA_2/ConnectsTo', data={'target': '/organisations/Internet/Subnets/ipv4Subnets/0.0.0.0'})

if __name__ == '__main__':
    insert_model()
