#!/usr/bin/env python3

# Set up the Syscat schema in the database,
# via the REST API.

# Third-party modules
import requests

# Edit to match your local setup
URL = 'http://localhost:4950/schema/v1'

def inject_schema():
    '''
    Take the contents of SCHEMA and use them to, well, build the schema via the REST API.
    '''
    # First, create the resourcetypes
    for resourcetype, details in SCHEMA['resourcetypes'].items():
        # Accumulate the resourcetype's attributes
        payload = {}
        for attribute in ['notes', 'dependent', 'attributes']:
            if attribute in details:
                payload[attribute] = details[attribute]
        # Create the resourcetype
        result = requests.post('%s/resourcetype/%s' % (URL, resourcetype), data=payload)
        # Report any failed requests
        if result.status_code != 201:
            print('ERROR %s - %s: /resourcetype/%s' % (result.status_code, result.text, resourcetype))
    # Now create the relationships between the types
    for details in SCHEMA['relationships']:
        # Accumulate the relationship's attributes
        payload = {}
        for attribute in ['cardinality', 'dependent']:
            if attribute in details:
                payload[attribute] = details[attribute]
        # Create the relationship
        result = requests.post('%s/relationship%s' % (URL, details['uri']), data=payload)
        if result.status_code != 201:
            print('ERROR %s - %s: %s' % (result.status_code, result.text, details['uri']))

SCHEMA = {
        'resourcetypes': {
            'any': {
                'notes': 'Represents any type of resource.',
                },
            'tags': {
                'notes': 'For categorising resources of any type.',
                },
            'countries': {
                'notes': 'The geographic kind.',
                },
            'states': {
                'notes': 'The geographic kind.',
                'dependent': 'true',
                },
            'cities': {
                'notes': 'The geographic kind.',
                'dependent': 'true',
                },
            'streets': {
                'dependent': 'true',
                },
            'street_numbers': {
                'notes': 'The UID is the street number',
                'dependent': 'true',
                'attributes': ['floor', 'name'],
                },
            'postbox_addresses': {
                'notes': 'The UID is the PO Box number.',
                },
            'postcodes': {},
            'sites': {
                'notes': 'Like a campus, a site contains one or more buildings.',
                'attributes': ['longname', 'comments']
                },
            'buildings': {
                'notes': 'Individual buildings.',
                'attributes': ['comments'],
                },
            'floors': {
                'notes': 'Floors within a building.',
                'attributes': ['comments'],
                },
            'rooms': {
                'notes': 'Rooms within a floor or a building.',
                'attributes': ['comments'],
                },
            'cages': {
                'notes': 'Lockable areas within a room, containing racks. Usually found in data centres.',
                'attributes': ['comments'],
                },
            'cabinets': {
                    'notes': 'A frame for rack-mounting equipment in. May be in the form of a lockable cabinet.',
                    'attributes': ['comments'],
                    },
            'racks': {
                    'notes': 'The rack location within a cabinet where a device is mounted. Usually the topmost rack number, in the case of multi-unit-height hardware.',
                    'attributes': ['comments'],
                    },
            'devices': {
                    'notes': 'Any kind of device that interacts with a network, be it L1, L2 or L3.',
                    'attributes': ['comments', 'sysDescr', 'snmp_community', 'serial_number', 'asset_number'],
                    },
            'deviceTypes': {
                    'notes': 'Type of device, e.g. physical, cluster or virtual.',
                    'attributes': ['comments'],
                    },
            'deviceRoles': {
                    'notes': 'Classification of the device, e.g. host, router or switch.',
                    'attributes': ['comments'],
                    },
            'lifeCyclePhases': {
                    'notes': 'Where the device currently is, in its lifecycle. E.g. ordered, installed, in service or decommissioned.',
                    'attributes': ['comments'],
                    },
            'environments': {
                    'notes': 'Development, testing, staging, production etc.',
                    'attributes': ['comments'],
                    },
            'serviceLevels': {
                    'notes': 'Sets the expectations for responses to issues, e.g. gold, silver, bronze, plastic.',
                    'attributes': ['comments'],
                    },
            'networkInterfaces': {
                    'dependent': 'true',
                    'attributes': ['comments', 'snmp_index', 'ifName', 'ifDescr', 'ifType', 'ifAlias', 'ifSpeed', 'ifHighspeed', 'ifPhysAddress'],
                    },
            'l1Links': {
                    'notes': 'Layer 1 network links, e.g. ethernet or fibre-optic cables.',
                    'attributes': ['comments', 'our_id', 'vendor_id'],
                    },
            'l1LinkTypes': {
                    'notes': 'Types of L1 link, e.g. cat5Ethernet, MMF, serialCable.',
                    'attributes': ['comments'],
                    },
            'l1Circuits': {
                    'notes': 'Logical circuits of one or more layer 1 links.',
                    'attributes': ['comments', 'our_id', 'vendor_id'],
                    },
            'parts': {
                    'notes': 'Spare parts, swappable components, anything of the kind',
                    'dependent': 'true',
                    'attributes': ['comments'],
                    },
            'operatingSystems': {
                    'dependent': 'true',
                    },
            'operatingSystemVersions': {
                    'dependent': 'true',
                    },
            'brands': {
                    'notes': 'Also known as "make".',
                    'attributes': ['comments'],
                    },
            'models': {
                    'dependent': 'true',
                    },
            'vlans': {
                    'notes': 'Layer 2 spans, i.e. ethernet segments. Not to be confused with subnets.',
                    'dependent': 'true',
                    },
            'l2Links': {
                    'attributes': ['comments', 'our_id', 'vendor_id'],
                    },
            'l2LinkTypes': {
                    'notes': 'Types of L2 link, e.g. cat5Ethernet, MMF, serialCable.',
                    'attributes': ['comments'],
                    },
            'l2Circuits': {
                    'notes': 'Logical circuits of one or more layer 2 links.',
                    'attributes': ['comments', 'our_id', 'vendor_id'],
                    },
            'vrfGroups': {
                    'dependent': 'true',
                    },
            'routingInstances': {
                    'dependent': 'true',
                    'attributes': ['snmp_community'],
                    },
            'ipv4Addresses': {
                    'dependent': 'true',
                    'attributes': ['prefixlength', 'comments'],
                    },
            'ipv4Subnets': {
                    'dependent': 'true',
                    'attributes': ['prefixlength', 'comments'],
                    },
            'ipv6Addresses': {
                    'dependent': 'true',
                    'attributes': ['prefixlength', 'comments'],
                    },
            'ipv6Subnets': {
                    'dependent': 'true',
                    'attributes': ['prefixlength', 'comments'],
                    },
            'l3Tunnels': {
                    'attributes': ['comments'],
                    },
            'l3TunnelTypes': {
                    'attributes': ['comments'],
                    },
            'asns': {
                    'notes': 'Autonomous Systems. The UID should be the ASN, and the "name" should be its human-friendly tag.',
                    'attributes': ['comments', 'name'],
                    },
            'applications': {
                    'attributes': ['comments'],
                    },
            'people': {
                    'notes': 'UID should be their login name or some other compact reference',
                    'attributes': ['given_name', 'surname'],
                    },
            'emailAddresses': {
                    'dependent': 'true',
                    'attributes': ['comments'],
                    },
            'emailAddresses': {
                    'dependent': 'true',
                    'attributes': ['comments'],
                    },
            'phoneNumbers': {
                    'attributes': ['comments'],
                    },
            'organisations': {
                    'notes': 'Any kind of organisation: professional, social or other.',
                    },
            'contracts': {
                    'attributes': ['comments', 'reference_number', 'start_date', 'end_date'],
                    },
            },
        'relationships': [
                {'uri': '/any/Tag/tags', 'cardinality': 'many:many'},
                {'uri': '/countries/State/states', 'cardinality': '1:many', 'dependent': 'true'},
                {'uri': '/countries/City/cities', 'cardinality': '1:many', 'dependent': 'true'},
                {'uri': '/countries/Postcode/postcodes', 'cardinality': '1:many', 'dependent': 'true'},
                {'uri': '/states/City/cities', 'cardinality': '1:many', 'dependent': 'true'},
                {'uri': '/states/Postcode/postcodes', 'cardinality': '1:many', 'dependent': 'true'},
                {'uri': '/cities/Postcode/postcodes', 'cardinality': '1:many', 'dependent': 'true'},
                {'uri': '/cities/Street/streets', 'cardinality': '1:many', 'dependent': 'true'},
                {'uri': '/streets/StreetNumber/street_numbers', 'cardinality': '1:many', 'dependent': 'true'},
                {'uri': '/buildings/Addresses/street_numbers', 'cardinality': 'many:1'},
                {'uri': '/street_numbers/postcode/postcodes', 'cardinality': 'many:1'},
                {'uri': '/postbox_addresses/postcode/postcodes', 'cardinality': 'many:1'},
                {'uri': '/buildings/Location/countries', 'cardinality': 'many:1'},
                {'uri': '/buildings/Location/states', 'cardinality': 'many:1'},
                {'uri': '/buildings/Location/cities', 'cardinality': 'many:1'},
                {'uri': '/buildings/Floor/floors', 'cardinality': '1:many', 'dependent': 'true'},
                {'uri': '/buildings/Room/rooms', 'cardinality': '1:many', 'dependent': 'true'},
                {'uri': '/floors/Room/rooms', 'cardinality': '1:many', 'dependent': 'true'},
                {'uri': '/rooms/Cage/cages', 'cardinality': '1:many', 'dependent': 'true'},
                {'uri': '/rooms/Cabinet/cabinets', 'cardinality': '1:many', 'dependent': 'true'},
                {'uri': '/cages/Cabinet/cabinets', 'cardinality': '1:many', 'dependent': 'true'},
                {'uri': '/cabinets/Rack/racks', 'cardinality': '1:many', 'dependent': 'true'},
                {'uri': '/devices/PurchasedFrom/organisations', 'cardinality': 'many:1'},
                {'uri': '/devices/Location/sites', 'cardinality': 'many:1'},
                {'uri': '/devices/Location/buildings', 'cardinality': 'many:1'},
                {'uri': '/devices/Location/floors', 'cardinality': 'many:1'},
                {'uri': '/devices/Location/rooms', 'cardinality': 'many:1'},
                {'uri': '/devices/Location/cages', 'cardinality': 'many:1'},
                {'uri': '/devices/Location/cabinets', 'cardinality': 'many:1'},
                {'uri': '/devices/Location/racks', 'cardinality': 'many:1'},
                {'uri': '/devices/Interfaces/networkInterfaces', 'cardinality': '1:many', 'dependent': 'true'},
                {'uri': '/devices/OperatingSystem/operatingSystems', 'cardinality': 'many:1'},
                {'uri': '/devices/OperatingSystem/operatingSystemVersions', 'cardinality': 'many:1'},
                {'uri': '/devices/Member/devices', 'cardinality': 'many:1'},
                {'uri': '/devices/HostedOn/devices', 'cardinality': 'many:1'},
                {'uri': '/networkInterfaces/Subinterfaces/networkInterfaces', 'cardinality': '1:many', 'dependent': 'true'},
                {'uri': '/networkInterfaces/Member/networkInterfaces', 'cardinality': 'many:1'},
                {'uri': '/networkInterfaces/Connectsto/networkInterfaces', 'cardinality': "1:1"},
                {'uri': '/parts/Model/models', 'cardinality': 'many:1'},
                {'uri': '/parts/PurchasedFrom/organisations', 'cardinality': 'many:1'},
                {'uri': '/models/CompatibleWith/models', 'cardinality': 'many:many'},
                {'uri': '/parts/InstalledIn/devices', 'cardinality': 'many:1'},
                {'uri': '/parts/StorageLocation/sites', 'cardinality': 'many:1'},
                {'uri': '/parts/StorageLocation/buildings', 'cardinality': 'many:1'},
                {'uri': '/parts/StorageLocation/rooms', 'cardinality': 'many:1'},
                {'uri': '/parts/StorageLocation/cages', 'cardinality': 'many:1'},
                {'uri': '/parts/StorageLocation/cabinets', 'cardinality': 'many:1'},
                {'uri': '/devices/ConnectsTo/l1Links', 'cardinality': 'many:many'},
                {'uri': '/networkInterfaces/ConnectsTo/l1Links', 'cardinality': 'many:1'},
                {'uri': '/devices/ConnectsTo/l1Circuits', 'cardinality': 'many:many'},
        {'uri': '/networkInterfaces/ConnectsTo/l1Circuits', 'cardinality': 'many:1'},
            {'uri': '/l1Links/ConnectsTo/l1Links', 'cardinality': 'many:1'},
            {'uri': '/l1Links/Member/l1Circuits', 'cardinality': 'many:1'},
            {'uri': '/l1Links/LinkType/l1LinkTypes', 'cardinality': 'many:1'},
            {'uri': '/devices/RoutingInstance/routingInstances', 'cardinality': '1:many', 'dependent': 'true'}, 
            {'uri': '/routingInstances/Interface/networkInterfaces', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/routingInstances/Member/vrfGroups', 'cardinality': 'many:1'},
            {'uri': '/brands/Produces/operatingSystems', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/operatingSystems/Version/operatingSystemVersions', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/brands/Produces/operatingSystems', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/devices/Model/models', 'cardinality': 'many:1'},
            {'uri': '/models/Capability/deviceRoles', 'cardinality': 'many:many'},
            {'uri': '/devices/DeviceType/deviceTypes', 'cardinality': 'many:1'},
            {'uri': '/devices/Role/deviceRoles', 'cardinality': 'many:many'},
            {'uri': '/devices/LifeCyclePhase/lifeCyclePhases', 'cardinality': 'many:1'},
            {'uri': '/devices/Environment/environments', 'cardinality': 'many:1'},
            {'uri': '/devices/ServiceLevel/serviceLevels', 'cardinality': 'many:1'},
            {'uri': '/brands/Produces/parts', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/devices/Vlan/vlans', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/networkInterfaces/Vlan/vlans', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/devices/ConnectsTo/l2Links', 'cardinality': 'many:many'},
            {'uri': '/networkInterfaces/ConnectsTo/l2Links', 'cardinality': 'many:many'},
            {'uri': '/devices/ConnectsTo/l2Circuits', 'cardinality': 'many:many'},
            {'uri': '/networkInterfaces/ConnectsTo/l2Circuits', 'cardinality': 'many:many'},
            {'uri': '/devices/Addresses/ipv4Addresses', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/devices/Addresses/ipv6Addresses', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/networkInterfaces/Addresses/ipv4Addresses', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/networkInterfaces/Addresses/ipv6Addresses', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/devices/RoutingInstance/routingInstances', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/devices/ConnectsTo/l3Tunnels', 'cardinality': 'many:1'},
            {'uri': '/networkInterfaces/Connectsto/l3Tunnels', 'cardinality': 'many:1'},
            {'uri': '/brands/Vendor/organisations', 'cardinality': 'many:1'},
            {'uri': '/devices/TechnicalOwner/people', 'cardinality': 'many:1'},
            {'uri': '/devices/BusinessOwner/people', 'cardinality': 'many:1'},
            {'uri': '/devices/BusinessOwner/organisations', 'cardinality': 'many:1'},
            {'uri': '/l1Links/BusinessOwner/people', 'cardinality': 'many:1'},
            {'uri': '/l1Links/BusinessOwner/organisations', 'cardinality': 'many:1'},
            {'uri': '/l1Circuits/BusinessOwner/people', 'cardinality': 'many:1'},
            {'uri': '/l1Circuits/BusinessOwner/organisations', 'cardinality': 'many:1'},
            {'uri': '/l2Links/Connectsto/l2Links', 'cardinality': 'many:many'},
            {'uri': '/l2Links/LinkType/l2LinkTypes', 'cardinality': 'many:1'},
            {'uri': '/l2Links/MemberOf/l2Circuits', 'cardinality': 'many:1'},
            {'uri': '/l2Links/ProvisionedBy/organisations', 'cardinality': 'many:1'},
            {'uri': '/l2Links/OperatedBy/organisations', 'cardinality': 'many:1'},
            {'uri': '/l2Circuits/ProvisionedBy/organisations', 'cardinality': 'many:1'},
            {'uri': '/l2Circuits/OperatedBy/organisations', 'cardinality': 'many:1'},
            {'uri': '/asns/VrfGroups/vrfGroups', 'cardinality': 'many:1', 'dependent': 'true'},
            {'uri': '/vrfGroups/Subnets/ipv4Subnets', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/vrfGroups/Subnets/ipv6Subnets', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/asns/Subnets/ipv4Subnets', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/asns/Subnets/ipv6Subnets', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/asns/Addresses/ipv4Addresses', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/asns/Addresses/ipv6Addresses', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/ipv4Subnets/Subnets/ipv4Subnets', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/ipv6Subnets/Subnets/ipv4Subnets', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/ipv4Subnets/Addresses/ipv4Addresses', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/ipv6Subnets/Addresses/ipv6Addresses', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/ipv4Addresses/ConnectsTo/l3Tunnels', 'cardinality': 'many:1'},
            {'uri': '/ipv6Addresses/ConnectsTo/l3Tunnels', 'cardinality': 'many:1'},
            {'uri': '/l3Tunnels/TunnelType/l3TunnelTypes', 'cardinality': 'many:1'},
            {'uri': '/asns/Owner/organisations', 'cardinality': 'many:1'},
            {'uri': '/ipv4Subnets/Owner/organisations', 'cardinality': 'many:1'},
            {'uri': '/ipv6Subnets/Owner/organisations', 'cardinality': 'many:1'},
            {'uri': '/applications/DependsOn/applications', 'cardinality': 'many:many'},
            {'uri': '/applications/RunsOn/devices', 'cardinality': 'many:many'},
            {'uri': '/applications/BusinessOwner/people', 'cardinality': 'many:1'},
            {'uri': '/applications/BusinessOwner/organisations', 'cardinality': 'many:1'},
            {'uri': '/applications/TechnicalOwner/people', 'cardinality': 'many:1'},
            {'uri': '/applications/TechnicalOwner/organisations', 'cardinality': 'many:1'},
            {'uri': '/organisations/ContactMethod/emailAddresses', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/people/ContactMethod/emailAddresses', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/people/ContactMethod/phoneNumbers', 'cardinality': '1:many', 'dependent': 'true'},
            {'uri': '/people/Member/organisations', 'cardinality': 'many:1'},
            {'uri': '/people/AccountManager/organisations', 'cardinality': 'many:1'},
            {'uri': '/people/SalesContact/organisations', 'cardinality': 'many:1'},
            {'uri': '/people/SupportContact/organisations', 'cardinality': 'many:1'},
            {'uri': '/organisations/Member/organisations', 'cardinality': 'many:many'},
            {'uri': '/contracts/Subcontracts/contracts', 'cardinality': 'many:1'},
            {'uri': '/contracts/Vendor/organisations', 'cardinality': 'many:1'},
            {'uri': '/contracts/Customer/organisations', 'cardinality': 'many:1'},
            {'uri': '/contracts/NegotiatedBy/people', 'cardinality': 'many:many'},
            {'uri': '/contracts/Supplies/devices', 'cardinality': '1:many'},
            {'uri': '/contracts/Supports/devices', 'cardinality': '1:many'},
            {'uri': '/contracts/Supplies/parts', 'cardinality': '1:many'},
            {'uri': '/contracts/Supports/parts', 'cardinality': '1:many'},
            {'uri': '/contracts/Supplies/buildings', 'cardinality': '1:many'},
            {'uri': '/contracts/Supports/buildings', 'cardinality': '1:many'},
            {'uri': '/contracts/Supplies/floors', 'cardinality': '1:many'},
            {'uri': '/contracts/Supports/floors', 'cardinality': '1:many'},
            {'uri': '/contracts/Supplies/rooms', 'cardinality': '1:many'},
            {'uri': '/contracts/Supports/rooms', 'cardinality': '1:many'},
            {'uri': '/contracts/Supplies/cages', 'cardinality': '1:many'},
            {'uri': '/contracts/Supports/cages', 'cardinality': '1:many'},
            {'uri': '/contracts/Supplies/cabinets', 'cardinality': '1:many'},
            {'uri': '/contracts/Supports/cabinets', 'cardinality': '1:many'},
            {'uri': '/sites/Organisation/organisations', 'cardinality': 'many:1'},
            {'uri': '/buildings/Tenant/organisations', 'cardinality': 'many:many'},
            {'uri': '/floors/Tenant/organisations', 'cardinality': 'many:many'},
            {'uri': '/rooms/Tenant/organisations', 'cardinality': 'many:many'},
            {'uri': '/cages/Tenant/organisations', 'cardinality': 'many:many'},
            {'uri': '/cabinets/Tenant/organisations', 'cardinality': 'many:many'},
            ]
        }

if __name__ == '__main__':
    inject_schema()