#!/usr/bin/python3

# For migrating data from Device42 to Syscat

import requests
import certifi
import re

D42_URI='https://device42.example.com/api/1.0'
D42_USER=False
D42_PASSWD=False

SYSCAT_URI='http://localhost:4950/raw/v1'

DEFAULT_ASN='ournet'


# Utility functions

# Leaving the name unchanged because Syscat is the only system to which we're POSTing data
def post(uri, data):
    """
    Post data to Syscat, and report the result to STDOUT for feedback to the user
    """
    response = requests.post('%s/%s' % (SYSCAT_URI, uri), data=data)
    print '%s: %s' % (uri, response.status_code)

def sanitise_uid(uid):
    '''
    Sanitise a UID string in the same way Restagraph does
    '''
    return re.sub('[/ ]', '_', uid)


# Actual migration functions

CUSTOMER_CACHE={}   # We'll need this for a lookup table later.
def migrate_customers():
    for cust in requests.get('%s/customers/' % D42_URI, auth=(D42_USER, D42_PASSWD)).json()['Customers']:
        # Add it to the cache
        CUSTOMER_CACHE[cust['id']] = cust['name']
        # Install it in Syscat
        post('organisations', {'uid': cust['name'], 'description': cust['notes']})

def create_brand(name, notes):
    post('brands', {'uid': name, 'notes': notes})

def migrate_brands():
    for vendor in requests.get('%s/vendors/' % D42_URI, auth=(D42_USER, D42_PASSWD)).json()['vendors']:
        create_brand(vendor['name'], ['notes'])

def create_model(brandname, modelname):
    brand='None'
    if brandname and brandname != None:
        brand = sanitise_uid(brandname)
    else:
        create_brand('None', 'For models with no identified brand')
    post('brands/%s/Models' % (brand), {'type': 'models', 'uid': modelname})

def migrate_models():
    for model in requests.get('%s/hardwares/' % D42_URI, auth=(D42_USER, D42_PASSWD)).json()['models']:
        create_model(model['manufacturer'], model['name'])

def create_operating_system(name, brand=False):
    post('operatingSystems', {'uid': name})
    # Associate the OS with its brand, if one was specified
    if brand:
        post('brands/%s/Produces' % (brand), {'target': '/operatingSystems/%s' % sanitise_uid(name)})

def migrate_operating_systems():
    for os in requests.get('%s/operatingsystems/' % D42_URI, auth=(D42_USER, D42_PASSWD)).json()['operatingsystems']:
        brand=False
        if os['manufacturer'] != None:
            brand=os['manufacturer']
        create_operating_system(os['name'], brand)

def migrate_buildings():
    '''
    Note that this creates a site with the same name as the building, then creates the building under it.
    '''
    for bldg in requests.get('%s/buildings/' % D42_URI, auth=(D42_USER, D42_PASSWD)).json()['buildings']:
        post('sites', data={'uid': bldg['name'], 'notes': 'Automatically created during migration from Device42'}),
        post('sites/%s/Buildings' % bldg['name'], data={'type': 'buildings', 'uid': bldg['name'], 'notes': bldg['notes']})

def migrate_rooms():
    for room in requests.get('%s/rooms/' % D42_URI, auth=(D42_USER, D42_PASSWD)).json()['rooms']:
        post('sites/%s/Buildings/buildings/%s/Rooms' % (room['building'], room['building']), data={'type': 'rooms', 'uid': room['name'], 'notes': room['notes']})

def create_device(details):
    data={
        'uid': details['name'],
        'in_service': details['in_service'],
    }
    # Serial number
    if 'serial_no' in details['serial_no'] and details['serial_no'] != None:
        data['serial_number']=details['serial_no']
    else:
        data['serial_number']=''
    # Asset number
    if 'asset_no' in details['asset_no'] and details['asset_no'] != None:
        data['asset_number']=details['asset_no']
    else:
        data['asset_number']=''
    # Now create it
    print('DEBUG Creating device with details: %s' % details)
    post('devices', data)
    # Now link other things as we confirm we have them
    # Owner
    if details['customer'] and details['customer'] != None:
        print('DEBUG Connecting device %s to customer %s' % (details['name'], details['name']))
        post('devices/%s/BusinessOwner' % details['name'], {'target': '/organisations/%s' % details['customer']})
    # Model
    if (details['hw_model']) and (details['hw_model'] != None) and (details['manufacturer']) and (details['manufacturer'] != None):
        print('DEBUG Connecting device %s to model %s/%s' % (details['name'], details['manufacturer'], details['hw_model']))
        post('devices/%s/Model' % details['name'], {'target': '/brands/%s/Models/models/%s' % (sanitise_uid(details['manufacturer']), sanitise_uid(details['hw_model']))})
    # OS
    if (details['os']) and (details['os'] != None):
        print('DEBUG Connecting device %s to OS %s' % (details['name'], details['os']))
        post('devices/%s/OperatingSystem' % details['name'], {'target': '/operatingSystems/%s' % (sanitise_uid(details['os']))})
    # Tags
    for tag in details['tags']:
        print('DEBUG Connecting')
        post('tags', data={'uid': tag})
        post('devices/%s/Tags' % details['name'], data={'target': '/tags/%s' % tag})
    # Site
    if details['building'] != None and details['building'] != '':
        if details['room'] != None and details['room'] != '':
            target='/sites/%s/Buildings/buildings/%s/Rooms/rooms/%s' % (details['building'], details['building'], sanitise_uid(details['room']))
        else:
            target='/sites/%s/Buildings/buildings/%s' % (details['building'], details['building'])
        print('DEBUG: linking device %s to location %s' % (details['name'], target))
        post('devices/%s/Location' % details['name'], data={'target': target})

def migrate_devices():
    for device in requests.get('%s/devices/all/?include_cols=name,serial_no,asset_no,in_service,service_level,type,tags,customer,hw_model,manufacturer, room, building,location,os,blankasnull=true' % D42_URI, auth=(D42_USER, D42_PASSWD)).json()['Devices']:
        create_device(device)

def migrate_vrfs():
    # Create the default ASN, because Device42 doesn't have this concept
    post('asn', {'uid': DEFAULT_ASN})
    # Now install the VRFs
    for vrf in requests.get('%s/vrf_group/' % D42_URI, auth=(D42_USER, D42_PASSWD)).json():
        post('asn/%s/VrfGroups' % DEFAULT_ASN, {'type': 'vrfGroups', 'uid': vrf['name']})

def migrate_subnets():
    for subnet in requests.get('%s/subnets/' % D42_URI, auth=(D42_USER, D42_PASSWD)).json()['subnets']:
        # Insert the subnet itself
        result=post(
                '/asn/%s/VrfGroups/%s/Subnets' % (DEFAULT_ASN, subnet['vrf_group_name']),
                {
                    'type': 'ipv4Subnets',
                    'uid': subnet['network'],
                    'prefixlength': subnet['mask_bits'],
                    'description': subnet['description'],
                    }
                )
        # Now link it to a customer,
        # using the cache to avoid a D42 lookup for every last subnet
        post('~A/Owner' % result.text, {'target': '/organisations/%s' % CUSTOMER_CACHE[subnet['customer_id']])
        # Tags
        for tag in subnet['tags']:
            # There's no way of just querying D42 for tags, so we just need to do this the hard way
            pass

def migrate_all_the_things():
    migrate_customers()
    migrate_brands()
    migrate_models()
    migrate_operating_systems()
    migrate_buildings()
    migrate_rooms()
    migrate_devices()
    migrate_vrfs()
    migrate_subnets()
