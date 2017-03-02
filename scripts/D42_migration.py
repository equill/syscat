#!/usr/bin/python3

# For migrating data from Device42 to Syscat

import requests
import certifi
import re

D42_URI='https://device42.example.com/api/1.0'
D42_USER=False
D42_PASSWD=False

SYSCAT_URI='http://localhost:4950/raw/v1'


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
def create_customer(name, notes):
    post('organisations', {'uid': name, 'description': notes})

def migrate_customers():
    for cust in requests.get('%s/customers/' % D42_URI, auth=(D42_USER, D42_PASSWD)).json()['Customers']:
        create_customer(cust['name'], cust['notes'])

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

def migrate_devices():
    for device in requests.get('%s/devices/all/?include_cols=name,serial_no,asset_no,in_service,service_level,type,tags,customer,hw_model,manufacturer,location,os,blankasnull=true' % D42_URI, auth=(D42_USER, D42_PASSWD)).json()['Devices']:
        create_device(device)

def migrate_all_the_things():
    migrate_customers()
    migrate_brands()
    migrate_models()
    migrate_operating_systems()
    migrate_devices()
