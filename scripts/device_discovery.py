#!/usr/bin/python3

# Perform discovery on an individual host, using SNMP version 2c

# Install tips on Ubuntu 16.04:
# sudo apt-get install python3-setuptools python3-certifi
# sudo easy_install3 pip
# sudo pip3 install pysnmp

# Third-party libraries
import netdescribe.device_discovery
import netdescribe.snmp.device_discovery
import requests
import certifi

# Included batteries
import argparse
import re
import logging
import sys


# Configuration details
SYSCAT_URI='http://localhost:4950/raw/v1'


# Configure logging
LOGLEVEL=logging.INFO
# Basic setup
LOGGER = logging.getLogger('device_discovery')
# Create console handler
# create and configure console handler, and add it to the logger
ch = logging.StreamHandler(stream=sys.stdout)
ch.setFormatter(logging.Formatter('%(asctime)s %(name)s %(levelname)s %(message)s'))
ch.setLevel(LOGLEVEL)
LOGGER.setLevel(LOGLEVEL)
LOGGER.addHandler(ch)


# Utility functions

def sanitise_uid(uid):
    '''
    Sanitise a UID string in the same way Restagraph does
    '''
    return re.sub('[/ ]', '_', uid)


# Populate Syscat with data

def populate_interfaces_flat(hostname, interfaces, ifaceaddrmap):
    '''
    Add interface details to a device.
    Just attach each interface directly to the device, without making any attempt
    to distinguish between subinterfaces and parents.
    '''
    for index, details in interfaces.items():
        ifurl = '%s/devices/%s/Interfaces' % (SYSCAT_URI, hostname)
        LOGGER.debug('Attempting to add network interface %s to device %s at URL %s', details['ifName'], hostname, ifurl)
        netresponse = requests.post(
                ifurl,
                data={
                    'type': 'networkInterfaces',
                    'uid': details['ifName'],
                    'snmp_index': index,
                    'ifname': details['ifName'],    # Just in case
                    'ifdescr': details['ifDescr'],
                    'ifalias': details['ifAlias'],
                    'iftype': details['ifType'],
                    'ifspeed': details['ifSpeed'],
                    'ifhighspeed': details['ifHighSpeed'],
                    'ifphysaddress': details['ifPhysAddress'],
                    },
                )
        LOGGER.debug('result of interface creation for %s (%s): %s - %s' % (index, details['ifName'], netresponse.status_code, netresponse.text))
        # Add IPv4 addresses
        if str(index) in ifaceaddrmap:    # Not all interfaces have addresses
            for addr in ifaceaddrmap[str(index)]:
                ipurl = '%s/devices/%s/Interfaces/networkInterfaces/%s/Addresses' % (SYSCAT_URI, hostname, sanitise_uid(details['ifName']))
                LOGGER.debug('Attempting to create IPv4 Address %s under URL %s' % (addr['address'], ipurl))
                addresponse = requests.post(
                        ipurl,
                        data={
                            'type': 'ipv4Addresses',
                            'uid': addr['address'],
                            'netmask': addr['netmask'],
                            }
                        )
            LOGGER.debug('result of address creation for %s: %s - %s' % (addr['address'], addresponse.status_code, addresponse.text))
        else:
            LOGGER.debug('No addresses found for interface with index number %s; moving on.' % str(index))

def populate_interface_tree(hostname, interfaces, ifstacktree, ifaceaddrmap):
    '''
    Add interfaces to a device in a nested style,
    so that subinterfaces are linked to their parent interfaces rather than the device.
    '''
    # Top-level
    for index, subs in ifstacktree.items():
        details = interfaces[index]
        ifurl = '%s/devices/%s/Interfaces' % (SYSCAT_URI, hostname)
        LOGGER.debug('Attempting to create top-level network interface %s at URL %s', details['ifName'], ifurl)
        netresponse = requests.post(
                ifurl,
                data={
                    'type': 'networkInterfaces',
                    'uid': details['ifName'],
                    'snmp_index': index,
                    'ifname': details['ifName'],    # Just in case
                    'ifdescr': details['ifDescr'],
                    'ifalias': details['ifAlias'],
                    'iftype': details['ifType'],
                    'ifspeed': details['ifSpeed'],
                    'ifhighspeed': details['ifHighSpeed'],
                    'ifphysaddress': details['ifPhysAddress'],
                    },
                )
        # Add subinterfaces (nobody ever goes deeper than 2, right?)
        if subs:
            for sub in subs:
                subdetails = interfaces[sub]
                if2url = '%s/devices/%s/Interfaces/networkInterfaces/%s/SubInterfaces' % (SYSCAT_URI, hostname, sanitise_uid(details['ifName']))
                LOGGER.debug('Attempting to create second-level network interface %s at URL %s' % (subdetails['ifName'], if2url))
                subresponse = requests.post(
                        if2url,
                        data={
                            'type': 'networkInterfaces',
                            'uid': subdetails['ifName'],
                            'snmp_index': sub,
                            'ifname': subdetails['ifName'],    # Just in case
                            'ifdescr': subdetails['ifDescr'],
                            'ifalias': subdetails['ifAlias'],
                            'iftype': subdetails['ifType'],
                            'ifspeed': subdetails['ifSpeed'],
                            'ifhighspeed': subdetails['ifHighSpeed'],
                            'ifphysaddress': subdetails['ifPhysAddress'],
                            },
                        )
                LOGGER.debug('result of interface creation for %s (%s): %s - %s' % (index, subdetails['ifName'], subresponse.status_code, subresponse.text))
            # Add IPv4 addresses
            if str(sub) in ifaceaddrmap:    # Not all interfaces have addresses
                for addr in ifaceaddrmap[str(sub)]:
                    ip2url = '%s/devices/%s/Interfaces/networkInterfaces/%s/SubInterfaces/networkInterfaces/%s/Addresses' % (
                                SYSCAT_URI,
                                hostname,
                                sanitise_uid(details['ifName']),
                                sanitise_uid(subdetails['ifName']),
                                )
                    LOGGER.debug('Attempting to create IPv4 Address %s under URL %s' % (addr['address'], ip2url))
                    addresponse = requests.post(
                            ip2url,
                            data={
                                'type': 'ipv4Addresses',
                                'uid': addr['address'],
                                'netmask': addr['netmask'],
                                }
                            )
                    LOGGER.debug('result of address creation for %s: %s - %s' % (addr['address'], addresponse.status_code, addresponse.text))
            else:
                LOGGER.debug('No addresses found for interface with index number %s; moving on.' % str(index))

def populateSyscat(device, hostname=False):
    '''
    Create a device in Syscat based on the information discovered by exploreDevice.
    Optionally accepts a hostname to override the discovered name, in case it's
    known to the business by another name..
    '''
    # Hostname.
    # If we were supplied one as a parameter, use that.
    if hostname:
        host = hostname
    # Otherwise, use the SNMP-discovered one
    else:
        host = device['sysinfo']['sysName']
    LOGGER.info('Populating Syscat with details for %s', host)
    # Create the device itself
    sysresponse = requests.post(
            '%s/devices' % (SYSCAT_URI),
            data={
                'uid': host,
                'sysdescr': device['sysinfo']['sysDescr'],
                },
            )
    LOGGER.debug('result of device creation: %s - %s' % (sysresponse.status_code, sysresponse.text))
    # Interfaces
    if 'ifStackTree' in device['network']:
        populate_interface_tree(hostname, device['network']['interfaces'], device['network']['ifStackTree'], device['network']['ifIfaceAddrMap'])
    else:
        populate_interfaces_flat(host, device['network']['interfaces'], device['network']['ifIfaceAddrMap'])


# Enable this to be run as a CLI script, as well as used as a library.
# Mostly used for testing, at this stage.
if __name__ == '__main__':
    # Get the command-line arguments
    parser = argparse.ArgumentParser(description='Perform SNMP discovery on a host, to populate Syscat with its details.')
    parser.add_argument('hostname', type=str, help='The hostname or address to perform discovery on')
    parser.add_argument('--community', type=str, action='store', dest='community', default='public', help='SNMP v2 community string')
    parser.add_argument('--debug', action='store_true', help='Enable debug logging')
    args=parser.parse_args()
    # Set debug logging, if requested
    if args.debug:
        LOGGER.setLevel(logging.DEBUG)
        ch.setLevel(logging.DEBUG)
    # Do the job
    populateSyscat(netdescribe.snmp.device_discovery.exploreDevice(args.hostname, LOGGER, community=args.community))
