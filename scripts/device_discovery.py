#!/usr/bin/python3

# Perform discovery on an individual host, using SNMP version 2c

# Install tips on Ubuntu 16.04:
# sudo apt-get install python3-setuptools python3-certifi
# sudo easy_install3 pip
# sudo pip3 install pysnmp

import requests
import certifi
import pysnmp.hlapi
import argparse
import re
import logging
import sys


# Configuration details
SYSCAT_URI='http://localhost:4950/raw/v1'


# Configure logging
LOGLEVEL=logging.INFO
# Basic setup
logger = logging.getLogger('device_discovery')
# Create console handler
# create and configure console handler, and add it to the logger
ch = logging.StreamHandler(stream=sys.stdout)
ch.setFormatter(logging.Formatter('%(asctime)s %(name)s %(levelname)s %(message)s'))
ch.setLevel(LOGLEVEL)
logger.setLevel(LOGLEVEL)
logger.addHandler(ch)


# Utility functions

def sanitise_uid(uid):
    '''
    Sanitise a UID string in the same way Restagraph does
    '''
    return re.sub('[/ ]', '_', uid)

def snmpGet(hostname, mib, attr, community, port=161):
    '''
    Perform an SNMP GET for a single OID or scalar attribute.
    Return only the value.
    '''
    # Use pysnmp to retrieve the data
    errorIndication, errorStatus, errorIndex, varBinds = next(
            pysnmp.hlapi.getCmd(
                # Create the SNMP engine
                pysnmp.hlapi.SnmpEngine(),
                # Authentication: set the SNMP version (2c) and community-string
                pysnmp.hlapi.CommunityData(community, mpModel=1),
                # Set the transport and target: UDP, hostname:port
                pysnmp.hlapi.UdpTransportTarget((hostname, port)),
                # Context is a v3 thing, but appears to be required anyway
                pysnmp.hlapi.ContextData(),
                # Specify the MIB object to read.
                # The 0 means we're retrieving a scalar value.
                pysnmp.hlapi.ObjectType(pysnmp.hlapi.ObjectIdentity(mib, attr, 0))))
    # Handle the responses
    if errorIndication:
        logger.error(errorIndication)
        return False
    elif errorStatus:
        logger.error('%s at %s' % (errorStatus.prettyPrint(),
                        errorIndex and varBinds[int(errorIndex) - 1][0] or '?'))
        return False
    # If we actually got something, return it in human-readable form
    else:
        return varBinds[0][1].prettyPrint()

def snmpBulkGet(hostname, mib, attr, community, port=161):
    '''
    Perform an SNMP BULKGET on mib::attr.
    Return a 2-level dict:
    - rowname
        - index = value
    This structure mirrors SNMP's representation of tables as rows with indexed values.
    '''
    logger.debug('Querying %s for %s::%s', hostname, mib, attr)
    # Number of nonrepeating MIB variables in the request
    nonRepeaters = 0
    # Maximum number of variables requested for each of the remaining MIB variables in the request
    maxRepetitions = 10000
    # Use pysnmp to retrieve the data
    data={} # Accumulator for the results
    for (errorIndication,
            errorStatus,
            errorIndex,
            varBinds) in pysnmp.hlapi.bulkCmd(
                    # Create the SNMP engine
                    pysnmp.hlapi.SnmpEngine(),
                    # Authentication: set the SNMP version (2c) and community-string
                    pysnmp.hlapi.CommunityData(community, mpModel=1),
                    # Set the transport and target: UDP, hostname:port
                    pysnmp.hlapi.UdpTransportTarget((hostname, port)),
                    # Context is a v3 thing, but appears to be required anyway
                    pysnmp.hlapi.ContextData(),
                    # Specify operational limits
                    nonRepeaters,
                    maxRepetitions,
                    # Specify the MIB object to read.
                    # The 0 means we're retrieving a scalar value.
                    pysnmp.hlapi.ObjectType(pysnmp.hlapi.ObjectIdentity(mib, attr)),
                    # Stop when we get results outside the scope we requested,
                    # instead of carrying on until the agent runs out of OIDs to send back.
                    lexicographicMode=False):
        # Handle the responses
        if errorIndication:
            logger.error(errorIndication)
            return False
        elif errorStatus:
            logger.error('%s at %s' % (errorStatus.prettyPrint(), errorIndex and varBinds[int(errorIndex) - 1][0] or '?'))
            return False
        # If we actually got something, return it in human-readable form
        else:
            for varBind in varBinds:
                # Extract the index values.
                # We're breaking down 'IF-MIB::ifType.530' into (row='ifType', index='530')
                keys = re.split('\.', re.split('::', varBind[0].prettyPrint())[1], maxsplit=1)
                row = keys[0]
                index = keys[1]
                # Now get the value
                value = varBind[1].prettyPrint()
                # Update the results table, ensuring the row is actually present
                logger.debug('%s.%s = %s', row, index, value)
                if row not in data:
                    data[row] = {}
                data[row][index] = value
    # Return what we found
    return data


# Functions to actually get the data

def identifyHost(hostname, community='public'):
    '''
    Extract some general identifying characteristics.
    Return a dict:
    - sysName      # Hostname. Should be the FDQN, but don't count on it.
    - sysDescr     # Detailed text description of the system.
    - sysObjectID  # Vendor's OID identifying the device.
    - sysServices  # Network-layer services offered by this device. Uses weird maths, but may be usable.
    '''
    logger.debug('Querying %s for general details', hostname)
    data={}
    for attr in [
            'sysName',
            'sysDescr',
            'sysObjectID',
            'sysServices',
            ]:
        response=snmpGet(hostname, 'SNMPv2-MIB', attr, community=community)
        if response:
            data[attr]=response
    return data

def getIfStackTable(hostname, community):
    '''
    Extract IF-MIB::ifStackTable from a device, per
    http://www.net-snmp.org/docs/mibs/ifMIBObjects.html
    and return it as a dict, where the key is the higher layer, and
    the value is the lower.
    '''
    logger.debug('Attempting to query %s for ifStackTable', hostname)
    data = {}
    try:
        rawdata = snmpBulkGet(hostname, 'IF-MIB', 'ifStackTable', community).items()
        logger.debug('rawdata: %s', rawdata)
        for raw, status in rawdata:
            stackparts=re.split('\.', raw)
            data[stackparts[0]] = stackparts[1]
        logger.debug('ifStackTable: %s', data)
        return data
    except:
        return False

def getInvStackTable(stack):
    '''
    Generate a mapping of interfaces to their subinterfaces,
    based on the dict returned by getifStackTable.
    Return a dict:
    - key = SNMP index of an interface
    - value = list of indices of inter
    '''
    data={}
    # Get the flat map
    # This returns a dict:
    # - SNMP index of parent interface
    # - list of indices of subinterfaces of that parent
    for upper, lower in stack.items():
        if lower not in data:
            data[lower] = []
        data[lower].append(upper)
    # Now turn that into a nested dict, so we have all the interdependencies mapped.
    # Start at subinterface '0', because that's how SNMP identifies "no interface here."
    return data

def ifInvStackTableToNest(table, index='0'):
    '''
    Take a table as output by the first section of getInvStackTable().
    Return a recursively nested dict:
    - key = SNMP index of an interface
    - value = False if this interface has no subinterfaces.
              If it _does_ have subinterfaces, a dict whose keys are their indices
    '''
    # If the value for this index is 0, this interface has no subinterfaces.
    # Return False to indicate this.
    if table[index] == '0':
        return False
    # Otherwise, there are subinterfaces to enumerate.
    # Recurse through this function.
    else:
        acc = {}
        for sub in table[index]:
            acc[sub] = ifInvStackTableToNest(table, sub)
        return acc

def getIfaceAddrMap(hostname, community):
    '''
    Extract a mapping of addresses to interfaces.
    Return a structure contained in a parent dict:
    - interface index (for reconciling with other interface data)
        - list of address dicts
            - address
            - netmask
    Tested only on Juniper SRX100 so far.
    '''
    addrIndex = snmpBulkGet(hostname, 'IP-MIB', 'ipAdEntIfIndex', community)['ipAdEntIfIndex']
    addrNetmask = snmpBulkGet(hostname, 'IP-MIB', 'ipAdEntNetMask', community)['ipAdEntNetMask']
    # SNMP returns this to us by address not interface.
    # Thus, we have to build an address-oriented dict first, then assemble the final result.
    acc = {}
    # Addresses
    for addr, index in addrIndex.items():
        acc[addr] = {'index': index}
    # Netmasks
    for addr, mask in addrNetmask.items():
        acc[addr]['netmask'] = mask
    result = {}
    for addr, details in acc.items():
        if details['index'] not in result:
            result[details['index']] = []
        result[details['index']].append({'address': addr, 'netmask': details['netmask']})
    return result

def discoverNetwork(hostname, community):
    '''
    Extract the device's network details, and return them as a nested structure:
    - interfaces
        - <SNMP index>
            - ifName    # Short name of the interface, in contrast to ifDescr
            - ifDescr   # Detailed text description of the interface
            - ifAlias   # Description string as configured by an administrator for this interface.
            - ifType    # IANA-specified interface type
            - ifSpeed   # reports the max speed in bits/second.
                        # If a 32-bit gauge is too small to report the speed, this should be
                        # set to the max possible value (4,294,967,295) and ifHighSpeed must
                        # be used instead.
            - ifHighSpeed   # ifHighSpeed is an estimate of the interface's current bandwidth
                            # in units of 1,000,000 bits per second. Zero for subinterfaces
                            # with no concept of bandwidth.
            - ifPhysAddress    # E.g. MAC address for an 802.x interface
    - ifStackTable      # Contents of the ifStackTable SNMP table for the device if it was returned.
                        # False if nothing was returned.
    - ifIfaceAddrMap      # Mapping of addresses to interface indices
        - output of getIfaceAddrMap()
    # The following entries will only be present if ifStackTable is not False:
    - ifStackTree       # Mapping of parent interfaces to subinterfaces
        - output of stackToDict()
    '''
    network = {'interfaces': {} }
    # Basic interface details
    ifTable = snmpBulkGet(hostname, 'IF-MIB', 'ifTable', community)
    for row in [
            'ifDescr',
            'ifType',
            'ifSpeed',
            'ifPhysAddress',
            ]:
        for index, value in ifTable[row].items():
            if index not in network['interfaces']:
                network['interfaces'][index] = {}
            network['interfaces'][index][row] = value
    # Extended interface details
    ifXTable = snmpBulkGet(hostname, 'IF-MIB', 'ifXTable', community)
    for row in [
            'ifName',
            'ifHighSpeed',
            'ifAlias',
            ]:
        # We should be able to assume that the index is already there by now.
        # If it isn't, we really do have a problem.
        for index, value in ifXTable[row].items():
            network['interfaces'][index][row] = value
    # Map addresses to interfaces
    network['ifIfaceAddrMap'] = getIfaceAddrMap(hostname, community)
    # ifStackTable encodes the relationship between subinterfaces and their parents.
    stack = getIfStackTable(hostname, community)
    if stack:
        network['ifStackTable'] = getInvStackTable(stack)
        network['ifStackTree'] = ifInvStackTableToNest(network['ifStackTable'])
    # Return all the stuff we discovered
    return network

def exploreDevice(hostname, community='public'):
    '''
    Build up a picture of a device via SNMP queries.
    Return the results as a nest of dicts:
    - sysinfo: output of identifyHost()
    - network: output of discoverNetwork()
    '''
    logger.info('Performing discovery on %s', hostname)
    # Dict to hold the device's information
    device={}
    # Top-level system information
    device['sysinfo'] = identifyHost(hostname, community)
    # Interfaces
    device['network'] = discoverNetwork(hostname, community)
    # Return the information we found
    return device

def populate_interfaces_flat(hostname, interfaces, ifaceaddrmap):
    '''
    Add interface details to a device.
    Just attach each interface directly to the device, without making any attempt
    to distinguish between subinterfaces and parents.
    '''
    for index, details in interfaces.items():
        ifurl = '%s/devices/%s/Interfaces' % (SYSCAT_URI, hostname)
        logger.debug('Attempting to add network interface %s to device %s at URL %s', details['ifName'], hostname, ifurl)
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
        logger.debug('result of interface creation for %s (%s): %s - %s' % (index, details['ifName'], netresponse.status_code, netresponse.text))
        # Add IPv4 addresses
        if str(index) in ifaceaddrmap:    # Not all interfaces have addresses
            for addr in ifaceaddrmap[str(index)]:
                ipurl = '%s/devices/%s/Interfaces/networkInterfaces/%s/Addresses' % (SYSCAT_URI, hostname, sanitise_uid(details['ifName']))
                logger.debug('Attempting to create IPv4 Address %s under URL %s' % (addr['address'], ipurl))
                addresponse = requests.post(
                        ipurl,
                        data={
                            'type': 'ipv4Addresses',
                            'uid': addr['address'],
                            'netmask': addr['netmask'],
                            }
                        )
            logger.debug('result of address creation for %s: %s - %s' % (addr['address'], addresponse.status_code, addresponse.text))
        else:
            logger.debug('No addresses found for interface with index number %s; moving on.' % str(index))

def populate_interface_tree(hostname, interfaces, ifstacktree, ifaceaddrmap):
    '''
    Add interfaces to a device in a nested style,
    so that subinterfaces are linked to their parent interfaces rather than the device.
    '''
    # Top-level
    for index, subs in ifstacktree.items():
        details = interfaces[index]
        ifurl = '%s/devices/%s/Interfaces' % (SYSCAT_URI, hostname)
        logger.debug('Attempting to create top-level network interface %s at URL %s', details['ifName'], ifurl)
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
                logger.debug('Attempting to create second-level network interface %s at URL %s' % (subdetails['ifName'], if2url))
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
                logger.debug('result of interface creation for %s (%s): %s - %s' % (index, subdetails['ifName'], subresponse.status_code, subresponse.text))
            # Add IPv4 addresses
            if str(sub) in ifaceaddrmap:    # Not all interfaces have addresses
                for addr in ifaceaddrmap[str(sub)]:
                    ip2url = '%s/devices/%s/Interfaces/networkInterfaces/%s/SubInterfaces/networkInterfaces/%s/Addresses' % (
                                SYSCAT_URI,
                                hostname,
                                sanitise_uid(details['ifName']),
                                sanitise_uid(subdetails['ifName']),
                                )
                    logger.debug('Attempting to create IPv4 Address %s under URL %s' % (addr['address'], ip2url))
                    addresponse = requests.post(
                            ip2url,
                            data={
                                'type': 'ipv4Addresses',
                                'uid': addr['address'],
                                'netmask': addr['netmask'],
                                }
                            )
                    logger.debug('result of address creation for %s: %s - %s' % (addr['address'], addresponse.status_code, addresponse.text))
            else:
                logger.debug('No addresses found for interface with index number %s; moving on.' % str(index))

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
    logger.info('Populating Syscat with details for %s', host)
    # Create the device itself
    sysresponse = requests.post(
            '%s/devices' % (SYSCAT_URI),
            data={
                'uid': host,
                'sysdescr': device['sysinfo']['sysDescr'],
                },
            )
    logger.debug('result of device creation: %s - %s' % (sysresponse.status_code, sysresponse.text))
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
        logger.setLevel(logging.DEBUG)
        ch.setLevel(logging.DEBUG)
    # Do the job
    populateSyscat(exploreDevice(args.hostname, community=args.community))
