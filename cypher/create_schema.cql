// Syscat schema for the Restagraph engine

// Resources
//
// L0
CREATE (:rgResource {name: 'any', notes: 'Represents any type of resource'});
CREATE (t:rgResource {name: 'tags', notes: 'For categorising any kind of resource'});
// L1
CREATE (:rgResource {name: 'country', notes: 'The geographical variety'});
CREATE (:rgResource {name: 'state', notes: 'The geographical variety', dependent: 'true'});
CREATE (:rgResource {name: 'city', notes: 'The geographical variety', dependent: 'true'});
CREATE (s:rgResource {name: 'sites', notes: 'Like a campus: a site that contains buildings, or is expected to.'}),
    (s)-[:rgHasAttribute]->(:rgAttribute {name: 'longname', notes: 'A name to go with the sitecode'}),
    (s)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (b:rgResource {name: 'buildings', notes: 'An individual building', dependent: 'true'}),
    (b)-[:rgHasAttribute]->(:rgAttribute {name: 'addressline1'}),
    (b)-[:rgHasAttribute]->(:rgAttribute {name: 'addressline2'}),
    (b)-[:rgHasAttribute]->(:rgAttribute {name: 'postcode'}),
    (b)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (f:rgResource {name: 'floors', notes: 'Floors within a building', dependent: 'true'}),
    (f)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (r:rgResource {name: 'rooms', notes: 'Rooms within a floor of a building', dependent: 'true'}),
    (r)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (c:rgResource {name: 'cages', notes: 'Lockable areas within a room, containing racks. Usually found in data centres.', dependent: 'true'}),
    (c)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (c:rgResource {name: 'cabinets', notes: 'A frame for rack-mounting equipment in. May be in the form of a lockable cabinet.', dependent: 'true'}),
    (c)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (r:rgResource {name: 'racks', notes: 'The rack location within a cabinet where a device is mounted. Usually the topmost rack number, in the case of multi-unit-height hardware.', dependent: 'true'}),
    (r)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (d:rgResource {name: 'devices', notes: 'Any kind of device that interacts with a network, be it L1, L2 or L3'}),
    (d)-[:rgHasAttribute]->(:rgAttribute {name: 'sysdescr', notes: 'SNMP host description. Usually either long and detailed per the RFC, or just the hostname.'}),
    (d)-[:rgHasAttribute]->(:rgAttribute {name: 'snmp_community', notes: 'Community string for SNMPv2 polling/traps.'}),
    (d)-[:rgHasAttribute]->(:rgAttribute {name: 'serial_number'}),
    (d)-[:rgHasAttribute]->(:rgAttribute {name: 'asset_number'}),
    (d)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (t:rgResource {name: 'deviceTypes', notes: 'Type of device, e.g. physical, cluster or virtual.'}),
    (t)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (r:rgResource {name: 'deviceRoles', notes: 'Classification of the device, e.g. host, router or switch.'}),
    (r)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (l:rgResource {name: 'lifeCyclePhases', notes: 'Where the device currently is, in its lifecycle. E.g. ordered, installed, in service or decommissioned.'}),
    (l)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (v:rgResource {name: 'environments', notes: 'Development, testing, staging, production etc.'}),
    (v)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (l:rgResource {name: 'serviceLevels', notes: 'Sets the expectations for responses to issues, e.g. gold, silver, bronze, plastic.'}),
    (l)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (i:rgResource {name: 'networkInterfaces', dependent: 'true'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'snmp_index', notes: 'The index by which this interface is identified when querying the host device via SNMP.'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'ifname', notes: 'Short name of the interface. Usually the one by which the device knows it.'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'ifdescr', notes: 'SNMP-discovered detailed text description of the interface.'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'iftype', notes: 'SNMP-discovered interface type, as specified by the IANA in the IANAifType-MIB.'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'ifalias', notes: 'Description string as configured by an administrator.'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'ifspeed', notes: 'SNMP: max speed in bits/second, or the nominal speed if it has no reliable maximum. If a 32-bit integer is too small, this will be set to the max possible value (4,294,967,295) and ifHighSpeed will be used to give the max speed.'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'ifhighspeed', notes: 'An estimate of the current bandwidth in units of 1,000,000 bits per second. Zero for subinterfaces with no concept of bandwidth.'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'ifphysaddress', notes: 'Physical address as specified at this network layer, e.g. MAC address for an ethernet NIC.'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (l:rgResource {name: 'l1Links'}),
    (l)-[:rgHasAttribute]->(:rgAttribute {name: 'our_id'}),
    (l)-[:rgHasAttribute]->(:rgAttribute {name: 'vendor_id'}),
    (l)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (l:rgResource {name: 'l1LinkTypes'}),
    (l)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (l:rgResource {name: 'l1Circuits'}),
    (l)-[:rgHasAttribute]->(:rgAttribute {name: 'our_id'}),
    (l)-[:rgHasAttribute]->(:rgAttribute {name: 'vendor_id'}),
    (l)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (p:rgResource {name: 'parts', notes: 'Spare parts, swappable components, anything of the kind', dependent: 'true'}),
    (p)-[:rgHasAttribute]->(:rgAttribute {name: 'our_reference', notes: 'What we call it, regardless of what the manufacturer/vendor does.'}),
    (p)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (r:rgResource {name: 'operatingSystems', dependent: 'true'});
CREATE (r:rgResource {name: 'operatingSystemVersions', dependent: 'true'});
// L1/logical
CREATE (b:rgResource {name: 'brands', notes: 'Also known as "make".'}),
    (b)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (:rgResource {name: 'models', dependent: 'true'});
// L2
CREATE (:rgResource {name: 'vlans', dependent: 'true', notes: 'Layer 2 groupings, i.e. ethernet. Not to be confused with subnets.'});
CREATE (r:rgResource {name: 'l2Links'}),
    (r)-[:rgHasAttribute]->(:rgAttribute {name: 'our_id'}),
    (r)-[:rgHasAttribute]->(:rgAttribute {name: 'vendor_id'}),
    (r)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (r:rgResource {name: 'l2LinkTypes'}),
    (r)-[:rgHasAttribute]->(:rgAttribute {name: 'our_id'}),
    (r)-[:rgHasAttribute]->(:rgAttribute {name: 'vendor_id'}),
    (r)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (r:rgResource {name: 'l2Circuits'}),
    (r)-[:rgHasAttribute]->(:rgAttribute {name: 'our_id'}),
    (r)-[:rgHasAttribute]->(:rgAttribute {name: 'vendor_id'}),
    (r)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
//
// L3
CREATE (:rgResource {name: 'vrfGroups', dependent: 'true'});
CREATE (r:rgResource {name: 'routingInstances', dependent: 'true'}),
    (r)-[:rghasAttribute]->(:rgAttribute {name: 'notes'}),
    (r)-[:rghasAttribute]->(:rgAttribute {name: 'snmp_community', notes: 'Community string for SNMPv2 polling/traps.'});
CREATE (a:rgResource {name: 'ipv4Addresses', dependent: 'true'}),
    (a)-[:rgHasAttribute]->(:rgAttribute {name: 'prefixlength'}),
    (a)-[:rgHasAttribute]->(:rgAttribute {name: 'description'});
CREATE (s:rgResource {name: 'ipv4Subnets', dependent: 'true'}),
    (s)-[:rgHasAttribute]->(:rgAttribute {name: 'prefixlength'}),
    (s)-[:rgHasAttribute]->(:rgAttribute {name: 'description'});
CREATE (a:rgResource {name: 'ipv6Addresses', dependent: 'true'}),
    (a)-[:rgHasAttribute]->(:rgAttribute {name: 'prefixlength'}),
    (a)-[:rgHasAttribute]->(:rgAttribute {name: 'description'});
CREATE (s:rgResource {name: 'ipv6Subnets', dependent: 'true'}),
    (s)-[:rgHasAttribute]->(:rgAttribute {name: 'prefixlength'}),
    (s)-[:rgHasAttribute]->(:rgAttribute {name: 'description'});
CREATE (t:rgResource {name: 'l3Tunnels'}),
    (t)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (t:rgResource {name: 'l3TunnelTypes'}),
    (t)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
// L3/logical
CREATE (r:rgResource {name: 'asn', notes: 'Autonomous Systems. The UID should be the ASN, and the "name" should be its human-friendly tag.'}),
    (r)-[:rgHasAttribute]->(:rgAttribute {name: 'name', required: 'false'});
//
// Layer 8 - applications
CREATE (:rgResource {name: 'applications'});
//
// Layer 9 - people and politics
CREATE (p:rgResource {name: 'people', notes: 'UID = login or other short reference'}),
    (p)-[:rgHasAttribute]->(:rgAttribute {name: 'given_name'}),
    (p)-[:rgHasAttribute]->(:rgAttribute {name: 'surname'});
CREATE (m:rgResource {name: 'emailAddresses', dependent: 'true'}),
    (m)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (p:rgResource {name: 'phoneNumbers', dependent: 'true'}),
    (p)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (o:rgResource {name: 'organisations', notes: 'Any kind of organisation'}),
    (o)-[:rgHasAttribute]->(:rgAttribute {name: 'description'});
CREATE (c:rgResource {name: 'contracts', notes: 'Agreements to supply goods or services'}),
    (c)-[:rgHasAttribute]->(:rgAttribute {name: 'referencenumber'}),
    (c)-[:rgHasAttribute]->(:rgAttribute {name: 'summary'}),
    (c)-[:rgHasAttribute]->(:rgAttribute {name: 'startdate'}),
    (c)-[:rgHasAttribute]->(:rgAttribute {name: 'enddate'}),
    (c)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});



// Relationships
//
// L0
MATCH (a:rgResource {name: 'any'}), (t:rgResource {name: 'tags'}) MERGE (a)-[:Tags {cardinality: 'many:many'}]->(t);
// L1
MATCH (c:rgResource {name: 'country'}), (s:rgResource {name: 'state'}) MERGE (c)-[:Contains {dependent: 'true', cardinality: '1:many'}]->(s);
MATCH (c:rgResource {name: 'country'}), (s:rgResource {name: 'city'}) MERGE (c)-[:Contains {dependent: 'true', cardinality: '1:many'}]->(s);
MATCH (s:rgResource {name: 'state'}), (c:rgResource {name: 'city'}) MERGE (s)-[:Contains {dependent: 'true', cardinality: '1:many'}]->(c);
MATCH (s:rgResource {name: 'sites'}), (b:rgResource {name: 'buildings'}) MERGE (s)-[:Buildings {dependent: 'true', cardinality: '1:many'}]->(b);
MATCH (b:rgResource {name: 'buildings'}), (c:rgResource {name: 'country'}) MERGE (b)-[:Location {cardinality: 'many:1'}]->(c);
MATCH (b:rgResource {name: 'buildings'}), (s:rgResource {name: 'state'}) MERGE (b)-[:Location {cardinality: 'many:1'}]->(s);
MATCH (b:rgResource {name: 'buildings'}), (c:rgResource {name: 'city'}) MERGE (b)-[:Location {cardinality: 'many:1'}]->(c);
MATCH (b:rgResource {name: 'buildings'}), (f:rgResource {name: 'floors'}) MERGE (b)-[:Floors {dependent: 'true', cardinality: '1:many'}]->(f);
MATCH (b:rgResource {name: 'buildings'}), (r:rgResource {name: 'rooms'}) MERGE (b)-[:Rooms {dependent: 'true', cardinality: '1:many'}]->(r);
MATCH (f:rgResource {name: 'floors'}), (r:rgResource {name: 'rooms'}) MERGE (f)-[:Rooms {dependent: 'true', cardinality: '1:many'}]->(r);
MATCH (r:rgResource {name: 'rooms'}), (c:rgResource {name: 'cages'}) MERGE (r)-[:Cages {dependent: 'true', cardinality: '1:many'}]->(c);
MATCH (r:rgResource {name: 'rooms'}), (c:rgResource {name: 'cabinets'}) MERGE (r)-[:Cabinets {dependent: 'true', cardinality: '1:many'}]->(c);
MATCH (r:rgResource {name: 'cages'}), (c:rgResource {name: 'cabinets'}) MERGE (r)-[:Cabinets {dependent: 'true', cardinality: '1:many'}]->(c);
MATCH (c:rgResource {name: 'cabinets'}), (r:rgResource {name: 'racks'}) MERGE (c)-[:Racks {dependent: 'true', cardinality: '1:many'}]->(r);
MATCH (d:rgResource {name: 'devices'}), (o:rgResource {name: 'organisations'}) MERGE (d)-[:PurchasedFrom {cardinality: 'many:1'}]->(o);
MATCH (d:rgResource {name: 'devices'}), (s:rgResource {name: 'sites'}) MERGE (d)-[:Location {cardinality: 'many:1'}]->(s);
MATCH (d:rgResource {name: 'devices'}), (s:rgResource {name: 'buildings'}) MERGE (d)-[:Location {cardinality: 'many:1'}]->(s);
MATCH (d:rgResource {name: 'devices'}), (s:rgResource {name: 'floors'}) MERGE (d)-[:Location {cardinality: 'many:1'}]->(s);
MATCH (d:rgResource {name: 'devices'}), (s:rgResource {name: 'rooms'}) MERGE (d)-[:Location {cardinality: 'many:1'}]->(s);
MATCH (d:rgResource {name: 'devices'}), (s:rgResource {name: 'cages'}) MERGE (d)-[:Location {cardinality: 'many:1'}]->(s);
MATCH (d:rgResource {name: 'devices'}), (s:rgResource {name: 'cabinets'}) MERGE (d)-[:Location {cardinality: 'many:1'}]->(s);
MATCH (d:rgResource {name: 'devices'}), (s:rgResource {name: 'racks'}) MERGE (d)-[:Location {cardinality: 'many:1'}]->(s);
MATCH (r:rgResource {name: 'devices'}), (i:rgResource {name: 'networkInterfaces'}) MERGE (r)-[:Interfaces {dependent: 'true', cardinality: '1:many'}]->(i);
MATCH (r:rgResource {name: 'devices'}), (o:rgResource {name: 'operatingSystems'}) MERGE (r)-[:OperatingSystem {cardinality: 'many:1'}]->(o);
MATCH (r:rgResource {name: 'devices'}), (o:rgResource {name: 'operatingSystemVersions'}) MERGE (r)-[:OperatingSystem {cardinality: 'many:1'}]->(o);
MATCH (d:rgResource {name: 'devices'}) MERGE (d)-[:Member {cardinality: 'many:1'}]->(d);
MATCH (d:rgResource {name: 'devices'}) MERGE (d)-[:HostedOn {cardinality: 'many:1'}]->(d);
MATCH (i:rgResource {name: 'networkInterfaces'}) MERGE (i)-[:SubInterfaces {dependent: 'true', cardinality: '1:many'}]->(i);
MATCH (i:rgResource {name: 'networkInterfaces'}) MERGE (i)-[:Member {dependent: 'true', cardinality: 'many:1'}]->(i);
MATCH (i:rgResource {name: 'networkInterfaces'}) MERGE (i)-[:ConnectsTo {cardinality: '1:1'}]->(i);
MATCH (p:rgResource {name: 'parts'}), (o:rgResource {name: 'organisations'}) MERGE (p)-[:PurchasedFrom {cardinality: 'many:1'}]->(o);
MATCH (p:rgResource {name: 'parts'}), (m:rgResource {name: 'models'}) MERGE (p)-[:CompatibleWith {cardinality: 'many:many'}]->(m);
MATCH (p:rgResource {name: 'parts'}), (d:rgResource {name: 'devices'}) MERGE (p)-[:InstalledIn {cardinality: 'many:1'}]->(d);
MATCH (p:rgResource {name: 'parts'}), (l:rgResource {name: 'sites'}) MERGE (p)-[:StorageLocation {cardinality: 'many:1'}]->(l);
MATCH (p:rgResource {name: 'parts'}), (l:rgResource {name: 'buildings'}) MERGE (p)-[:StorageLocation {cardinality: 'many:1'}]->(l);
MATCH (p:rgResource {name: 'parts'}), (l:rgResource {name: 'rooms'}) MERGE (p)-[:StorageLocation {cardinality: 'many:1'}]->(l);
MATCH (p:rgResource {name: 'parts'}), (l:rgResource {name: 'cages'}) MERGE (p)-[:StorageLocation {cardinality: 'many:1'}]->(l);
MATCH (p:rgResource {name: 'parts'}), (l:rgResource {name: 'cabinets'}) MERGE (p)-[:StorageLocation {cardinality: 'many:1'}]->(l);
MATCH (d:rgResource {name: 'devices'}), (l:rgResource {name: 'l1Links'}) MERGE (d)-[:ConnectsTo {cardinality: 'many:many'}]->(l);
MATCH (i:rgResource {name: 'networkInterfaces'}), (l:rgResource {name: 'l1Links'}) MERGE (i)-[:ConnectsTo {cardinality: 'many:1'}]->(l);
MATCH (d:rgResource {name: 'devices'}), (l:rgResource {name: 'l1Circuits'}) MERGE (d)-[:ConnectsTo {cardinality: 'many:many'}]->(l);
MATCH (i:rgResource {name: 'networkInterfaces'}), (l:rgResource {name: 'l1Circuits'}) MERGE (i)-[:ConnectsTo {cardinality: 'many:1'}]->(l);
MATCH (s:rgResource {name: 'l1Links'}) MERGE (s)-[:ConnectsTo {cardinality: 'many:many'}]->(s);
MATCH (s:rgResource {name: 'l1Links'}), (c:rgResource {name: 'l1LinkTypes'}) MERGE (s)-[:Type {cardinality: 'many:1'}]->(c);
MATCH (s:rgResource {name: 'l1Links'}), (c:rgResource {name: 'l1Circuits'}) MERGE (s)-[:MemberOf {cardinality: 'many:1'}]->(c);
//
// L1/logical
MATCH (r:rgResource {name: 'devices'}), (a:rgResource {name: 'routingInstances'}) MERGE (r)-[:RoutingInstances {dependent: 'true', cardinality: 'many:1'}]->(a);
MATCH (a:rgResource {name: 'routingInstances'}), (i:rgResource {name: 'networkInterfaces'}) MERGE (a)-[:Interfaces {cardinality: '1:many'}]->(i);
MATCH (r:rgResource {name: 'routingInstances'}), (v:rgResource {name: 'vrfGroups'}) MERGE (r)-[:Member {cardinality: 'many:1'}]->(v);
MATCH (b:rgResource {name: 'brands'}), (o:rgResource {name: 'operatingSystems'}) MERGE (b)-[:Produces {dependent: 'true', cardinality: '1:many'}]->(o);
MATCH (o:rgResource {name: 'operatingSystems'}), (v:rgResource {name: 'operatingSystemVersions'}) MERGE (o)-[:Versions {dependent: 'true', cardinality: '1:many'}]->(v);
MATCH (b:rgResource {name: 'brands'}), (m:rgResource {name: 'models'}) MERGE (b)-[:Models {dependent: 'true', cardinality: '1:many'}]->(m);
MATCH (r:rgResource {name: 'devices'}), (m:rgResource {name: 'models'}) MERGE (r)-[:Model {cardinality: 'many:1'}]->(m);
MATCH (r:rgResource {name: 'models'}), (m:rgResource {name: 'deviceRoles'}) MERGE (r)-[:Capability {cardinality: 'many:many'}]->(m);
// Type is a reserved word in Cypher, so we use the longer-form DeviceType for the relationship
MATCH (r:rgResource {name: 'devices'}), (m:rgResource {name: 'deviceTypes'}) MERGE (r)-[:DeviceType {cardinality: 'many:1'}]->(m);
MATCH (r:rgResource {name: 'devices'}), (m:rgResource {name: 'deviceRoles'}) MERGE (r)-[:Role {cardinality: 'many:many'}]->(m);
MATCH (d:rgResource {name: 'devices'}), (p:rgResource {name: 'lifeCyclePhases'}) MERGE (d)-[:LifeCyclePhase {cardinality: 'many:1'}]->(p);
MATCH (d:rgResource {name: 'devices'}), (v:rgResource {name: 'environments'}) MERGE (d)-[:Environment {cardinality: 'many:1'}]->(v);
MATCH (d:rgResource {name: 'devices'}), (l:rgResource {name: 'serviceLevels'}) MERGE (d)-[:ServiceLevel {cardinality: 'many:1'}]->(l);
MATCH (b:rgResource {name: 'brands'}), (p:rgResource {name: 'parts'}) MERGE (b)-[:PartNumber {dependent: 'true', cardinality: 'many:1'}]->(p);
//
// L1->L2
MATCH (d:rgResource {name: 'devices'}), (v:rgResource {name: 'vlans'}) MERGE (d)-[:Vlan {dependent: 'true', cardinality: '1:many'}]->(v);
MATCH (i:rgResource {name: 'networkInterfaces'}), (v:rgResource {name: 'vlans'}) MERGE (i)-[:Vlan {dependent: 'true', cardinality: '1:many'}]->(v);
MATCH (d:rgResource {name: 'devices'}), (l:rgResource {name: 'l2Links'}) MERGE (d)-[:ConnectsTo {cardinality: 'many:many'}]->(l);
MATCH (i:rgResource {name: 'networkInterfaces'}), (l:rgResource {name: 'l2Links'}) MERGE (i)-[:ConnectsTo {cardinality: 'many:1'}]->(l);
MATCH (d:rgResource {name: 'devices'}), (l:rgResource {name: 'l2Circuits'}) MERGE (d)-[:ConnectsTo {cardinality: 'many:many'}]->(l);
MATCH (i:rgResource {name: 'networkInterfaces'}), (l:rgResource {name: 'l2Circuits'}) MERGE (i)-[:ConnectsTo {cardinality: 'many:1'}]->(l);
//
// L1->L3
MATCH (d:rgResource {name: 'devices'}), (a:rgResource {name: 'ipv4Addresses'}) MERGE (d)-[:Addresses {dependent: 'true', cardinality: '1:many'}]->(a);
MATCH (d:rgResource {name: 'devices'}), (a:rgResource {name: 'ipv6Addresses'}) MERGE (d)-[:Addresses {dependent: 'true', cardinality: '1:many'}]->(a);
MATCH (i:rgResource {name: 'networkInterfaces'}), (a:rgResource {name: 'ipv4Addresses'}) MERGE (i)-[:Addresses {dependent: 'true', cardinality: '1:many'}]->(a);
MATCH (i:rgResource {name: 'networkInterfaces'}), (a:rgResource {name: 'ipv6Addresses'}) MERGE (i)-[:Addresses {dependent: 'true', cardinality: '1:many'}]->(a);
MATCH (d:rgResource {name: 'devices'}), (r:rgResource {name: 'routingInstances'}) MERGE (d)-[:RoutingInstance {dependent: 'true', cardinality: '1:many'}]->(r);
MATCH (d:rgResource {name: 'devices'}), (t:rgResource {name: 'l3Tunnels'}) MERGE (d)-[:ConnectsTo {cardinality: '1:many'}]->(t);
MATCH (i:rgResource {name: 'networkInterfaces'}), (t:rgResource {name: 'l3Tunnels'}) MERGE (i)-[:ConnectsTo {cardinality: 'many:1'}]->(t);
//
// L1->L9
MATCH (b:rgResource {name: 'brands'}), (o:rgResource {name: 'organisations'}) MERGE (b)-[:Vendor {cardinality: 'many:1'}]->(o);
MATCH (r:rgResource {name: 'devices'}), (p:rgResource {name: 'people'}) MERGE (r)-[:TechnicalOwner {cardinality: 'many:1'}]->(p);
MATCH (r:rgResource {name: 'devices'}), (p:rgResource {name: 'people'}) MERGE (r)-[:BusinessOwner {cardinality: 'many:1'}]->(p);
MATCH (r:rgResource {name: 'devices'}), (o:rgResource {name: 'organisations'}) MERGE (r)-[:BusinessOwner {cardinality: 'many:1'}]->(o);
MATCH (c:rgResource {name: 'l1Links'}), (o:rgResource {name: 'organisations'}) MERGE (c)-[:ProvisionedBy {cardinality: 'many:1'}]-(o);
MATCH (c:rgResource {name: 'l1Links'}), (o:rgResource {name: 'organisations'}) MERGE (c)-[:OperatedBy {cardinality: 'many:1'}]-(o);
MATCH (c:rgResource {name: 'l1Circuits'}), (o:rgResource {name: 'organisations'}) MERGE (c)-[:ProvisionedBy {cardinality: 'many:1'}]-(o);
MATCH (c:rgResource {name: 'l1Circuits'}), (o:rgResource {name: 'organisations'}) MERGE (c)-[:OperatedBy {cardinality: 'many:1'}]-(o);
//
// L2
MATCH (l:rgResource {name: 'l2Links'}) MERGE (l)-[:ConnectsTo {cardinality: 'many:many'}]->(l);
MATCH (l:rgResource {name: 'l2Links'}), (t:rgResource {name: 'l2LinkTypes'}) MERGE (l)-[:Type {cardinality: 'many:1'}]->(t);
MATCH (l:rgResource {name: 'l2Links'}), (c:rgResource {name: 'l2Circuits'}) MERGE (l)-[:MemberOf {cardinality: 'many:1'}]->(c);
//
// L2->L9
MATCH (c:rgResource {name: 'l2Links'}), (o:rgResource {name: 'organisations'}) MERGE (c)-[:ProvisionedBy {cardinality: 'many:1'}]-(o);
MATCH (c:rgResource {name: 'l2Links'}), (o:rgResource {name: 'organisations'}) MERGE (c)-[:OperatedBy {cardinality: 'many:1'}]-(o);
MATCH (c:rgResource {name: 'l2Circuits'}), (o:rgResource {name: 'organisations'}) MERGE (c)-[:ProvisionedBy {cardinality: 'many:1'}]-(o);
MATCH (c:rgResource {name: 'l2Circuits'}), (o:rgResource {name: 'organisations'}) MERGE (c)-[:OperatedBy {cardinality: 'many:1'}]-(o);
//
// L3
MATCH (a:rgResource {name: 'asn'}), (v:rgResource {name: 'vrfGroups'}) MERGE (a)-[:VrfGroups {dependent: 'true', cardinality: '1:many'}]->(v);
MATCH (v:rgResource {name: 'vrfGroups'}), (s:rgResource {name: 'ipv4Subnets'}) MERGE (v)-[:Subnets {dependent: 'true', cardinality: '1:many'}]->(s);
MATCH (v:rgResource {name: 'vrfGroups'}), (s:rgResource {name: 'ipv6Subnets'}) MERGE (v)-[:Subnets {dependent: 'true', cardinality: '1:many'}]->(s);
MATCH (a:rgResource {name: 'asn'}), (a:rgResource {name: 'ipv4Addresses'}) MERGE (a)-[:Addresses {dependent: 'true', cardinality: '1:many'}]->(a);
MATCH (a:rgResource {name: 'asn'}), (a:rgResource {name: 'ipv6Addresses'}) MERGE (a)-[:Addresses {dependent: 'true', cardinality: '1:many'}]->(a);
MATCH (s:rgResource {name: 'ipv4Subnets'}), (a:rgResource {name: 'ipv4Addresses'}) MERGE (s)-[:Addresses {dependent: 'true', cardinality: '1:many'}]->(a);
MATCH (s:rgResource {name: 'ipv6Subnets'}), (a:rgResource {name: 'ipv6Addresses'}) MERGE (s)-[:Addresses {dependent: 'true', cardinality: '1:many'}]->(a);
MATCH (a:rgResource {name: 'asn'}), (s:rgResource {name: 'ipv4Subnets'}) MERGE (a)-[:Subnets {dependent: 'true', cardinality: '1:many'}]->(s);
MATCH (a:rgResource {name: 'asn'}), (s:rgResource {name: 'ipv6Subnets'}) MERGE (a)-[:Subnets {dependent: 'true', cardinality: '1:many'}]->(s);
MATCH (s:rgResource {name: 'ipv4Subnets'}) MERGE (s)-[:Subnets {dependent: 'true', cardinality: '1:many'}]->(s);
MATCH (s:rgResource {name: 'ipv6Subnets'}) MERGE (s)-[:Subnets {dependent: 'true', cardinality: '1:many'}]->(s);
MATCH (a:rgResource {name: 'ipv4Addresses'}), (t:rgResource {name: 'l3Tunnels'}) MERGE (a)-[:ConnectsTo {cardinality: 'many:1'}]->(t);
MATCH (a:rgResource {name: 'ipv6Addresses'}), (t:rgResource {name: 'l3Tunnels'}) MERGE (a)-[:ConnectsTo {cardinality: 'many:1'}]->(t);
MATCH (t:rgResource {name: 'l3Tunnels'}), (y:rgResource {name: 'l3TunnelTypes'}) MERGE (t)-[:Type {cardinality: 'many:1'}]->(y);
//
// L3/logical
// L3->L9
MATCH (a:rgResource {name: 'asn'}), (o:rgResource {name: 'organisations'}) MERGE (a)-[:Owner {cardinality: 'many:1'}]->(o);
MATCH (s:rgResource {name: 'ipv4Subnets'}), (o:rgResource {name: 'organisations'}) MERGE (s)-[:Owner {cardinality: 'many:1'}]->(o);
MATCH (s:rgResource {name: 'ipv6Subnets'}), (o:rgResource {name: 'organisations'}) MERGE (s)-[:Owner {cardinality: 'many:1'}]->(o);
//
// L8
MATCH (a:rgResource {name: 'applications'}) MERGE (a)-[:DependsOn {cardinality: 'many:many'}]->(a);
//
// L8->L1
MATCH (a:rgResource {name: 'applications'}), (h:rgResource {name: 'devices'}) MERGE (a)-[:RunsOn {cardinality: 'many:many'}]->(h);
//
// L8->L9
MATCH (a:rgResource {name: 'applications'}), (p:rgResource {name: 'people'}) MERGE (a)-[:BusinessOwner {cardinality: 'many:1'}]->(p);
MATCH (a:rgResource {name: 'applications'}), (p:rgResource {name: 'people'}) MERGE (a)-[:TechnicalOwner {cardinality: 'many:1'}]->(p);
//
// L9
MATCH (o:rgResource {name: 'organisations'}), (m:rgResource {name: 'emailAddresses'}) MERGE (o)-[:ContactMethods {dependent: 'true', cardinality: '1:many'}]->(m);
MATCH (p:rgResource {name: 'people'}), (m:rgResource {name: 'emailAddresses'}) MERGE (p)-[:ContactMethods {dependent: 'true', cardinality: '1:many'}]->(m);
MATCH (p:rgResource {name: 'people'}), (n:rgResource {name: 'phoneNumbers'}) MERGE (p)-[:ContactMethods {dependent: 'true', cardinality: '1:many'}]->(n);
MATCH (p:rgResource {name: 'people'}), (o:rgResource {name: 'organisations'}) MERGE (p)-[:Member {cardinality: '1:many'}]->(o);
MATCH (p:rgResource {name: 'people'}), (o:rgResource {name: 'organisations'}) MERGE (p)-[:AccountManager {cardinality: '1:many'}]->(o);
MATCH (p:rgResource {name: 'people'}), (o:rgResource {name: 'organisations'}) MERGE (p)-[:SalesContact {cardinality: '1:many'}]->(o);
MATCH (p:rgResource {name: 'people'}), (o:rgResource {name: 'organisations'}) MERGE (p)-[:SupportContact {cardinality: '1:many'}]->(o);
MATCH (o:rgResource {name: 'organisations'}) MERGE (o)-[:Member {cardinality: 'many:many'}]->(o);
MATCH (c:rgResource {name: 'contracts'}) MERGE (c)-[:Subcontracts {cardinality: '1:many'}]->(c);
MATCH (c:rgResource {name: 'contracts'}), (o:rgResource {name: 'organisations'}) MERGE (c)-[:Vendor {cardinality: 'many:many'}]->(o);
MATCH (c:rgResource {name: 'contracts'}), (o:rgResource {name: 'organisations'}) MERGE (c)-[:Customer {cardinality: 'many:many'}]->(o);
MATCH (c:rgResource {name: 'contracts'}), (p:rgResource {name: 'people'}) MERGE (c)-[:NegotiatedBy {cardinality: 'many:many'}]->(p);
MATCH (c:rgResource {name: 'contracts'}), (d:rgResource {name: 'devices'}) MERGE (c)-[:Supplies {cardinality: '1:many'}]->(d);
MATCH (c:rgResource {name: 'contracts'}), (d:rgResource {name: 'devices'}) MERGE (c)-[:Supports {cardinality: '1:many'}]->(d);
MATCH (c:rgResource {name: 'contracts'}), (p:rgResource {name: 'parts'}) MERGE (c)-[:Supplies {cardinality: '1:many'}]->(p);
MATCH (c:rgResource {name: 'contracts'}), (p:rgResource {name: 'parts'}) MERGE (c)-[:Supports {cardinality: '1:many'}]->(p);
MATCH (c:rgResource {name: 'contracts'}), (b:rgResource {name: 'buildings'}) MERGE (c)-[:Supplies {cardinality: '1:many'}]->(b);
MATCH (c:rgResource {name: 'contracts'}), (b:rgResource {name: 'buildings'}) MERGE (c)-[:Supports {cardinality: 'many:many'}]->(b);
MATCH (c:rgResource {name: 'contracts'}), (f:rgResource {name: 'floors'}) MERGE (c)-[:Supplies {cardinality: 'many:many'}]->(f);
MATCH (c:rgResource {name: 'contracts'}), (f:rgResource {name: 'floors'}) MERGE (c)-[:Supports {cardinality: 'many:many'}]->(f);
MATCH (c:rgResource {name: 'contracts'}), (r:rgResource {name: 'rooms'}) MERGE (c)-[:Supplies {cardinality: '1:many'}]->(r);
MATCH (c:rgResource {name: 'contracts'}), (r:rgResource {name: 'rooms'}) MERGE (c)-[:Supports {cardinality: '1:many'}]->(r);
MATCH (c:rgResource {name: 'contracts'}), (r:rgResource {name: 'cages'}) MERGE (c)-[:Supplies {cardinality: '1:many'}]->(r);
MATCH (c:rgResource {name: 'contracts'}), (r:rgResource {name: 'cages'}) MERGE (c)-[:Supports {cardinality: '1:many'}]->(r);
MATCH (c:rgResource {name: 'contracts'}), (r:rgResource {name: 'cabinets'}) MERGE (c)-[:Supplies {cardinality: '1:many'}]->(r);
MATCH (c:rgResource {name: 'contracts'}), (r:rgResource {name: 'cabinets'}) MERGE (c)-[:Supports {cardinality: '1:many'}]->(r);
// L9->L1
MATCH (b:rgResource {name: 'sites'}), (o:rgResource {name: 'organisations'}) MERGE (b)-[:Organisation {cardinality: 'many:1'}]->(o);
MATCH (b:rgResource {name: 'buildings'}), (o:rgResource {name: 'organisations'}) MERGE (b)-[:Tenant {cardinality: 'many:1'}]->(o);
MATCH (b:rgResource {name: 'floors'}), (o:rgResource {name: 'organisations'}) MERGE (b)-[:Tenant {cardinality: 'many:1'}]->(o);
MATCH (b:rgResource {name: 'rooms'}), (o:rgResource {name: 'organisations'}) MERGE (b)-[:Tenant {cardinality: 'many:1'}]->(o);
MATCH (b:rgResource {name: 'cages'}), (o:rgResource {name: 'organisations'}) MERGE (b)-[:Tenant {cardinality: 'many:1'}]->(o);
MATCH (b:rgResource {name: 'cabinets'}), (o:rgResource {name: 'organisations'}) MERGE (b)-[:Tenant {cardinality: 'many:1'}]->(o);
