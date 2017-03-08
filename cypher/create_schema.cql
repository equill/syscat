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
    (s)-[:rgHasAttribute]->(:rgAttribute {name: 'longName', notes: 'A name to go with the sitecode'}),
    (s)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (b:rgResource {name: 'buildings', notes: 'An individual building', dependent: 'true'}),
    (b)-[:rgHasAttribute]->(:rgAttribute {name: 'addressLine1'}),
    (b)-[:rgHasAttribute]->(:rgAttribute {name: 'addressLine2'}),
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
    (d)-[:rgHasAttribute]->(:rgAttribute {name: 'description'}),
    (d)-[:rgHasAttribute]->(:rgAttribute {name: 'serial_number'}),
    (d)-[:rgHasAttribute]->(:rgAttribute {name: 'asset_number'}),
    (d)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (t:rgResource {name: 'deviceTypes', notes: 'Type of device, e.g. physical, cluster or virtual.'}),
    (t)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (r:rgResource {name: 'deviceRoles', notes: 'Classification of the device, e.g. host, router or switch.'}),
    (r)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (l:rgResource {name: 'lifeCyclePhases', notes: 'Where the device currently is, in its lifecycle. E.g. ordered, installed, in service or decommissioned.', dependent: 'true'}),
    (l)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (v:rgResource {name: 'environments', notes: 'Development, testing, staging, production etc.', dependent: 'true'}),
    (v)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (l:rgResource {name: 'serviceLevels', notes: 'Sets the expectations for responses to issues, e.g. gold, silver, bronze, plastic.', dependent: 'true'}),
    (l)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (i:rgResource {name: 'ethernetInterfaces', dependent: 'true'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'mac_address'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'snmp_index'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'speed'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'alias'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'description'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (p:rgResource {name: 'parts', notes: 'Spare parts, swappable components, anything of the kind', dependent: 'true'}),
    (p)-[:rgHasAttribute]->(:rgAttribute {name: 'ourReference', notes: 'What we call it, regardless of what the manufacturer/vendor does.'}),
    (p)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (r:rgResource {name: 'operatingSystems', dependent: 'true'});
CREATE (r:rgResource {name: 'operatingSystemVersions', dependent: 'true'});
CREATE (r:rgResource {name: 'networkSegments'}), (r)-[:rgHasAttribute]->(:rgAttribute {name: 'theirReference'});
CREATE (r:rgResource {name: 'networkCircuits'}), (r)-[:rgHasAttribute]->(:rgAttribute {name: 'theirReference'});
// Network segment types
CREATE (r:rgResource {name: 'ethernet'});
CREATE (r:rgResource {name: 'fibreOpticCable'});
CREATE (r:rgResource {name: 'vLink'});
// L1/logical
CREATE (b:rgResource {name: 'brands'}),
    (b)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (:rgResource {name: 'models', dependent: 'true'});
// L2
CREATE (:rgResource {name: 'vlans', notes: 'Layer 2 groupings, i.e. ethernet. Not to be confused with subnets.'});
CREATE (g:rgResource {name: 'vlanGroups', notes: 'UID: name. VLAN ID goes in the VLAN_ID field - this deals with the problem where the same VLAN ID is used in different ways in different, disconnected, parts of the network.'}), (g)-[:rgHasAttribute]->(:rgAttribute {name: 'vlanid'});
// Network link types (e.g, Telco circuits)
//
// L3
CREATE (:rgResource {name: 'vrfGroups'});
CREATE (:rgResource {name: 'routingInstances'}); // FIXME should be dependent on a router
CREATE (a:rgResource {name: 'ipv4Addresses', dependent: 'true'}),
    (a)-[:rgHasAttribute]->(:rgAttribute {name: 'prefixlength'});
CREATE (s:rgResource {name: 'ipv4Subnets', dependent: 'true'}),
    (s)-[:rgHasAttribute]->(:rgAttribute {name: 'prefixlength'});
CREATE (a:rgResource {name: 'ipv6Addresses', dependent: 'true'}),
    (a)-[:rgHasAttribute]->(:rgAttribute {name: 'prefixlength'});
CREATE (s:rgResource {name: 'ipv6Subnets', dependent: 'true'}),
    (s)-[:rgHasAttribute]->(:rgAttribute {name: 'prefixlength'});
// DNS
CREATE (z:rgResource {name: 'dnsZones'}),
    (z)-[:rgHasAttribute]->(:rgAttribute {name: 'domain'});
CREATE (:rgResource {name: 'aRecord', dependent: 'true'});
CREATE (:rgResource {name: 'mxRecord', dependent: 'true'});
CREATE (:rgResource {name: 'aaaaRecord', dependent: 'true'});
CREATE (:rgResource {name: 'aliasRecord', dependent: 'true'});
// L3/logical
CREATE (b:rgResource {name: 'bgpInstances', dependent: 'true'});
CREATE (b:rgResource {name: 'bgpPeerings', dependent: 'true'});
CREATE (r:rgResource {name: 'asn', notes: 'Autonomous Systems. The UID should be the ASN, and the "name" should be its human-friendly tag.'}), (r)-[:rgHasAttribute]->(:rgAttribute {name: 'name', required: 'false'});
//
// Layer 8 - applications
CREATE (:rgResource {name: 'applications'});
//
// Layer 9 - people and politics
CREATE (p:rgResource {name: 'people', notes: 'UID = login or other short reference'}),
    (p)-[:rgHasAttribute]->(:rgAttribute {name: 'givenName'}),
    (p)-[:rgHasAttribute]->(:rgAttribute {name: 'surname'});
CREATE (m:rgResource {name: 'emailAddresses', dependent: 'true'}),
    (m)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (p:rgResource {name: 'phoneNumbers', dependent: 'true'}),
    (p)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});
CREATE (o:rgResource {name: 'organisations', notes: 'Any kind of organisation'}),
    (o)-[:rgHasAttribute]->(:rgAttribute {name: 'description'});
CREATE (c:rgResource {name: 'contracts', notes: 'Agreements to supply goods or services'}),
    (c)-[:rgHasAttribute]->(:rgAttribute {name: 'referenceNumber'}),
    (c)-[:rgHasAttribute]->(:rgAttribute {name: 'summary'}),
    (c)-[:rgHasAttribute]->(:rgAttribute {name: 'startDate'}),
    (c)-[:rgHasAttribute]->(:rgAttribute {name: 'endDate'}),
    (c)-[:rgHasAttribute]->(:rgAttribute {name: 'notes'});



// Relationships
//
// L0
MATCH (a:rgResource {name: 'any'}), (t:rgResource {name: 'tags'}) MERGE (a)-[:Tags]->(t);
// L1
MATCH (c:rgResource {name: 'country'}), (s:rgResource {name: 'state'}) MERGE (c)-[:Contains {dependent: 'true'}]->(s);
MATCH (c:rgResource {name: 'country'}), (s:rgResource {name: 'city'}) MERGE (c)-[:Contains {dependent: 'true'}]->(s);
MATCH (s:rgResource {name: 'state'}), (c:rgResource {name: 'city'}) MERGE (s)-[:Contains {dependent: 'true'}]->(c);
MATCH (s:rgResource {name: 'sites'}), (b:rgResource {name: 'buildings'}) MERGE (s)-[:Buildings {dependent: 'true'}]->(b);
MATCH (b:rgResource {name: 'buildings'}), (c:rgResource {name: 'country'}) MERGE (b)-[:Location]->(c);
MATCH (b:rgResource {name: 'buildings'}), (s:rgResource {name: 'state'}) MERGE (b)-[:Location]->(s);
MATCH (b:rgResource {name: 'buildings'}), (c:rgResource {name: 'city'}) MERGE (b)-[:Location]->(c);
MATCH (b:rgResource {name: 'buildings'}), (f:rgResource {name: 'floors'}) MERGE (b)-[:Floors {dependent: 'true'}]->(f);
MATCH (b:rgResource {name: 'buildings'}), (r:rgResource {name: 'rooms'}) MERGE (b)-[:Rooms {dependent: 'true'}]->(r);
MATCH (f:rgResource {name: 'floors'}), (r:rgResource {name: 'rooms'}) MERGE (f)-[:Rooms {dependent: 'true'}]->(r);
MATCH (r:rgResource {name: 'rooms'}), (c:rgResource {name: 'cages'}) MERGE (r)-[:Cages {dependent: 'true'}]->(c);
MATCH (r:rgResource {name: 'rooms'}), (c:rgResource {name: 'cabinets'}) MERGE (r)-[:Cabinets {dependent: 'true'}]->(c);
MATCH (r:rgResource {name: 'cages'}), (c:rgResource {name: 'cabinets'}) MERGE (r)-[:Cabinets {dependent: 'true'}]->(c);
MATCH (c:rgResource {name: 'cabinets'}), (r:rgResource {name: 'racks'}) MERGE (c)-[:Racks {dependent: 'true'}]->(r);
MATCH (d:rgResource {name: 'devices'}), (o:rgResource {name: 'organisations'}) MERGE (d)-[:PurchasedFrom]->(o);
MATCH (d:rgResource {name: 'devices'}), (s:rgResource {name: 'sites'}) MERGE (d)-[:Location]->(s);
MATCH (d:rgResource {name: 'devices'}), (s:rgResource {name: 'buildings'}) MERGE (d)-[:Location]->(s);
MATCH (d:rgResource {name: 'devices'}), (s:rgResource {name: 'floors'}) MERGE (d)-[:Location]->(s);
MATCH (d:rgResource {name: 'devices'}), (s:rgResource {name: 'rooms'}) MERGE (d)-[:Location]->(s);
MATCH (d:rgResource {name: 'devices'}), (s:rgResource {name: 'cages'}) MERGE (d)-[:Location]->(s);
MATCH (d:rgResource {name: 'devices'}), (s:rgResource {name: 'cabinets'}) MERGE (d)-[:Location]->(s);
MATCH (d:rgResource {name: 'devices'}), (s:rgResource {name: 'racks'}) MERGE (d)-[:Location]->(s);
MATCH (r:rgResource {name: 'devices'}), (i:rgResource {name: 'ethernetInterfaces'}) MERGE (r)-[:Interfaces {dependent: 'true'}]->(i);
MATCH (r:rgResource {name: 'devices'}), (o:rgResource {name: 'operatingSystems'}) MERGE (r)-[:OperatingSystem]->(o);
MATCH (r:rgResource {name: 'devices'}), (o:rgResource {name: 'operatingSystemVersions'}) MERGE (r)-[:OperatingSystem]->(o);
MATCH (d:rgResource {name: 'devices'}) MERGE (d)-[:Member]->(d);
MATCH (d:rgResource {name: 'devices'}) MERGE (d)-[:HostedOn]->(d);
MATCH (i:rgResource {name: 'ethernetInterfaces'}) MERGE (i)-[:SubInterface]->(i);
MATCH (i:rgResource {name: 'ethernetInterfaces'}), (s:rgResource {name: 'networkSegments'}) MERGE (i)-[:ConnectedTo]->(s);
MATCH (p:rgResource {name: 'parts'}), (o:rgResource {name: 'organisations'}) MERGE (p)-[:PurchasedFrom]->(o);
MATCH (p:rgResource {name: 'parts'}), (m:rgResource {name: 'models'}) MERGE (p)-[:CompatibleWith]->(m);
MATCH (p:rgResource {name: 'parts'}), (d:rgResource {name: 'devices'}) MERGE (p)-[:InstalledIn]->(d);
MATCH (p:rgResource {name: 'parts'}), (l:rgResource {name: 'sites'}) MERGE (p)-[:StorageLocation]->(l);
MATCH (p:rgResource {name: 'parts'}), (l:rgResource {name: 'buildings'}) MERGE (p)-[:StorageLocation]->(l);
MATCH (p:rgResource {name: 'parts'}), (l:rgResource {name: 'rooms'}) MERGE (p)-[:StorageLocation]->(l);
MATCH (p:rgResource {name: 'parts'}), (l:rgResource {name: 'cages'}) MERGE (p)-[:StorageLocation]->(l);
MATCH (p:rgResource {name: 'parts'}), (l:rgResource {name: 'cabinets'}) MERGE (p)-[:StorageLocation]->(l);
MATCH (s:rgResource {name: 'networkSegments'}) MERGE (s)-[:ConnectsTo]->(s);
MATCH (s:rgResource {name: 'networkSegments'}), (c:rgResource {name: 'networkCircuits'}) MERGE (s)-[:MemberOf]->(c);
// Circuit types
MATCH (s:rgResource {name: 'networkSegments'}), (t:rgResource {name: 'ethernet'}) MERGE (s)-[:Details]->(t);
MATCH (s:rgResource {name: 'networkSegments'}), (t:rgResource {name: 'fibreOpticCable'}) MERGE (s)-[:Details]->(t);
MATCH (s:rgResource {name: 'networkSegments'}), (t:rgResource {name: 'vLink'}) MERGE (s)-[:Details]->(t);
//
// L1/logical
MATCH (r:rgResource {name: 'devices'}), (a:rgResource {name: 'routingInstances'}) MERGE (r)-[:RoutingInstances {dependent: 'true'}]->(a);
MATCH (a:rgResource {name: 'routingInstances'}), (i:rgResource {name: 'ethernetInterfaces'}) MERGE (a)-[:Interfaces]->(i);
MATCH (r:rgResource {name: 'routingInstances'}), (v:rgResource {name: 'vrfGroups'}) MERGE (r)-[:Member]->(v);
MATCH (b:rgResource {name: 'brands'}), (o:rgResource {name: 'operatingSystems'}) MERGE (b)-[:Produces {dependent: 'true'}]->(o);
MATCH (o:rgResource {name: 'operatingSystems'}), (v:rgResource {name: 'operatingSystemVersions'}) MERGE (o)-[:Versions {dependent: 'true'}]->(v);
MATCH (b:rgResource {name: 'brands'}), (m:rgResource {name: 'models'}) MERGE (b)-[:Models {dependent: 'true'}]->(m);
MATCH (r:rgResource {name: 'devices'}), (m:rgResource {name: 'models'}) MERGE (r)-[:Model]->(m);
MATCH (r:rgResource {name: 'models'}), (m:rgResource {name: 'deviceRoles'}) MERGE (r)-[:Capability]->(m);
// Type is a reserved word in Cypher, so we use the longer-form DeviceType for the relationship
MATCH (r:rgResource {name: 'devices'}), (m:rgResource {name: 'deviceTypes'}) MERGE (r)-[:DeviceType]->(m);
MATCH (r:rgResource {name: 'devices'}), (m:rgResource {name: 'deviceRoles'}) MERGE (r)-[:Role]->(m);
MATCH (d:rgResource {name: 'devices'}), (p:rgResource {name: 'lifeCyclePhases'}) MERGE (d)-[:LifeCyclePhase {dependent: 'true'}]->(p);
MATCH (d:rgResource {name: 'devices'}), (v:rgResource {name: 'environments'}) MERGE (d)-[:Environment {dependent: 'true'}]->(v);
MATCH (d:rgResource {name: 'devices'}), (l:rgResource {name: 'serviceLevels'}) MERGE (d)-[:ServiceLevel {dependent: 'true'}]->(l);
MATCH (b:rgResource {name: 'brands'}), (p:rgResource {name: 'parts'}) MERGE (b)-[:PartNumber {dependent: 'true'}]->(p);
//
// L1->L2
MATCH (d:rgResource {name: 'devices'}), (v:rgResource {name: 'vlans'}) MERGE (d)-[:Vlan]->(v);
MATCH (i:rgResource {name: 'ethernetInterfaces'}), (v:rgResource {name: 'vlans'}) MERGE (i)-[:Vlan]->(v);
//
// L1->L3
MATCH (d:rgResource {name: 'devices'}), (a:rgResource {name: 'ipv4Addresses'}) MERGE (d)-[:Addresses {dependent: 'true'}]->(a);
MATCH (d:rgResource {name: 'devices'}), (a:rgResource {name: 'ipv6Addresses'}) MERGE (d)-[:Addresses {dependent: 'true'}]->(a);
MATCH (i:rgResource {name: 'ethernetInterfaces'}), (a:rgResource {name: 'ipv4Addresses'}) MERGE (i)-[:Addresses {dependent: 'true'}]->(a);
MATCH (i:rgResource {name: 'ethernetInterfaces'}), (a:rgResource {name: 'ipv6Addresses'}) MERGE (i)-[:Addresses {dependent: 'true'}]->(a);
MATCH (r:rgResource {name: 'devices'}), (b:rgResource {name: 'bgpInstances'}) MERGE (r)-[:BgpInstance]->(b);
//
// L1->L9
MATCH (b:rgResource {name: 'brands'}), (o:rgResource {name: 'organisations'}) MERGE (b)-[:Vendor]->(o);
MATCH (r:rgResource {name: 'devices'}), (p:rgResource {name: 'people'}) MERGE (r)-[:TechnicalOwner]->(p);
MATCH (r:rgResource {name: 'devices'}), (p:rgResource {name: 'people'}) MERGE (r)-[:BusinessOwner]->(p);
MATCH (r:rgResource {name: 'devices'}), (o:rgResource {name: 'organisations'}) MERGE (r)-[:BusinessOwner]->(o);
MATCH (c:rgResource {name: 'networkSegments'}), (o:rgResource {name: 'organisations'}) MERGE (c)-[:ProvisionedBy]-(o);
MATCH (c:rgResource {name: 'networkCircuits'}), (o:rgResource {name: 'organisations'}) MERGE (c)-[:ProvisionedBy]-(o);
MATCH (c:rgResource {name: 'networkCircuits'}), (o:rgResource {name: 'organisations'}) MERGE (c)-[:BilledBy]-(o);
//
// L2
MATCH (r:rgResource {name: 'devices'}), (i:rgResource {name: 'ethernetInterfaces'}) MERGE (r)-[:Interfaces]->(i);
MATCH (v:rgResource {name: 'vlans'}), (g:rgResource {name: 'vlanGroups'}) MERGE (v)-[:Member]->(g);
//
// L2->L3
//
// L3
MATCH (a:rgResource {name: 'asn'}), (v:rgResource {name: 'vrfGroups'}) MERGE (a)-[:VrfGroups]->(v);
MATCH (v:rgResource {name: 'vrfGroups'}), (s:rgResource {name: 'ipv4Subnets'}) MERGE (v)-[:Subnets {dependent: 'true'}]->(s);
MATCH (v:rgResource {name: 'vrfGroups'}), (s:rgResource {name: 'ipv6Subnets'}) MERGE (v)-[:Subnets {dependent: 'true'}]->(s);
MATCH (s:rgResource {name: 'ipv4Subnets'}), (a:rgResource {name: 'ipv4Addresses'}) MERGE (s)-[:Addresses {dependent: 'true'}]->(a);
MATCH (s:rgResource {name: 'ipv6Subnets'}), (a:rgResource {name: 'ipv6Addresses'}) MERGE (s)-[:Addresses {dependent: 'true'}]->(a);
MATCH (a:rgResource {name: 'asn'}), (s:rgResource {name: 'ipv4Subnets'}) MERGE (a)-[:Subnets {dependent: 'true'}]->(s);
MATCH (a:rgResource {name: 'asn'}), (s:rgResource {name: 'ipv6Subnets'}) MERGE (a)-[:Subnets {dependent: 'true'}]->(s);
MATCH (s:rgResource {name: 'ipv4Subnets'}) MERGE (s)-[:Subnets]->(s);
MATCH (s:rgResource {name: 'ipv6Subnets'}) MERGE (s)-[:Subnets]->(s);
// DNS
MATCH (d:rgResource {name: 'applications'}), (z:rgResource {name: 'dnsZones'}) MERGE (d)-[:Serves]->(z);
MATCH (z:rgResource {name: 'dnsZones'}) MERGE (z)-[:Delegates]->(z);
MATCH (z:rgResource {name: 'dnsZones'}), (o:rgResource {name: 'organisations'}) MERGE (z)-[:Owner]->(o);
MATCH (z:rgResource {name: 'dnsZones'}), (a:rgResource {name: 'aRecord'}) MERGE (z)-[:Records {dependent: 'true'}]->(a);
MATCH (z:rgResource {name: 'dnsZones'}), (a:rgResource {name: 'mxRecord'}) MERGE (z)-[:Records {dependent: 'true'}]->(a);
MATCH (z:rgResource {name: 'dnsZones'}), (a:rgResource {name: 'aaaaRecord'}) MERGE (z)-[:Records {dependent: 'true'}]->(a);
MATCH (z:rgResource {name: 'dnsZones'}), (a:rgResource {name: 'aliasRecord'}) MERGE (z)-[:Records {dependent: 'true'}]->(a);
MATCH (a:rgResource {name: 'aRecord'}), (d:rgResource {name: 'ipv4Addresses'}) MERGE (a)-[:ResolvesTo {dependent: 'true'}]->(d);
MATCH (a:rgResource {name: 'mxRecord'}), (d:rgResource {name: 'ipv4Addresses'}) MERGE (a)-[:ResolvesTo {dependent: 'true'}]->(d);
MATCH (a:rgResource {name: 'mxRecord'}), (d:rgResource {name: 'ipv6Addresses'}) MERGE (a)-[:ResolvesTo {dependent: 'true'}]->(d);
MATCH (a:rgResource {name: 'aaaaRecord'}), (d:rgResource {name: 'ipv6Addresses'}) MERGE (a)-[:ResolvesTo {dependent: 'true'}]->(d);
MATCH (d:rgResource {name: 'ipv4Addresses'}), (a:rgResource {name: 'aRecord'}) MERGE (d)-[:PTR]->(a);
MATCH (z:rgResource {name: 'aliasRecord'}), (a:rgResource {name: 'aRecord'}) MERGE (z)-[:CNAME]->(a);
MATCH (z:rgResource {name: 'aliasRecord'}), (a:rgResource {name: 'aaaaRecord'}) MERGE (z)-[:CNAME]->(a);
//
// L3/logical
MATCH (b:rgResource {name: 'bgpInstances'}), (i:rgResource {name: 'routingInstances'}) MERGE (b)-[:Member]->(i);
MATCH (b:rgResource {name: 'bgpInstances'}), (p:rgResource {name: 'bgpPeerings'}) MERGE (b)-[:bgpPeering]->(p);
MATCH (b:rgResource {name: 'devices'}), (p:rgResource {name: 'bgpPeerings'}) MERGE (b)-[:bgpPeering]->(p);
MATCH (b:rgResource {name: 'bgpPeerings'}), (a:rgResource {name: 'ipv4Addresses'}) MERGE (b)-[:RemoteAddress]->(a);
MATCH (b:rgResource {name: 'bgpPeerings'}), (a:rgResource {name: 'ipv6Addresses'}) MERGE (b)-[:RemoteAddress]->(a);
MATCH (b:rgResource {name: 'bgpPeerings'}), (a:rgResource {name: 'ipv4Addresses'}) MERGE (b)-[:LocalAddress]->(a);
MATCH (b:rgResource {name: 'bgpPeerings'}), (a:rgResource {name: 'ipv6Addresses'}) MERGE (b)-[:LocalAddress]->(a);
MATCH (b:rgResource {name: 'bgpPeerings'}), (a:rgResource {name: 'asn'}) MERGE (b)-[:RemoteAs]->(a);
MATCH (b:rgResource {name: 'bgpInstances'}), (a:rgResource {name: 'asn'}) MERGE (b)-[:ASN]->(a);
// L3->L9
MATCH (a:rgResource {name: 'asn'}), (o:rgResource {name: 'organisations'}) MERGE (a)-[:Owner]->(o);
MATCH (s:rgResource {name: 'ipv4Subnets'}), (o:rgResource {name: 'organisations'}) MERGE (s)-[:Owner]->(o);
MATCH (s:rgResource {name: 'ipv6Subnets'}), (o:rgResource {name: 'organisations'}) MERGE (s)-[:Owner]->(o);
//
// L8
MATCH (a:rgResource {name: 'applications'}) MERGE (a)-[:DependsOn]->(a);
//
// L8->L1
MATCH (a:rgResource {name: 'applications'}), (h:rgResource {name: 'devices'}) MERGE (a)-[:RunsOn]->(h);
//
// L8->L9
MATCH (a:rgResource {name: 'applications'}), (p:rgResource {name: 'people'}) MERGE (a)-[:BusinessOwner]->(p);
MATCH (a:rgResource {name: 'applications'}), (p:rgResource {name: 'people'}) MERGE (a)-[:TechnicalOwner]->(p);
//
// L9
MATCH (o:rgResource {name: 'organisations'}), (m:rgResource {name: 'emailAddresses'}) MERGE (o)-[:ContactMethods {dependent: 'true'}]->(m);
MATCH (p:rgResource {name: 'people'}), (m:rgResource {name: 'emailAddresses'}) MERGE (p)-[:ContactMethods {dependent: 'true'}]->(m);
MATCH (p:rgResource {name: 'people'}), (n:rgResource {name: 'phoneNumbers'}) MERGE (p)-[:ContactMethods {dependent: 'true'}]->(n);
MATCH (p:rgResource {name: 'people'}), (o:rgResource {name: 'organisations'}) MERGE (p)-[:Member]->(o);
MATCH (p:rgResource {name: 'people'}), (o:rgResource {name: 'organisations'}) MERGE (p)-[:AccountManager]->(o);
MATCH (p:rgResource {name: 'people'}), (o:rgResource {name: 'organisations'}) MERGE (p)-[:SalesContact]->(o);
MATCH (p:rgResource {name: 'people'}), (o:rgResource {name: 'organisations'}) MERGE (p)-[:SupportContact]->(o);
MATCH (o:rgResource {name: 'organisations'}) MERGE (o)-[:Member]->(o);
MATCH (c:rgResource {name: 'contracts'}) MERGE (c)-[:Subcontracts]->(c);
MATCH (c:rgResource {name: 'contracts'}), (o:rgResource {name: 'organisations'}) MERGE (c)-[:Vendor]->(o);
MATCH (c:rgResource {name: 'contracts'}), (o:rgResource {name: 'organisations'}) MERGE (c)-[:Customer]->(o);
MATCH (c:rgResource {name: 'contracts'}), (p:rgResource {name: 'people'}) MERGE (c)-[:NegotiatedBy]->(p);
MATCH (c:rgResource {name: 'contracts'}), (d:rgResource {name: 'devices'}) MERGE (c)-[:Supplies]->(d);
MATCH (c:rgResource {name: 'contracts'}), (d:rgResource {name: 'devices'}) MERGE (c)-[:Supports]->(d);
MATCH (c:rgResource {name: 'contracts'}), (p:rgResource {name: 'parts'}) MERGE (c)-[:Supplies]->(p);
MATCH (c:rgResource {name: 'contracts'}), (p:rgResource {name: 'parts'}) MERGE (c)-[:Supports]->(p);
MATCH (c:rgResource {name: 'contracts'}), (b:rgResource {name: 'buildings'}) MERGE (c)-[:Supplies]->(b);
MATCH (c:rgResource {name: 'contracts'}), (b:rgResource {name: 'buildings'}) MERGE (c)-[:Supports]->(b);
MATCH (c:rgResource {name: 'contracts'}), (f:rgResource {name: 'floors'}) MERGE (c)-[:Supplies]->(b);
MATCH (c:rgResource {name: 'contracts'}), (f:rgResource {name: 'floors'}) MERGE (c)-[:Supports]->(b);
MATCH (c:rgResource {name: 'contracts'}), (r:rgResource {name: 'rooms'}) MERGE (c)-[:Supplies]->(r);
MATCH (c:rgResource {name: 'contracts'}), (r:rgResource {name: 'rooms'}) MERGE (c)-[:Supports]->(r);
MATCH (c:rgResource {name: 'contracts'}), (r:rgResource {name: 'cages'}) MERGE (c)-[:Supplies]->(r);
MATCH (c:rgResource {name: 'contracts'}), (r:rgResource {name: 'cages'}) MERGE (c)-[:Supports]->(r);
MATCH (c:rgResource {name: 'contracts'}), (r:rgResource {name: 'cabinets'}) MERGE (c)-[:Supplies]->(r);
MATCH (c:rgResource {name: 'contracts'}), (r:rgResource {name: 'cabinets'}) MERGE (c)-[:Supports]->(r);
// L9->L1
MATCH (b:rgResource {name: 'sites'}), (o:rgResource {name: 'organisations'}) MERGE (b)-[:Organisation]->(o);
MATCH (b:rgResource {name: 'buildings'}), (o:rgResource {name: 'organisations'}) MERGE (b)-[:Organisation]->(o);
MATCH (b:rgResource {name: 'floors'}), (o:rgResource {name: 'organisations'}) MERGE (b)-[:Organisation]->(o);
MATCH (b:rgResource {name: 'rooms'}), (o:rgResource {name: 'organisations'}) MERGE (b)-[:Organisation]->(o);
MATCH (b:rgResource {name: 'cages'}), (o:rgResource {name: 'organisations'}) MERGE (b)-[:Organisation]->(o);
MATCH (b:rgResource {name: 'cabinets'}), (o:rgResource {name: 'organisations'}) MERGE (b)-[:Organisation]->(o);
