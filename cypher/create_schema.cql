// Syscat schema for the Restagraph engine

// Resources
//
// L1
CREATE (d:rgResource {name: 'devices', notes: 'Any kind of device that interacts with a network, be it L1, L2 or L3'}),
    (d)-[:rgHasAttribute]->(:rgAttribute {name: 'description'}),
    (d)-[:rgHasAttribute]->(:rgAttribute {name: 'serial_number'}),
    (d)-[:rgHasAttribute]->(:rgAttribute {name: 'asset_number'}),
    (d)-[:rgHasAttribute]->(:rgAttribute {name: 'in_service'});
CREATE (r:rgResource {name: 'deviceTypes', notes: 'Type of device, e.g. physical, cluster or virtual.'});
CREATE (r:rgResource {name: 'deviceRoles', notes: 'Classification of the device, e.g. host, router or switch.'});
CREATE (i:rgResource {name: 'ethernetInterfaces', dependent: 'true'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'mac_address'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'snmp_index'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'speed'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'alias'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'description'});
CREATE (r:rgResource {name: 'operatingSystems'});
CREATE (r:rgResource {name: 'sites'});
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
// Layer 9 - people
CREATE (p:rgResource {name: 'people', notes: 'UID = login or other short reference'}), (p)-[:rgHasAttribute]->(:rgAttribute {name: 'givenName'}), (p)-[:rgHasAttribute]->(:rgAttribute {name: 'surname'});
CREATE (o:rgResource {name: 'organisations', notes: 'Any kind of organisation'}),
    (o)-[:rgHasAttribute]->(:rgAttribute {name: 'description'});


// Relationships
//
// L1
MATCH (r:rgResource {name: 'devices'}), (i:rgResource {name: 'ethernetInterfaces'}) MERGE (r)-[:Interfaces {dependent: 'true'}]->(i);
MATCH (r:rgResource {name: 'devices'}), (o:rgResource {name: 'operatingSystems'}) MERGE (r)-[:OperatingSystem]->(o);
MATCH (i:rgResource {name: 'ethernetInterfaces'}) MERGE (i)-[:SubInterface]->(i);
MATCH (i:rgResource {name: 'ethernetInterfaces'}), (s:rgResource {name: 'networkSegments'}) MERGE (i)-[:ConnectedTo]->(s);
MATCH (s:rgResource {name: 'networkSegments'}) MERGE (s)-[:ConnectsTo]->(s);
MATCH (s:rgResource {name: 'networkSegments'}), (c:rgResource {name: 'networkCircuits'}) MERGE (s)-[:MemberOf]->(c);
// Circuit types
MATCH (s:rgResource {name: 'networkSegments'}), (t:rgResource {name: 'ethernet'}) MERGE (s)-[:Details]->(t);
MATCH (s:rgResource {name: 'networkSegments'}), (t:rgResource {name: 'fibreOpticCable'}) MERGE (s)-[:Details]->(t);
MATCH (s:rgResource {name: 'networkSegments'}), (t:rgResource {name: 'vLink'}) MERGE (s)-[:Details]->(t);
//
// L1/logical
MATCH (r:rgResource {name: 'devices'}), (b:rgResource {name: 'sites'}) MERGE (r)-[:Site]->(b);
MATCH (r:rgResource {name: 'devices'}), (a:rgResource {name: 'routingInstances'}) MERGE (r)-[:RoutingInstances {dependent: 'true'}]->(a);
MATCH (a:rgResource {name: 'routingInstances'}), (i:rgResource {name: 'ethernetInterfaces'}) MERGE (a)-[:Interfaces]->(i);
MATCH (r:rgResource {name: 'routingInstances'}), (v:rgResource {name: 'vrfGroups'}) MERGE (r)-[:Member]->(v);
MATCH (b:rgResource {name: 'brands'}), (o:rgResource {name: 'operatingSystems'}) MERGE (b)-[:Produces]->(o);
MATCH (b:rgResource {name: 'brands'}), (m:rgResource {name: 'models'}) MERGE (b)-[:Models {dependent: 'true'}]->(m);
MATCH (r:rgResource {name: 'devices'}), (m:rgResource {name: 'models'}) MERGE (r)-[:Model]->(m);
MATCH (r:rgResource {name: 'models'}), (m:rgResource {name: 'deviceRoles'}) MERGE (r)-[:Capability]->(m);
// Type is a reserved word in Cypher, so we use the longer-form DeviceType
MATCH (r:rgResource {name: 'devices'}), (m:rgResource {name: 'deviceTypes'}) MERGE (r)-[:DeviceType]->(m);
MATCH (r:rgResource {name: 'devices'}), (m:rgResource {name: 'deviceRoles'}) MERGE (r)-[:Role]->(m);
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
MATCH (p:rgResource {name: 'people'}), (o:rgResource {name: 'organisations'}) MERGE (p)-[:Member]->(o);
MATCH (o:rgResource {name: 'organisations'}) MERGE (o)-[:Member]->(o);
// L9->L1
MATCH (b:rgResource {name: 'sites'}), (o:rgResource {name: 'organisations'}) MERGE (b)-[:Organisation]->(o);
