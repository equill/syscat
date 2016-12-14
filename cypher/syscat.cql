// Syscat schema for Restagraph
//
// Currently L3-only, because we can't yet model interfaces properly

// Clear the way
MATCH (n:rgResource)-[r]->(m:rgResource) DELETE r;
MATCH (n:rgResource)-[r]->(m:rgAttribute) DELETE r;
MATCH (n:rgAttribute) DELETE n;
MATCH (n:rgResource) DELETE n;

// Resources
//
// L1
CREATE (r:rgResource {name: 'devices', notes: 'Any kind of device that interacts with a network, be it L1, L2 or L3'});
CREATE (r:rgResource {name: 'deviceRoles', notes: 'Classification of the device, e.g. host, router or switch.'});
CREATE (r:rgResource {name: 'operatingSystems'});
CREATE (r:rgResource {name: 'buildings'});
CREATE (r:rgResource {name: 'networkSegments'}), (r)-[:rgHasAttribute]->(:rgAttribute {name: 'ourReference'}), (r)-[:rgHasAttribute]->(:rgAttribute {name: 'theirReference'});
CREATE (r:rgResource {name: 'networkCircuit'}), (r)-[:rgHasAttribute]->(:rgAttribute {name: 'ourReference'}), (r)-[:rgHasAttribute]->(:rgAttribute {name: 'theirReference'});
// Network segment types
CREATE (r:rgResource {name: 'ethernet'});
CREATE (r:rgResource {name: 'fibreOpticCable'});
CREATE (r:rgResource {name: 'vLink'});
// L1/logical
CREATE (:rgResource {name: 'vrfGroups'});
CREATE (:rgResource {name: 'routingInstances'}); // FIXME should be dependent on a router
CREATE (:rgResource {name: 'models'});
CREATE (:rgResource {name: 'vendors'});
// L2
CREATE (:rgResource {name: 'vlans', notes: 'Layer 2 groupings, i.e. ethernet. Not to be confused with subnets.'});
CREATE (g:rgResource {name: 'vlanGroups', notes: 'UID: name. VLAN ID goes in the VLAN_ID field - this deals with the problem where the same VLAN ID is used in different ways in different, disconnected, parts of the network.'}), (g)-[:rgHasAttribute]->(:rgAttribute {name: 'vlanid'});
//
// L3
CREATE (:rgResource {name: 'ipv4Addresses'});
CREATE (:rgResource {name: 'ipv4Subnets'});
CREATE (:rgResource {name: 'ipv6Addresses'});
CREATE (:rgResource {name: 'ipv6Subnets'});
// L3/logical
CREATE (b:rgResource {name: 'bgpInstances'});
CREATE (b:rgResource {name: 'bgpPeerings'});
CREATE (r:rgResource {name: 'as', notes: 'Autonomous Systems. The UID should be the ASN, and the "name" should be its human-friendly tag.'}), (r)-[:rgHasAttribute]->(:rgAttribute {name: 'name', required: 'false'});
//
// Telco circuits
//
// Layer 8 - applications
CREATE (:rgResource {name: 'applications'});
//
// Layer 9 - people
CREATE (p:rgResource {name: 'people', notes: 'UID = login or other short reference'}), (p)-[:rgHasAttribute]->(:rgAttribute {name: 'givenName'}), (p)-[:rgHasAttribute]->(:rgAttribute {name: 'surname'});
CREATE (p:rgResource {name: 'organisations', notes: 'Any kind of organisation'});

// Waiting on https://github.com/equill/restagraph/issues/17
CREATE (i:rgResource {name: 'interfaces'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'mac-address'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'maxSpeed'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'description'});


// Relationships
//
// L1
MATCH (r:rgResource {name: 'devices'}), (a:rgResource {name: 'as'}) MERGE (r)-[:MemberOf]->(a);
MATCH (r:rgResource {name: 'devices'}), (o:rgResource {name: 'operatingSystems'}) MERGE (r)-[:OperatingSystem]->(o);
MATCH (i:rgResource {name: 'interfaces'}) MERGE (i)-[:SubInterface]->(i);
MATCH (i:rgResource {name: 'interfaces'}), (s:rgResource {name: 'networkSegments'}) MERGE (i)-[:ConnectedTo]->(s);
MATCH (s:rgResource {name: 'networkSegments'}) MERGE (s)-[:ConnectsTo]->(s);
MATCH (s:rgResource {name: 'networkSegments'}), (c:rgResource {name: 'networkCircuit'}) MERGE (s)-[:ConnectsTo]->(c);
// Circuit types
MATCH (s:rgResource {name: 'networkSegments'}), (t:rgResource {name: 'ethernet'}) MERGE (s)-[:Details]->(t);
MATCH (s:rgResource {name: 'networkSegments'}), (t:rgResource {name: 'fibreOpticCable'}) MERGE (s)-[:Details]->(t);
MATCH (s:rgResource {name: 'networkSegments'}), (t:rgResource {name: 'vLink'}) MERGE (s)-[:Details]->(t);
//
// L1/logical
MATCH (r:rgResource {name: 'devices'}), (b:rgResource {name: 'buildings'}) MERGE (r)-[:Location]->(b);
MATCH (r:rgResource {name: 'devices'}), (a:rgResource {name: 'routingInstances'}) MERGE (r)-[:RoutingInstance]->(a);
MATCH (i:rgResource {name: 'interfaces'}), (a:rgResource {name: 'routingInstances'}) MERGE (i)-[:RoutingInstance]->(a);
MATCH (r:rgResource {name: 'routingInstances'}), (v:rgResource {name: 'vrfGroups'}) MERGE (r)-[:Member]->(v);
MATCH (o:rgResource {name: 'operatingSystems'}), (v:rgResource {name: 'vendors'}) MERGE (o)-[:Vendor]->(v);
MATCH (r:rgResource {name: 'devices'}), (m:rgResource {name: 'models'}) MERGE (r)-[:Model]->(m);
MATCH (m:rgResource {name: 'models'}), (v:rgResource {name: 'vendors'}) MERGE (m)-[:Vendor]->(v);
MATCH (r:rgResource {name: 'models'}), (m:rgResource {name: 'deviceRoles'}) MERGE (r)-[:Type]->(m);
MATCH (r:rgResource {name: 'devices'}), (m:rgResource {name: 'deviceRoles'}) MERGE (r)-[:Type]->(m);
//
// L1->L2
MATCH (d:rgResource {name: 'devices'}), (v:rgResource {name: 'vlans'}) MERGE (d)-[:Vlan]->(v);
MATCH (i:rgResource {name: 'interfaces'}), (v:rgResource {name: 'vlans'}) MERGE (i)-[:Vlan]->(v);
//
// L1->L3
MATCH (d:rgResource {name: 'devices'}), (a:rgResource {name: 'ipv4Addresses'}) MERGE (d)-[:Ipv4Addresses]->(a);
MATCH (d:rgResource {name: 'devices'}), (a:rgResource {name: 'ipv6Addresses'}) MERGE (d)-[:Ipv4Addresses]->(a);
MATCH (i:rgResource {name: 'interfaces'}), (a:rgResource {name: 'ipv4Addresses'}) MERGE (i)-[:Ipv4Addresses]->(a);
MATCH (i:rgResource {name: 'interfaces'}), (a:rgResource {name: 'ipv6Addresses'}) MERGE (i)-[:Ipv4Addresses]->(a);
MATCH (r:rgResource {name: 'devices'}), (b:rgResource {name: 'bgpInstances'}) MERGE (r)-[:BgpInstance]->(b);
MATCH (v:rgResource {name: 'vrfGroups'}), (a:rgResource {name: 'as'}) MERGE (v)-[:AS]->(a);
//
// L1->L9
MATCH (r:rgResource {name: 'devices'}), (p:rgResource {name: 'people'}) MERGE (r)-[:TechnicalOwner]->(p);
MATCH (c:rgResource {name: 'networkSegment'}), (o:rgResource {name: 'organisations'}) MERGE (c)-[:ProvisionedBy]-(o);
MATCH (c:rgResource {name: 'networkCircuit'}), (o:rgResource {name: 'organisations'}) MERGE (c)-[:ProvisionedBy]-(o);
MATCH (c:rgResource {name: 'networkCircuit'}), (o:rgResource {name: 'organisations'}) MERGE (c)-[:BilledBy]-(o);
//
// L2: Waiting on https://github.com/equill/restagraph/issues/17
MATCH (r:rgResource {name: 'devices'}), (i:rgResource {name: 'interfaces'}) MERGE (r)-[:Interfaces]->(i);
MATCH (i:rgResource {name: 'interfaces'}), (a:rgResource {name: 'ipv4Addresses'}) MERGE (i)-[:Ipv4Addresses]->(a);
MATCH (v:rgResource {name: 'vlans'}), (g:rgResource {name: 'vlanGroups'}) MERGE (v)-[:Member]->(g);
//
// L2->L3
MATCH (v:rgResource {name: 'vlanGroups'}), (a:rgResource {name: 'as'}) MERGE (v)-[:AS]->(a);
//
// L3
MATCH (a:rgResource {name: 'ipv4Addresses'}), (s:rgResource {name: 'ipv4Subnets'}) MERGE (a)-[:Subnet]->(s);
MATCH (a:rgResource {name: 'ipv6Addresses'}), (s:rgResource {name: 'ipv6Subnets'}) MERGE (a)-[:Subnet]->(s);
MATCH (s:rgResource {name: 'ipv4Subnets'}), (a:rgResource {name: 'ipv4Addresses'}) MERGE (s)-[:Gateways]->(a);
MATCH (s:rgResource {name: 'ipv6Subnets'}), (a:rgResource {name: 'ipv6Addresses'}) MERGE (s)-[:Gateways]->(a);
MATCH (s:rgResource {name: 'ipv4Subnets'}), (a:rgResource {name: 'as'}) MERGE (s)-[:AS]->(a);
MATCH (s:rgResource {name: 'ipv6Subnets'}), (a:rgResource {name: 'as'}) MERGE (s)-[:AS]->(a);
MATCH (s:rgResource {name: 'ipv4Subnets'}) MERGE (s)-[:Subnet]->(s);
MATCH (s:rgResource {name: 'ipv6Subnets'}) MERGE (s)-[:Subnet]->(s);
//
// L3/logical
MATCH (b:rgResource {name: 'bgpInstances'}), (i:rgResource {name: 'routingInstances'}) MERGE (b)-[:Member]->(i);
MATCH (b:rgResource {name: 'bgpInstances'}), (p:rgResource {name: 'bgpPeerings'}) MERGE (b)-[:bgpPeering]->(p);
MATCH (b:rgResource {name: 'bgpPeerings'}), (a:rgResource {name: 'ipv4Addresses'}) MERGE (b)-[:remoteAddress]->(a);
MATCH (b:rgResource {name: 'bgpPeerings'}), (a:rgResource {name: 'ipv6Addresses'}) MERGE (b)-[:remoteAddress]->(a);
MATCH (b:rgResource {name: 'bgpPeerings'}), (a:rgResource {name: 'ipv4Addresses'}) MERGE (b)-[:localAddress]->(a);
MATCH (b:rgResource {name: 'bgpPeerings'}), (a:rgResource {name: 'ipv6Addresses'}) MERGE (b)-[:localAddress]->(a);
MATCH (b:rgResource {name: 'bgpPeerings'}), (a:rgResource {name: 'as'}) MERGE (b)-[:remoteAs]->(a);
MATCH (b:rgResource {name: 'bgpInstances'}), (a:rgResource {name: 'as'}) MERGE (b)-[:AS]->(a);
//
// L3->L1
MATCH (a:rgResource {name: 'ipv4Addresses'}), (s:rgResource {name: 'routingInstances'}) MERGE (a)-[:Vrf]->(s);
MATCH (a:rgResource {name: 'ipv6Addresses'}), (s:rgResource {name: 'routingInstances'}) MERGE (a)-[:Vrf]->(s);
//
// L8
MATCH (a:rgResource {name: 'applications'}) MERGE (a)-[:DependsOn]->(a);
//
// L8->L1
MATCH (a:rgResource {name: 'applications'}), (h:rgResource {name: 'devices'}) MERGE (a)-[:RunsOn]->(h);
//
// L8->L3
MATCH (a:rgResource {name: 'applications'}), (d:rgResource {name: 'ipv4Addresses'}) MERGE (a)-[:ConnectsTo]->(d);
MATCH (a:rgResource {name: 'applications'}), (d:rgResource {name: 'ipv6Addresses'}) MERGE (a)-[:ConnectsTo]->(d);
//
// L8->L9
MATCH (a:rgResource {name: 'applications'}), (p:rgResource {name: 'people'}) MERGE (a)-[:BusinessOwner]->(p);
MATCH (a:rgResource {name: 'applications'}), (p:rgResource {name: 'people'}) MERGE (a)-[:TechnicalOwner]->(p);
//
// L9
MATCH (p:rgResource {name: 'people'}), (o:rgResource {name: 'organisations'}) MERGE (p)-[:Member]->(o);
MATCH (o:rgResource {name: 'organisations'}) MERGE (o)-[:Member]->(o);
