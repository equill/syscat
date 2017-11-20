# Syscat - the System Catalogue

"From spare parts to org charts"

An API-based network catalogue/configuration database that actually covers everything, from hardware assets up to application interdependencies, through to the people and organisations that have any kind of stake in them.

The REST API is its primary interface; anything else will be built on that. This is to make it automation-friendly, as well as to make it possible for dissatisfied/impatient users to build the interface that suits them.

Its schema is user-extendable, so you can add whatever network elements it doesn't already cover.


## Usage

### Startup

If it was supplied as a standalone executable, it checks for the following environment variables, with default values as shown:

- `SYSCAT_LISTEN_ADDR` = address(es) on which the application server should listen for incoming HTTP requests
    - default: `localhost`
- `SYSCAT_LISTEN_PORT` = port on which the application server should listen for incoming HTTP requests
    - default: `4950`
- `SYSCAT_NEO4J_HOSTNAME` = hostname or IP address on which the Neo4j server is listening
    - default: `localhost`
- `SYSCAT_NEO4J_USER` = username for authenticating to the Neo4j server
    - default: `neo4j`
- `SYSCAT_NEO4J_PASSWORD` = password for authenticating that user to the Neo4j server
    - default: it really doesn't matter. Set it to something hard to crack, and keep it secret.


## Basic design

### Background thinking

- model the network as it is, however messed-up that might be.
- also model how it's intended to be, and provide means to compare the two.
- enable users to record whatever information they _do_ have on hand, and evolve the picture as new information comes to hand
    - e.g, you can assign an IP address to a host, then move it to the correct interface, then assign that interface to a routing instance, without losing any information.
- enforce as little policy or dogma as possible
    - you can use Syscat in a descriptive way, a prescriptive way, or any combination of the two
    - _I_ might judge how you laid out your network, but Syscat doesn't.
- cover multiple ASes, and multiple VRF-groups within each AS, so you can model interactions between several networks.
- automation is increasingly valuable in network management. An API-first design means that anything you can do with the GUI (when one is eventually added) can be done by a script.
- no one GUI design suits everybody. An API-first design means you can build your own on top, if the included one doesn't suit, or if you're fed up waiting for it to come into being.
- some things only make sense in the context of other things, e.g. interfaces only exist in the context of a device. The schema reflects this.

### Architecture

A web application server fronting a Neo4j graph database.

The schema is defined within the database itself, so you can reliably find the current version.

There's a raw API, which provides the core functionality, plus domain-specific APIs for things such as IPAM which can't be fully served that way.


### Raw API

This provides most of the functionality, is rooted at `/raw/v<version/` and uses [Restagraph](https://github.com/equill/restagraph) as its engine.

It dynamically generates the API according to what's in the database, so keeps up automatically with any updates. This is what enables you to extend the schema according to your needs.

It looks slightly cumbersome, using a `/<type>/<uid>/<relationship>/<type>/<uid>...` format, but this has the virtues of consistency and predictability, which makes automation easier.


### IPAM (IP Address Management) API

Some things do need their own API; among them are creating and removing IP subnets and addresses.

While nothing actually _stops_ you using the Raw API to add, change and remove subnets and addresses, it's a tedious and error-prone process, especially if you add and remove subnets by hand - finding the parent subnet, moving the new child subnets and addresses under a new mid-level subnet, and doing all that in reverse when you remove them.

It's aware of ASes and VRFs, in keeping with the rest of the system.

New subnets are automatically created under the most appropriate supernet, and subnets and IP addresses are automatically rehomed as subnets are created, deleted and resized.

GET requests are the search interface to the IPAM section:

- `/subnets`
- `/addresses`

Both return the URI for interacting with the subnet or address in question via the Raw API, which is what you need when you then link them to/from other resources, such as delegating a subnet to another suborganisation, or allocating an address to a device.

Search for subnet 192.168.0.0/16 under ASN 64496:
```
GET http://localhost:4950/ipam/v1/subnets?asns=64496?subnet=192.168.0.0/16

/asns/64496/Subnets/ipv4Subnets/192.168.0.0
```
