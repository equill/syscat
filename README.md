# Syscat - the System Catalogue

"From spare parts to org charts"

An API-based network catalogue/configuration database that actually covers everything, from hardware assets up to application interdependencies, through to the people and organisations that have any kind of stake in them.

The REST API is its primary interface; anything else, such as a web GUI, can be built on that. This is to make it automation-friendly, as well as to make it viable for users to build the interface that suits them.


# Installation

- very quick-and-dirty installation on a Linux system.
- assumes you want to store the Neo4j data under `/opt/syscat`.
- maps the listening ports for Syscat and Neo4j to non-default values, to avoid clashing with local (development) installations.


Download [this docker-compose.yml file](https://github.com/equill/syscat_scripts/blob/master/docker/docker-compose.yml), and modify it as necessary.


Now execute the following:
```
docker pull neo4j:3.4.1
docker pull equill/syscat:0.3.1
mkdir -p /opt/syscat/{logs,data}
chown 100 /opt/syscat/{logs,data}
docker stack deploy -c docker-compose.yml syscat
```

After a moment, you should be able to connect to Syscat via HTTP on port 4951, and to Neo4j via HTTP on 7676, or Bolt on 7688. These are set in `docker-compose.yml`, to avoid conflicting with the standalone image.

Note that on some systems, connections to localhost hang forever. It does listen on all addresses, so if localhost hangs you can just connect on any other address, e.g. the host computer's LAN address.

## Startup

If it was supplied as a standalone executable, it checks for the following environment variables, with default values as shown:

- `SYSCAT_LISTEN_ADDR` = address(es) on which the application server should listen for incoming HTTP requests
    - default: `localhost`
- `SYSCAT_LISTEN_PORT` = port on which the application server should listen for incoming HTTP requests
    - default: `4950`
- `SYSCAT_NEO4J_HOSTNAME` = hostname or IP address on which the Neo4j server is listening
    - default: `localhost`
- `SYSCAT-NEO4J_PORT` = port on which the Neo4j service is listening for HTTP requests
    - default: 7474
- `SYSCAT_NEO4J_USER` = username for authenticating to the Neo4j server
    - default: `neo4j`
- `SYSCAT_NEO4J_PASSWORD` = password for authenticating that user to the Neo4j server
    - default: it really doesn't matter. Set it to something hard to crack, and keep it secret.

These are also set in the `docker-compose.yml` file linked above.


# Basic design

## Background thinking

- model the network as it is, however messed-up that might be.
- also enable the user to model how it's intended to be, and provide means to compare the two. It's up to you whether you combine these views in a single instance, or record the intended model in one instance and the discovered details in another.
- enable users to record whatever information they _do_ have on hand, and evolve the picture as new information comes to hand
    - e.g, you can assign an IP address to a host, then move it to the correct interface, then assign that interface to a routing instance, without losing any information, including any incoming links from other things.
- enforce as little policy or dogma as possible
    - _I_ might judge how you laid out your network, but Syscat doesn't.
- most networks interact with other networks in some way; enable modelling that with multiple organisations, multiple ASes and multiple VRF-groups within each organisation.
- API-first implementation, because
    - automation is increasingly crucial in network management. An API-first design means that anything you can do with the GUI (when one is eventually added) can be done by a script.
    - no one GUI design suits everybody. An API-first design means you can build your own on top, if the included one doesn't suit, or if you're fed up waiting for me to build it.
- some things only make sense in the context of other things, e.g. interfaces only exist in the context of a device. The schema reflects this.

## Architecture

A web application server fronting a Neo4j graph database.

The schema is defined within the database itself, so you can query it to confirm the API that is actually in use via the URI `/schema/v1`.

There's a raw API, which provides the core functionality, plus domain-specific APIs for things such as IPAM, which can't be properly served via the raw API.


# Schema API

The output is raw JSON, so you'll want to run the output through something like `jq`.

To dump the whole schema: `/schema/v1`. Note that the `v1` part refers to the version of the API, *not* of the schema being reported on. There's no version-control of schemas, just a history of what versions have been installed, which is used by the engine to decide whether an update is needed.

To get the details of one resourcetype: `/schema/v1?name=resourcetype`.

To add a resourcetype: POST

To remove a resourcetype: DELETE

*CAUTION*: when you delete a resourcetype from the schema, all instances of that resourcetype are removed along with it. You can only recover from this by restoring the database from backup, so use this command with care.

To add a relationship between resourcetypes: POST

To delete a relationship between resourcetypes: DELETE


## Raw API

This provides most of the functionality, and is rooted at `/raw/v1/`.

It dynamically generates the API according to what's in the database, so keeps up automatically with any updates. This is what enables you to extend the schema according to your needs.

It looks slightly cumbersome, using a `/<type>/<uid>/<relationship>/<type>/<uid>...` format, but its consistency and predictability make automation easier.


## Create a resource
```
POST /api/v1/<resource-type>/
```

With payload of `uid=<uid>`, plus optionally `<attribute-name>=<value>` pairs for any subset of the attributes defined for this resource type.

On success, returns a code of 201 and a JSON representation of the newly-created resource.

The UID must actually be unique for each resource-type. That is, if you define a `routers` resource and a `switches` resource, no two routers can have the same UID, but a router and a switch can. Bear this in mind when designing your schema.


## Retrieve a resource
```
GET /api/v1/<resource-type>/<uid>
```

Returns a JSON representation of the resource.


## Retrieve all resources of a given type
```
GET /api/v1/<resource-type>/
```

Returns a JSON representation of all resources of that type, or 404 if there aren't any.


## Delete a resource
```
DELETE /api/v1/<resource-type>
```
Requires a payload of `'uid=<uid>'`, and any other parameters are ignored.

Returns `204 (NO CONTENT)` on success.


## Create a relationship from one resource to another

Note that, due to the way Neo4J works, these are always directional.

```
POST /api/v1/<resource-type>/<Unique ID>/<relationship>
with parameter: 'target' = '/type/uid'
```

Parameter _must_ include `type` and `uid`, and _may_ also include `attributes`.

If the destination resource doesn't already exist, it will be automatically created first. This has to be done as a separate transaction; beware race-conditions where two clients try to create the same thing at the same time.


## Retrieve the type and UID of all resources to which this one has a specific relationship
```
GET /api/v1/<resource-type>/<Unique ID>/<relationship>
```


## Delete a relationship to another object
```
DELETE /api/v1/<resource-type>/<Unique ID>/<relationship>/<Unique ID>
```


## Search for objects to which this one has a particular kind of relationship, optionally matching a set of attribute/value pairs
```
GET /api/v1/<resource-type>/<Unique ID>/<relationship>/?<attribute>=<value>
```

Regular expressions based on [Java regexes](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html) can be used. Negation can be effected by putting `!` at the start of the regex.


## Search for objects with a set relationship to another resource

This is currently limited to one hop.
```
GET /api/v1/<resource-type>?outbound=<relationship>/<resource-type>/<resource-uid>
```

E.g, `GET /api/v1/devices?outbound=BusinessOwner/organisations/Sales`


## Create a resource that depends on another for its context
This is defined in the schema by adding the attribute `dependent=true` to the dependent `rgResource` definition, and by then adding the same attribute to the relationships to that resource-type from resource-types that are valid parents.
It's valid to create resources that depend on other dependent resources, with no limit to the depth of these chains.
```
POST /api/v1/<parent-type>/<parent-uid>/<relationship-type>
with parameters: 'type=<child-type>' and 'uid=<child-uid>' (both are required)
```

## Delete a dependent resource
Either use the `DELETE` method on the full path to the resource in question to remove it specifically, or pass the `delete-dependent=true` parameter to the API call to one of its parents further up the chain.
The `delete-dependent` parameter acts recursively downward from whatever resource is being deleted.


## Move a dependent resource from one parent to another
Note that the new parent must be a valid parent for the child resource, and the new relationship must also be a valid dependent relationship.
```
POST /api/v1/path/to/dependent/resource
with parameter: 'target=/uri/path/to/new/parent/and/relationship'
```

# IPAM (IP Address Management) API

While nothing actually _stops_ you using the Raw API to add, change and remove subnets and addresses, it's a tedious and error-prone process, especially if you add and remove subnets by hand - finding the parent subnet, moving the new child subnets and addresses under a new mid-level subnet, and doing all that in reverse when you remove them.

The IPAM API is aware of organisations and VRF-groups, in keeping with the rest of the system.

New subnets are automatically inserted under the most appropriate supernet, and subnets and IP addresses are automatically rehomed as subnets are created, deleted and resized.

GET requests are the search interface to the IPAM section:

- `/subnets`
- `/addresses`

Both return the URI for interacting with the subnet or address in question via the Raw API, which is what you need when you then link them to/from other resources, such as delegating a subnet to another suborganisation, or allocating an address to a device.

Note that the forward-slash in CIDR notation is replaced with an underscore, for URL friendliness. Thus, `198.51.100.0/24` becomes `198.51.100.0_24`. The address and prefix-length attributes are stored within the object in the database, so no information is actually lost.

## Examples

We'll use the organisation `myCompany`, the subnet `192.0.2.0/24` and [curl](https://curl.haxx.se/) as the HTTP client.

Insert the subnet:
```
curl -X POST -d 'org=myCompany' -d 'subnet=192.0.2.0/24' http://localhost:4950/ipam/v1/subnets

/organisations/myCompany/Subnets/ipv4Subnets/192.0.2.0_24
```

Find the subnet:
```
curl 'http://localhost:4950/ipam/v1/subnets?org=myCompany&subnet=192.0.2.0/24'

/organisations/myCompany/Subnets/ipv4Subnets/192.0.2.0_24
```

Delete it:
```
curl -X DELETE -d 'org=myCompany' -d 'subnet=192.0.2.0/24' http://localhost:4950/ipam/v1/subnets
```

Addresses work the same way, except with a URI ending in `/addresses` instead of `/subnets`.
