# Syscat - the System Catalogue

"From spare parts to org charts"

**What problem does it solve?**

Tracking all the "things" in an IT environment, and all the relationships between them.

"All" is a key word here - this project was born from frustration with systems designed to cover only a subset, and that don't enable you to extend them directly to cover things they missed out.


**What is it?**

An API-based network catalogue/configuration database that actually covers everything, from hardware assets up to application interdependencies, through to the people and organisations that have any kind of stake in them.

The REST API is its primary interface; anything else, such as a web GUI, can be built on that. This is to make it automation-friendly, as well as to make it feasible for users to build the interface that suits them.

It's designed to represent the network you have, in whatever level of detail you actually have, without trying to force opinions about how you _should_ have architected it. It just has opinions about how that should be represented.

The storage layer is [Neo4j](https://neo4j.com), an extremely capable graph database. Its [Cypher query language](https://neo4j.com/docs/developer-manual/current/cypher/) is more flexible and expressive than any REST API can be.

Current development status: early beta. Breaking changes are still possible, but are uncommon.


# Installation

Summary: download and run the Docker image, using the environment variables described in the Configuration section below, along with a Neo4j database.

`docker-compose` files are available to make it easier to get started and try it out.


## Demo instance

A quick-and-easy installation, best for getting a feel for how Syscat works and what it can do.

- for modelling either the expected state of the network, or its discovered state, but not (sanely) both at the same time. For both, see the "full power" section below.
- no persistent storage.
- maps the listening ports for Syscat and Neo4j to non-default values, to avoid clashing with local (development) installations.
- exposes endpoints for querying the Neo4j database directly, to take advantage of the full expressiveness of the [Cypher query language](https://neo4j.com/docs/developer-manual/current/cypher/).
- requires [Docker in swarm mode](https://docs.docker.com/engine/swarm/) to run multiple containers in their own protected network. This is overhead I'd rather avoid, but it's the practical way to run both the app and its database server.


### Installation and startup

0. Ensure [Docker swarm mode is enabled](https://docs.docker.com/engine/swarm/swarm-tutorial/create-swarm/) on your workstation.
0. Download [docker-compose-expected.yml](https://github.com/equill/syscat_scripts/blob/master/docker/docker-compose-expected.yml), and modify it as necessary.
0. Execute `docker stack deploy -c docker-compose-expected.yml syscat`

After about a minute, you should be able to connect to Syscat via HTTP on port 4952, and to Neo4j via HTTP on 7679, or via Bolt (Neo4j's optimised protocol) on 7691. These are set in `docker-compose.yml`. The delay on startup is from creating the schema in the database.

E.g: assuming `docker0`'s address is 10.255.0.1, and your shell prompt is `$`:
```
$ curl http://10.255.0.1:4952/raw/v1/devices
[]

$ curl -X POST -d 'uid=router1' http://10.255.0.1:4952/raw/v1/devices
{"uid":"router1","original_uid":"router1"}

$ curl -X POST -d 'uid=router2' http://10.255.0.1:4952/raw/v1/devices
{"uid":"router2","original_uid":"router2"}

$ curl http://10.255.0.1:4952/raw/v1/devices
[{"uid":"router1","original_uid":"router1"},{"uid":"router2","original_uid":"router2"}]
```

For better readability, pipe the output through `jq`, and use the `-s` option to curl to silence the progress bar:
```
$ curl -s http://10.255.0.1:4952/raw/v1/devices | jq .
[
  {
    "uid": "router1",
    "original_uid": "router1"
  },
  {
    "uid": "router2",
    "original_uid": "router2"
  }
]
```

Note that on some systems, connections to localhost hang forever. It does listen on all addresses, so if localhost hangs you can just connect on any other address, e.g. the host computer's LAN address, or the gateway address for the `docker0` interface.


## Full power

More useful for real-world use: model both the expected and discovered state, but keep them separate and provide persistent storage for both.

- for modelling both the expected state and the discovered state of an IT environment, each in its own instance.
- involves two parallel instances, each with its own database, listening on separate ports.
- provides persistent data storage.
- as with the demo instance, exposes both databases via both protocols, and requires Docker in swarm mode.


### Installation and startup

0. Ensure [Docker swarm mode is enabled](https://docs.docker.com/engine/swarm/swarm-tutorial/create-swarm/) on your workstation.
0. Download [docker-compose-combined.yml](https://github.com/equill/syscat_scripts/blob/master/docker/docker-compose-combined.yml)
0. create two volumes: one called `disc_data` and one called `exp_data`. To create them on your workstation as a test:

    `docker volume create --driver local disc_data`

    `docker volume create --driver local exp_data`

0. Execute `docker stack deploy -c docker-compose-combined.yml syscat`

Once it's created the schemas in both databases, you should see processes listening on 6 TCP ports:

- Expected state:
    - 4952 = REST API for Syscat
    - 7678 = HTTP API for the Neo4j database
    - 7691 = Bolt endpoint for the Neo4j database
- Discovered state:
    - 4953 = REST API for Syscat
    - 7679 = HTTP API for the Neo4j database
    - 7692 = Bolt endpoint for the Neo4j database

It works exactly as in the previous example, except now you have a second instance listening on port 4953 to track the results of your discovery scripts.

### Why run two instances at once?

There's a crucial difference between what was _allocated_, and what is _configured_. You _can_ represent both within the same database, but it quickly gets messy and difficult to distinguish between them. A GUI can still blend both sources for presentation to the user, and good design can make it clear which is which.

In terms of operational security, this also makes it much simpler to prevent your discovery scripts from messing with the desired state of the environment, and to prevent users editing the discovered state by accident.

If you're using SNMP for monitoring, you can cleanly separate "this interface _should_ be monitored for availability" from "this interface's index in `ifTable` is 436".


## Configuration

On startup, Syscat checks for the following environment variables, with default values as shown:

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
    - default: don't use that. Set it to something hard to crack and keep it secret.

These are set in the `docker-compose` files linked above.


# Schema API

The output is raw JSON, so if you're using `curl` you'll want to run the output through something like `jq`.

To dump the whole schema: `/schema/v1`. Note that the `v1` part refers to the version of the API, *not* of the schema being reported on. There's no version-control of schemas, just a history of what versions have been installed, which is used by the engine to decide whether an update is needed.

To get the details of one resourcetype: `http://localhost:4951/schema/v1/resourcetype/newresource`

To add a top-level resourcetype called `newresource`:
```
curl -X POST -d 'notes=A new type of resource' -d 'dependent=false' http://localhost:4951/schema/v1/resourcetype/newresource
```

Both parameters are optional:

- `notes` allows you to add a descriptive comment about the resourcetype
- `dependent` denotes whether it exists only in the context of another resource. This defaults to false.

To remove it:
```
curl -X DELETE http://localhost:4951/schema/v1/resourcetype/newresource
```

*CAUTION*: when you delete a resourcetype from the schema, all instances of that resourcetype are removed along with it. You can only recover from this by restoring the database from backup, so use this command with care.

To add a relationship between resourcetypes:
```
curl -X POST -d 'dependent=false' -d 'cardinality=many:many' http://localhost:4951/schema/v1/<from-resourcetype>/<relationship>/to-resourcetype>
```

The parameters are optional here, too:

- `dependent` indicates whether the from-type is a parent of the to-type. Default is false.
- `cardinality` controls whether this relationship is `1:1`, `many:1`, `1:many` or `many:many`. Default is `many:many`.

Example:
```
curl -X POST -d 'dependent=true' -d 'cardinality=1:many' http://localhost:4951/schema/v1/devices/Interfaces/networkInterfaces
```

To delete a relationship between resourcetypes:
```
curl -X DELETE http://localhost:4951/schema/v1/<from-resource>/<relationship>/to-resource>
```


## Raw API

This provides most of the functionality, and is rooted at `/raw/v1/`.

It dynamically generates the API according to what's in the database, so keeps up automatically with any updates. This is what enables you to extend the schema according to your needs.

It looks slightly cumbersome, using a `/<type>/<uid>/<relationship>/<type>/<uid>...` format, but its consistency and predictability make automation easier.


## Create a resource
```
POST /raw/v1/<resource-type>/
```

With payload of `uid=<uid>`, plus optionally `<attribute-name>=<value>` pairs for any subset of the attributes defined for this resource type.

On success, returns a code of 201 and a JSON representation of the newly-created resource.

The UID must actually be unique for each resource-type. That is, if you define a `routers` resource and a `switches` resource, no two routers can have the same UID, but a router and a switch can. Bear this in mind when designing your schema.


## Retrieve a resource

```
GET /raw/v1/<resource-type>/<uid>
```

Returns a JSON representation of the resource.


## Retrieve all resources of a given type

```
GET /raw/v1/<resource-type>/
```

Returns a JSON representation of all resources of that type, or 404 if there aren't any.


## Delete a resource

```
DELETE /raw/v1/<resource-type>
```
Requires a payload of `'uid=<uid>'`, and any other parameters are ignored.

Returns `204 (NO CONTENT)` on success.


## Create a relationship from one resource to another

Note that, due to the way Neo4J works, these are always directional.

```
POST /raw/v1/<resource-type>/<Unique ID>/<relationship>
with parameter: 'target' = '/type/uid'
```

Parameter _must_ include `type` and `uid`, and _may_ also include `attributes`.

If the destination resource doesn't already exist, it will be automatically created first. This has to be done as a separate transaction; beware race-conditions where two clients try to create the same thing at the same time.


## Retrieve the type and UID of all resources to which this one has a specific relationship
```
GET /raw/v1/<resource-type>/<Unique ID>/<relationship>
```



```
DELETE /raw/v1/<resource-type>/<Unique ID>/<relationship>/<Unique ID>
```


## Search for objects to which this one has a particular kind of relationship, optionally matching a set of attribute/value pairs
```
GET /raw/v1/<resource-type>/<Unique ID>/<relationship>/?<attribute>=<value>
```

Regular expressions based on [Java regexes](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html) can be used. Negation can be effected by putting `!` at the start of the regex.


## Search for objects with a set relationship to another resource

This is currently limited to one hop.
```
GET /raw/v1/<resource-type>?outbound=<relationship>/<resource-type>/<resource-uid>
```

E.g, `GET /raw/v1/devices?outbound=BusinessOwner/organisations/Sales`


## Create a resource that depends on another for its context

This is defined in the schema by adding the attribute `dependent=true` to the dependent `rgResource` definition, and by then adding the same attribute to the relationships to that resource-type from resource-types that are valid parents.
It's valid to create resources that depend on other dependent resources, with no limit to the depth of these chains.
```
POST /raw/v1/<parent-type>/<parent-uid>/<relationship-type>
with parameters: 'type=<child-type>' and 'uid=<child-uid>' (both are required)
```


## Delete a dependent resource

Either use the `DELETE` method on the full path to the resource in question to remove it specifically, or pass the `delete-dependent=true` parameter to the API call to one of its parents further up the chain.
The `delete-dependent` parameter acts recursively downward from whatever resource is being deleted.


## Move a dependent resource from one parent to another

Note that the new parent must be a valid parent for the child resource, and the new relationship must also be a valid dependent relationship.
```
POST /raw/v1/path/to/dependent/resource
with parameter: 'target=/uri/path/to/new/parent/and/relationship'
```


# IPAM (IP Address Management) API

You _can_ use the Raw API to manage IP addresses and subnets, but it's a tedious and error-prone process: finding the parent subnet, moving the new child subnets and addresses under a newly-added mid-level subnet, and doing all that in reverse when you remove them.

The IPAM API is aware of organisations and VRF-groups, in keeping with the rest of the system.

New subnets are automatically inserted under the most appropriate supernet, and subnets and IP addresses are automatically rehomed as subnets are created, deleted and resized.

GET requests are the search interface to the IPAM section:

- `/subnets`
- `/addresses`

Arguments:

- org
- subnet
- vrf

All arguments are required when searching.

Both return the URI for interacting with the subnet or address in question via the Raw API, which is what you need when you then link them to/from other resources, such as delegating a subnet to another suborganisation, or allocating an address to a device.

Note that the forward-slash in CIDR notation is replaced with an underscore, for URL friendliness. Thus, `198.51.100.0/24` becomes `198.51.100.0_24`. The address and prefix-length attributes are stored within the object in the database, so no information is actually lost.

## Examples

We'll use the organisation `myCompany`, the subnet `192.0.2.0/24` and [curl](https://curl.haxx.se/) as the HTTP client.

Insert the subnet:
```
curl -X POST -d 'org=myCompany' -d 'subnet=192.0.2.0/24' -d 'vrf=default' http://localhost:4951/ipam/v1/subnets

/organisations/myCompany/Subnets/ipv4Subnets/192.0.2.0_24
```

Find the subnet:
```
curl 'http://localhost:4951/ipam/v1/subnets?org=myCompany&subnet=192.0.2.0/24'

/organisations/myCompany/Subnets/ipv4Subnets/192.0.2.0_24
```

Delete it:
```
curl -X DELETE -d 'org=myCompany' -d 'subnet=192.0.2.0/24' http://localhost:4951/ipam/v1/subnets
```

Addresses work the same way, except with a URI ending in `/addresses` instead of `/subnets`.


# Basic design

## Background thinking

- model the network as it is, however messed-up that might be.
- model the whole thing: _all_ the things and the people, and all the relationships between them.
- enable users to record whatever information they _do_ have on hand, and evolve the picture as new information comes to hand. E.g, you can assign an IP address to a host, then move it to the correct interface, then assign that interface to a routing instance, all without losing any information such as incoming links from other things.
- most networks interact with other networks in some way, some of which are operated by other organisations.
- production networks in midsize and greater organisations have multiple ASes and multiple VRF groups.
- API-first implementation, because
    - automation is crucial in network management. An API-first design means that anything you can do with the GUI (when one is eventually added) can be done by a script.
    - no one GUI design suits everybody. An API-first design means you can build your own on top, if the included one doesn't suit, or if you've timed out while waiting for me to build it.
- some things only make sense in the context of other things, e.g. interfaces only exist in the context of a device. These can be created as "dependent" resources, with relationships that denote which kinds of things they depend on.
- no one model fits all use-cases, and all organisations have some custom use-cases. Provide users with a way to extend and modify it to their own requirements, that integrates seamlessly with the one provided by default.


## Architecture

A web application server fronting a Neo4j graph database. The schema is defined in the database, and is used to dynamically construct the API in response to each query - this is the key to Syscat's flexibility.

Because the schema is in the database, you can query it to confirm the API that is actually in use via the URI `/schema/v1`.

There's a domain-specific API for IPAM functionality, because that just can't be properly served via the raw API. Naturally, it's at `/ipam/v1`.


### Why a graph database?

Relational databases just run out of breath - in practical terms, they can't provide the flexibility. RDF databases are optimised for offline (very large) analysis, and this absolutely needs to be an online system that's continually being updated.

Although I wasn't thinking in those terms when I began this project, it turned out that there's a crucial difference in the worldview of relational vs graph databases: graph databases separate what something _means_ from what it _is_, and use the relationships to represent that meaning in terms of context, where relational databases conflate the thing with its meaning. And Neo4j provides the ACID dependability that we've learned to rely on from an RDBMS.

For any one implication of this difference, you _can_ find a way to represent it in a relational database. However, all those join tables accumulate quickly, and the DBMS eventually just grinds to a halt. Don't get me wrong: I've been a fan of relational databases since I started in IT in the '90s, and I'm still very fond of them. There are just some problems for which they're not a good fit, and this is one of them.
