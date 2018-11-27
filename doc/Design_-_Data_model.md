# Design decisions - the data model

...or, "why do you call it one thing in one place, and something different in another?"

There are many distinctions which seem subtle and nit-picking on the surface, which can lead to all sorts of havoc in an operating network when admins are confused by them or gloss over the differences. The author has witnessed many of these, and this is why Syscat´s data model is designed to call out these differences in the hope of reducing the confusion that leads to problems later.

Most of these differences come down to how things are thought of when being allocated, versus how they actually behave when configured.

## IP addresses vs subnets vs addresses on interfaces

These are several different things, and Syscat is designed to distinguish between them:

- subnets, as allocated via IPAM (IP Address Management)
    - represented as the lowest address in the subnet, plus the prefix-length for the subnet
    - has the relationship `Subnets` to a parent subnet (of the same protocol), VRF-group or organisation
    - can have the relationship `Allocated` to a VLAN-group
- addresses, as allocated via IPAM
    - represented as an address, _without_ a prefix-length
    - has the relationship `Addresses` to a parent subnet - this is the context in which it´s defined
    - can have the relationship 

### Why? Because IP addresses are more complicated than they look

An address is simple, even if you qualify it with a netmask or prefix-length to denote membership of a subnet. But addresses are never used in isolation, as abstract concepts.

- is that an address as allocated to an interface?
- is that an address configured on an interface?
- can you safely assume that the address configured on an interface is the same as what was allocated?
    - or that an address is configured on it at all?
    - or that _only_ the addresses allocated are the ones configured?
    - the answer to all these questions, as is obvious to anybody with experience, is no.
- is that a subnet, defined by the network address (IPv4) or the bottom-most address in the subnet (IPv6) with a prefix-length or (for IPv4) a netmask?
    - could be. But these only make sense in the context of allocation. There´s no way to configure a subnet on a VLAN, because the idea doesn´t make sense.
    - these can´t be configured on interfaces, either. The prefix-length on an interface´s address defines a filter used by the TCP/IP stack to decide which packets to respond to and which to ignore. Again, this is learned by experience, from troubleshooting misbehaviour stemming from a misconfigured prefix-length.
- is that an address allocated by DNS? If it is, how does it relate to what´s allocated to a device or interface, to what´s configured in reality, and to what interface a packet reaches?
    - it may or may not relate to what´s allocated. The chances of a disconnect increase when allocation and DNS are handled by different teams, and again when those teams don´t share a single reference system.
    - if a host is connected to more than one network (e.g, via a remote-access VPN to a second network), it´s possible to resolve a domain name within one network, and get routed through another network to the same address in a different network, and connect to a system you didn´t expect. The author has witnessed this happen, and would like to help people spot these situations _before_ users discover them the hard way.


## VLANs and VRFs vs VLAN-groups and VRF-groups

When allocated, they´re groups. When they´re configured, they´re VLANs and VRFs that may or may not bear any relationship to what was allocated, or to those on connected devices or interfaces - and this doesn´t always mean they won´t cheerfully forward frames or packets for each other.


## Physical locations

Why are these in both Expected and Discovered schemas?

They very nearly weren´t. However, it´s entirely possible that you´d want to perform a physical audit, in the same manner as performing discovery via SNMP, and it´s equally possible that things are somewhere other than where you expected.
