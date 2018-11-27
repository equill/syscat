# Design decisions - the engine

...or, why I built it this way.

## Graph database

Relational databases grind to a halt with all the joins, especially given the profusion of many-to-many joins that are necessary for granting this level of flexibility.

The performance of graph databases, on the other hand, scales extremely well for this kind of problem.

Other advantages of graph databases here are that they mirror the data model much more naturally, and help emphasise the delineation between what things are and what they mean.


## Defining the schema within the database, and deriving the API dynamically from it

Graph databases don´t typically have a schema mechanism that parallels the one in relational databases; Neo4j is no exception. It´s arguably not required, but it _does_ aid communication of what is and is not expected to be in there - it gives a definitive reference for the data model, in a form that prevents it being distributed across unconnected files of source code.

The author also simply didn´t see a practical way of expressing the data model in the source-code without essentially replicating this in a less efficient manner.

One structural consideration is that it enables changes to the schema to be propagated instantly to the API without adding the complexity of a distributed caching layer.

The origin of the engine is also partly outright efficiency (or laziness, depending on your perspective). The author started writing the API the hard way, explicitly coding each endpoint individually, but quickly realised what a marathon grind this would turn into. So he detoured into writing this engine in order to implement it in a way that scales much better with time, as well as enabling the API to evolve rapidly in response to feedback.

Adding an API to query the schema was an obvious step to provide dynamically-generated documentation of the API in its current form. Extending that to enable updates makes it possible to provide end-users with a way to extend the data model to meet their own needs, which seamlessly extends the predefined one rather than clumsily bolting on a limited collection of attributes.


### Why not embrace the schemaless freedom of a graph database?

Many regard the lack of formal schemas in a graph database to be a feature, rather than a limitation. They see the schema mechanism of an RDBMS as something that limits their freedom to connect things in any which way. The author can see the value of this in code-driven rapid prototyping, or in capturing and encoding data that has no inherent structure, but this problem fits neither of those descriptions.

In a production application, predictability is valuable and, in a real-world IT environment, not all relationships make sense between all pairs of things. A CPU cannot be the business owner of a network interface, for example, nor will a VLAN group be allocated to a postcode. Hence the value of explicitly stating which things can have what relationships with which other things.

While implementing and working with this, the importance of cardinality emerged:

- one-to-one
- one-to-many, e.g. network interfaces to IP addresses
- many-to-one, e.g. devices to vendors
- many-to-many, e.g. IPsec VPNs between pairs of end-point addresses


## Separating what should be from what is

"In theory, theory and practice are the same. In practice, they're not."

Some (arguably most) users want to model what they intend their environment to look like. Others want to keep track of what´s actually there. The author thinks both are pretty good ideas, but came to recognise that not only is that best done by operating one instance of the application for each, but that there are aspects of each that is irrelevant in the other.

For example:

- IPAM is _exclusively_ a matter of what should be. Thus, the "discovered" instance has no IPAM API, to prevent any confusion over this.
- When describing what is, that is, the discovered state of the environment, there is no such thing as an address being allocated to an interface. Either it´s configured there or it isn´t.
- SNMP indices cannot be allocated and configured. They can only be discovered.
- VRF groups and VLAN groups are purely conceptual; they don´t exist anywere in a network device´s configuration.
- an address can be allocated to a device, perhaps because no decision has been made about which interface it will eventually be configured on, or because you´re mapping out a partner company´s environment and you simply don´t know. However, a discovered address is _always_ configured on an interface, not on a device or a routing instance within it.

Most things, however, are common to both. For example, VRFs and VLANs can be both intended to be configured on a device, and actually configured and discovered on it.

Having both also makes it feasible to compare a fully aggregated picture of both expectations and reality, regardless of the source and collection method of any piece of information, making it possible to easily compare both full pictures and thus close the loop.
