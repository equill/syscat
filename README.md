# Syscat - the System Catalogue

"From spare parts to org charts"

A network configuration database that actually covers everything, from hardware assets up to application interdependencies, through to the people and organisations that have any kind of stake in them.

The REST API will be its primary interface; anything else will be built on that. Thus, it lends itself to being driven by scripts and by other systems.

[Restagraph](https://github.com/equill/restagraph) provides the core framework and ensures a self-documenting schema. Extensions, such as an IPAM API, will be built on top of that.
