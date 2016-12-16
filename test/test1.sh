URL='http://localhost:4950/api/v1'

# Autonomous systems
curl -X POST -d 'uid=0' -d 'name=Internet' "$URL/as"
curl -X POST -d 'uid=1' -d 'name=HomeNetwork' "$URL/as"

# Subnet and addresses
curl -X POST -d 'uid=192.168.0.0/24' "$URL/ipv4Subnets"
curl -X POST -d 'uid=192.168.0.1' "$URL/ipv4Addresses"
curl -X POST -d 'uid=192.168.0.2' "$URL/ipv4Addresses"

# Device roles
curl -X POST -d 'uid=host' "$URL/deviceRoles"
curl -X POST -d 'uid=router' "$URL/deviceRoles"
curl -X POST -d 'uid=switch' "$URL/deviceRoles"

# Router
curl -X POST -d 'uid=amchitka' "$URL/devices"

# Hosts
curl -X POST -d 'uid=host1' "$URL/devices"
curl -X POST -d 'Type=host' "$URL/devices"
