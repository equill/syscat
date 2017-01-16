#!/usr/bin/python

import requests

URL='http://localhost:4950/'
REST='%sapi/v1' % URL

requests.post('%s/deviceRoles' % REST, data={'uid': 'router'})
requests.get('%s/deviceRoles' % REST)
requests.post('%s/devices' % REST, data={'uid': 'amchitka'})
requests.get('%s/devices' % REST)
request.post('%s/devices/amchitka/Role/' % REST, data={'target': '/deviceRoles/router'})
