#!/usr/bin/python

import requests

URL='http://localhost:4950'
REST='%s/api/v1' % URL

def post(uri, data):
    response = requests.post('%s/%s' % (REST, uri), data=data)
    print '%s: %s' % (uri, response.status_code)

post('deviceRoles', {'uid': 'router'})
requests.get('%s/deviceRoles' % REST)
post('devices', {'uid': 'amchitka'})
requests.get('%s/devices' % REST)
post('devices/amchitka/Role/', {'target': '/deviceRoles/router'})
