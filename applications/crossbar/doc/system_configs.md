

Manipulate documents in the system\_config database via Crossbar. You must be super\_duper\_admin to access this resource.

#### Sample cURL Requests

##### List all known configs

    curl -v -H "content-type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/system_configs

##### Create a new config

    curl -v -X PUT -H "content-type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/system_configs -d '{"data":{"id":"abc123", "foo":"bar"}}'

##### Get default config

    curl -v -H "content-type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/system_configs/abc123

##### Get node-specific config

    curl -v -H "content-type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/system_configs/abc123/kazoo_apps@server.com

##### Update default config

    curl -v -X POST -H "content-type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/system_configs/abc123 -d '{"data":{"foo":"baz"}}'

##### Update node specific config

    curl -v -X POST -H "content-type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/system_configs/abc123/ecallmgr@server.com -d '{"data":{"offthe":"chain"}}'

##### Delete node specific config

    curl -v -X DELETE -H "content-type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/system_configs/abc123/ecallmgr@server.com

##### Delete the whole config

    curl -v -X DELETE -H "content-type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/system_configs/abc123
