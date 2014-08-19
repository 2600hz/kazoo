/*
Section: Crossbar
Title: Resources
Language: en-US
*/

Resources represent external assets such as TDM hardware, SIP trunks, transcoders, and other remote termination/originating call services or equipment.

There are two levels of resources, global (or system-wide), and per-account (bring your own carrier). The JSON format for both is identical; only their location in the Kazoo database structure defines whether they are globally available or not.

## Crossbar

### URL Structure

When interacting with an account's resources, the URL structure is as one would expect: `/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}`. To modify the global resources, simply omit `/accounts/{ACCOUNT_ID}` from the URL (your auth token must have super-duper admin priviledges).

There are two deprecated API endpoints, `global_resources` and `local_resources`. These should continue to work as before, but it is recommended to use `resources` instead, using the presence of an account id to toggle whether the resource is global or not.

### Account Resources URI

`/v2/accounts/{ACCOUNT_ID}/resources`

This URI is used to manipulate the resources available to the account.

#### _GET_ - Fetch account resources:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" 'http://server:8000/v2/accounts/{ACCOUNT_ID}/resources'
    {"auth_token": "{AUTH_TOKEN}",
     "data": [
         {"enabled": true,
          "id": "8c8afd2d8fc055de888933189d0f93eb",
          "name": "Carrier1",
          "weight": "50"
         },
         {"enabled": true,
          "id": "0c23dd40580a985b6fbc8e9286805eea",
          "name": "Carrier2",
          "weight": "50"
         }
     ],
     "page_size": 2,
     "request_id": "{REQUEST_ID}",
     "revision": "{REVISION_ID}",
     "status": "success"
    }

#### _PUT_ - Update account resources:

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" 'http://server:8000/v2/accounts/{ACCOUNT_ID}/resources' -d '{"data":{"name":"Carrier 3", "gateways":[]}}'
    {"auth_token": "{AUTH_TOKEN}",
     "data": {
         "emergency": false,
         "enabled": true,
         "flags": [],
         "gateways": [],
         "grace_period": 5,
         "id": "caa784ef72ba2e23e517bc04a3b39bf1",
         "media": {
             "audio": {
                 "codecs": ["PCMU"]
             },
             "video": {
                 "codecs": []
             }
         },
         "name": "Carrier 3",
         "rules": [],
         "weight_cost": 50
     },
     "request_id": "{REQUEST_ID}",
     "revision": "{REVISION_ID}",
     "status": "success"
    }

#### _GET_ - Fetch details about a resource

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" 'http://server:8000/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}'
    {"auth_token": "{AUTH_TOKEN}",
     "data": {
         "caller_id_options": {
             "type": "external"
         },
         "emergency": false,
         "enabled": true,
         "flags": [],
         "gateways": [
             {"channel_selection": "ascending",
              "codecs": ["PCMU", "PCMA"],
              "custom_sip_headers": {},
              "emergency": false,
              "enabled": true,
              "endpoint_type": "sip",
              "format_from_uri": false,
              "invite_format": "route",
              "password": "DrWoody",
              "prefix": "+1",
              "progress_timeout": "6",
              "realm": "carrier1.com",
              "server": "carrier1.com",
              "skype_rr": true,
              "suffix": "100",
              "username": "blazemore"
          }
         ],
         "grace_period": 5,
         "id": "{RESOURCE_ID}",
         "media": {
             "audio": {
                 "codecs": ["PCMU"]
             },
             "video": {
                 "codecs": []
             }
         },
         "name": "Carrier1",
         "peer": false,
         "rules": [
             "^\\+{0,1}1{0,1}(\\d{10})$"
         ],
         "type": "local",
         "weight_cost": "50"
     },
     "request_id": "{REQUEST_ID}",
     "revision": "{REVISION_ID}",
     "status": "success"
    }

#### _POST_ - Edit details about a resource

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" 'http://server:8000/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}' -d '{"data":{"name":"Carrier 3", "gateways":[{"server":"carrier3.com"}]}}'
    {"auth_token": "{AUTH_TOKEN}"
     "data": {
         "emergency": false,
         "enabled": true,
         "flags": [],
         "gateways": [
             {"channel_selection": "ascending",
              "custom_sip_headers": {},
              "emergency": false,
              "enabled": true,
              "endpoint_type": "sip",
              "invite_format": "route",
              "server": "carrier3.com",
              "skype_rr": true
              }
         ],
         "grace_period": 5,
         "id": "{RESOURCE_ID}",
         "media": {
             "audio": {
                 "codecs": ["PCMU"]
             },
             "video": {
                 "codecs": []
             }
         },
         "name": "Carrier 3",
         "rules": [],
         "weight_cost": 50
     },
     "request_id": "{REQUEST_ID}",
     "revision": "{REVISION_ID}",
     "status": "success"
    }

#### _DELETE_ - Remove an account resource:

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}
