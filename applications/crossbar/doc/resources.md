/*
Section: Crossbar
Title: Resources
Language: en-US
*/

Resources represent external assets such as TDM hardware, SIP trunks, transcoders, and other remote termination/originating call services or equipment.

There are two levels of resources, global (or system-wide), and per-account (bring your own carrier). The JSON format for both is identical; only their location in the Kazoo database structure defines whether they are globally available or not.

## Jobs

It is possible to add numbers, in bulk, to an account using the Jobs API below. If a job fails to run, there is a recovery process that runs periodically to attempt to resume stalled jobs.

You can configure how frequently the system checks for failed jobs in `system_config/crossbar.resources`, using the `job_recover_timeout_s` key (defaults to 6 hours).

You can configure how what is considered a 'stalled' job by defining how old the job is (the last time the job document was modified) relative to the current time. Configure in `system_config/crossbar.resources`, using the `job_recover_threshold_s` key (defaults to 1 hour). If a job is not completed, and hasn't been modified in over an hour, there's a good chance the job executor died. A new job executor will be started to pick up where the old one left off.

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

### The JOBS API

It is helpful to be able to upload a list of numbers to a specific account, versus adding them one after another.

#### _GET_ - List of jobs (pending or completed)

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" 'http://server:8000/v2/accounts/{ACCOUNT_ID}/resources/jobs'
    {"auth_token": "{AUTH_TOKEN}",
     "data": [
        {
            "failures": 0,
            "successes": 2,
            "id": "201408-394de70ecf6f8252",
            "status": "pending",
            "timestamp": 63575950041,
            "resource_id":{RESOURCE_ID}
        },
        {
            "failures": 0,
            "successes": 1,
            "id": "201408-70766ed00a24",
            "status": "pending",
            "timestamp": 63575878379,
            "resource_id":{RESOURCE_ID}
        }
       ]
     "page_size": 2,
     "request_id": "{REQUEST_ID}",
     "revision": "6d25843f5a4fc66b24c635d13904635d",
     "start_key": 63573276761,
     "status": "success"
    }

Do note you can use the `created_from` and `created_to` flags to change to time period queried.

The keys `failures` and `successes` represent the count of how many numbers failed and succeeded, respectively.

#### _PUT_ - Create a new job to add a list of numbers

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" 'http://server:8000/v2/accounts/{ACCOUNT_ID}/resources/jobs' -d '{"data":{"numbers":["+12223334444", "+23334445555"], "resource_id":"{RESOURCE_ID}"}}'
    {"auth_token": "{AUTH_TOKEN}",
     "data": {
        "errors": {},
        "id": "201408-39512771f9d2d499",
        "resource_id":"{RESOURCE_ID}",
        "numbers": [
            "+12223334444"
        ],
        "successes": {}
     },
     "request_id": "{REQUEST_ID}",
     "revision": "1-3d73b81456af994775d6de995020875b",
     "status": "success"
    }

#### _GET_ - Get a job's status and other information

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" 'http://server:8000/v2/accounts/{ACCOUNT_ID}/resources/jobs/{JOB_ID}'
    {"auth_token": "{AUTH_TOKEN}",
     "data": {
        "resource_id": "{RESOURCE_ID}",
        "errors": {},
        "id": "201408-394de70ecf6f8252",
        "numbers": [
            "3148096310"
        ],
        "status": "pending",
        "successes": {},
        "timestamp": 63575950041
     },
     "request_id": "{REQUEST_ID}",
     "revision": "1-8300dfdd9dd1b79efc3bace9dbe603a2",
     "status": "success"
    }
