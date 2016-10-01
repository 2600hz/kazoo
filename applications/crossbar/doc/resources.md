### Resources

#### About Resources

Resources represent external assets such as TDM hardware, SIP trunks, transcoders, and other remote termination/originating call services or equipment.

There are two levels of resources, global (or system-wide), and per-account (bring your own carrier). The JSON format for both is identical; only their location in the Kazoo database structure defines whether they are globally available or not.

When interacting with an account's resources, the URL structure is as one would expect: `/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}`. To modify the global resources, simply omit `/accounts/{ACCOUNT_ID}` from the URL (your auth token must have super-duper admin privileges).

There are two deprecated API endpoints, `global_resources` and `local_resources`. These should continue to work as before, but it is recommended to use `resources` instead, using the presence of an account id to toggle whether the resource is global or not.

#### About Adding Bulk Numbers

It is possible to add numbers, in bulk, to an account using the Jobs API below. If a job fails to run, there is a recovery process that runs periodically to attempt to resume stalled jobs.

You can configure how frequently the system checks for failed jobs in `system_config/crossbar.resources`, using the `job_recover_timeout_s` key (defaults to 6 hours).

You can configure how what is considered a 'stalled' job by defining how old the job is (the last time the job document was modified) relative to the current time. Configure in `system_config/crossbar.resources`, using the `job_recover_threshold_s` key (defaults to 1 hour). If a job is not completed, and hasn't been modified in over an hour, there's a good chance the job executor died. A new job executor will be started to pick up where the old one left off.

#### Resources Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`emergency` | Determines if the resource represents emergency services | `boolean` | `false` | `false`
`enabled` | Determines if the resource is currently enabled | `boolean` | `true` | `false`
`flags` | A list of flags that can be provided on the request and must match for the resource to be eligible | `array(string)` | `[]` | `false`
`flags.[]` |   | `string` |   | `false`
`format_from_uri` | When set to true requests to this resource will have a re-formated SIP From Header | `boolean` |   | `false`
`from_uri_realm` | When formating SIP From on outbound requests this can be used to override the realm | `string` |   | `false`
`gateways` | A list of gateways avaliable for this resource | `array(object)` |   | `true`
`gateways.[].bypass_media` | The resource gateway bypass media mode | `boolean` |   | `false`
`gateways.[].caller_id_type` | The type of caller id to use | `string('internal', 'external', 'emergency')` |   | `false`
`gateways.[].channel_selection` | Automatic selection of the channel within the span: ascending starts at 1 and moves up; descending is the opposite | `string('ascending', 'descending')` | `ascending` | `false`
`gateways.[].codecs` | A list of single list codecs supported by this gateway (to support backward compatibilty) | `array(string('G729', 'PCMU', 'PCMA', 'G722_16', 'G722_32', 'CELT_48', 'CELT_64', 'Speex', 'GSM', 'OPUS', 'H261', 'H263', 'H264', 'VP8'))` |   | `false`
`gateways.[].codecs.[]` |   | `string` |   | `false`
`gateways.[].custom_sip_headers` |   | `object` | `{}` | `false`
`gateways.[].custom_sip_interface` | The name of a custom SIP interface | `string` |   | `false`
`gateways.[].enabled` | Determines if the resource gateway is currently enabled | `boolean` | `true` | `false`
`gateways.[].endpoint_type` | What type of endpoint is this gateway | `string('sip', 'freetdm', 'skype', 'amqp')` | `sip` | `false`
`gateways.[].force_port` | Allow request only from this port | `boolean` | `false` | `false`
`gateways.[].format_from_uri` | When set to true requests to this resource gateway will have a re-formated SIP From Header | `boolean` |   | `false`
`gateways.[].from_uri_realm` | When formating SIP From on outbound requests this can be used to override the realm | `string` |   | `false`
`gateways.[].invite_format` | The format of the DID needed by the underlying hardware/gateway | `string('route', 'username', 'e164', 'npan', '1npan')` | `route` | `false`
`gateways.[].media` | The media parameters for the resource gateway | `object` |   | `false`
`gateways.[].media.fax_option` | Support T.38 | `boolean` |   | `false`
`gateways.[].password` | SIP authentication password | `string(0..32)` |   | `false`
`gateways.[].port` | This resource gateway port | `integer` | `5060` | `false`
`gateways.[].prefix` | A string to prepend to the dialed number or capture group of the matching rule | `string(0..64)` |   | `false`
`gateways.[].progress_timeout` | The progress timeout to apply to the resource gateway | `integer` |   | `false`
`gateways.[].realm` | This resource gateway authentication realm | `string(0..64)` |   | `false`
`gateways.[].route` | A staticly configured SIP URI to route all call to | `string` |   | `false`
`gateways.[].server` | This resource gateway server | `string(1..64)` |   | `true`
`gateways.[].skype_interface` | The name of the Skype interface to route the call over | `string` |   | `false`
`gateways.[].skype_rr` | Determines whether to round-robin calls amongst all interfaces (overrides "skype_interface" setting) | `boolean` | `true` | `false`
`gateways.[].span` | The identity of the hardware on the media server | `string` |   | `false`
`gateways.[].suffix` | A string to append to the dialed number or capture group of the matching rule | `string(0..64)` |   | `false`
`gateways.[].username` | SIP authentication username | `string(0..32)` |   | `false`
`grace_period` | The amount of time, in seconds, to wait before starting another resource | `integer` | `5` | `false`
`media` | The default resouce media parameters applied if not present to all specified gateways | `object` | `{}` | `false`
`media.audio` | The default audio media parameters | `object` | `{}` | `false`
`media.audio.codecs` | A list of default codecs to use | `array(string('OPUS', 'CELT@32000h', 'G7221@32000h', 'G7221@16000h', 'G722', 'speex@32000h', 'speex@16000h', 'PCMU', 'PCMA', 'G729', 'GSM', 'CELT@48000h', 'CELT@64000h', 'G722_16', 'G722_32', 'CELT_48', 'CELT_64', 'Speex', 'speex'))` | `PCMU` | `false`
`media.audio.codecs.[]` |   | `string` |   | `false`
`media.bypass_media` | Default bypass media mode | `boolean` |   | `false`
`media.fax_option` | Support T.38 | `boolean` |   | `false`
`media.video` | The default video media parameters | `object` | `{}` | `false`
`media.video.codecs` | A list of default codecs to use | `array(string('H261', 'H263', 'H264', 'VP8'))` | `[]` | `false`
`media.video.codecs.[]` |   | `string` |   | `false`
`name` | A friendly name for the resource | `string(1..128)` |   | `true`
`require_flags` | When set to true this resource is ignored if the request does not specify outbound flags | `boolean` |   | `false`
`rules` | A list of regular expressions of which one must match for the rule to be eligible, they can optionally contain capture groups | `array(string)` | `[]` | `false`
`rules.[]` |   | `string` |   | `false`
`weight_cost` | A value between 0 and 100 that determines the order of resources when multiple can be used | `integer` | `50` | `false`

#### Fetch an account's resources

> GET /v2/accounts/{ACCOUNT_ID}/resources

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
         {"enabled": true,
          "id": "{RESOURCE_ID}",
          "name": "Carrier1",
          "weight": "50"
         },
         {"enabled": true,
          "id": "{RESOURCE_ID}",
          "name": "Carrier2",
          "weight": "50"
         }
    ],
    "page_size": 2,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION_ID}",
    "status": "success"
}
```

#### Create a new resource

> PUT /v2/accounts/{ACCOUNT_ID}/resources

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"name":"Carrier 3", "gateways":[]}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "emergency": false,
        "enabled": true,
        "flags": [],
        "gateways": [],
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
```

#### Remove a resource

> DELETE /v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "caller_id_options": {
            "type": "external"
        },
        "emergency": false,
        "enabled": true,
        "flags": [],
        "gateways": [
            {
                "channel_selection": "ascending",
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
        "name": "Carrier 3",
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
```

#### Fetch a resource

> GET /v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "caller_id_options": {
            "type": "external"
        },
        "emergency": false,
        "enabled": true,
        "flags": [],
        "gateways": [
            {
                "channel_selection": "ascending",
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
        "name": "Carrier 3",
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
```

#### Change a resource

> POST /v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{...ResourceData...}}'
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "caller_id_options": {
            "type": "external"
        },
        "emergency": false,
        "enabled": true,
        "flags": [],
        "gateways": [
            {
                "channel_selection": "ascending",
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
        "name": "Carrier 3",
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
```

#### Fetch a listing of jobs

Do note you can use the `created_from` and `created_to` flags to change to time period queried.

The keys `failures` and `successes` represent the count of how many numbers failed and succeeded, respectively.

> GET /v2/accounts/{ACCOUNT_ID}/resources/jobs

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/jobs
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
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
    "revision": "{REVISION}",
    "start_key": 63573276761,
    "status": "success"
}
```

#### Create a new job

> PUT /v2/accounts/{ACCOUNT_ID}/resources/jobs

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"numbers":["+12223334444", "+23334445555"], "resource_id":"{RESOURCE_ID}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/jobs
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
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
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Change a collection

> POST /v2/accounts/{ACCOUNT_ID}/resources/collection

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"numbers":["+12223334444", "+23334445555"], "resource_id":"{RESOURCE_ID}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/collection
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data":{
        "errors":{
            "{RESOURCE_ID}":"{ERROR_MESSAGE}"
        },
        "successes":{
            "{RESOURCE_ID}":{RESOURCE_DOC}
        }
    }
}
```

#### Create a new collection of resources

> PUT /v2/accounts/{ACCOUNT_ID}/resources/collection

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":[{...RESOURCE...}, {...RESOURCE...}]}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/collection
```

#### Fetch a job's status

> GET /v2/accounts/{ACCOUNT_ID}/resources/jobs/{JOB_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/jobs/{JOB_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
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
    "revision": "{REVISION}",
    "status": "success"
}
```
