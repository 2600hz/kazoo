# Resources

## About Resources

Resources represent external assets such as TDM hardware, SIP trunks, trans-coders, and other remote termination/originating call services or equipment.

There are two levels of resources, global (or system-wide), and per-account (bring your own carrier). The JSON format for both is identical; only their location in the Kazoo database structure defines whether they are globally available or not.

When interacting with an account's resources, the URL structure is as one would expect: `/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}`. To modify the global resources, simply omit `/accounts/{ACCOUNT_ID}` from the URL (your auth token must have super-duper admin privileges).

To perform bulk resource operations use the collections endpoints.

### About Adding Bulk Numbers

It is possible to add numbers, in bulk, to an account using the Jobs API below. If a job fails to run, there is a recovery process that runs periodically to attempt to resume stalled jobs.

You can configure how frequently the system checks for failed jobs in `system_config/crossbar.resources`, using the `job_recover_timeout_s` key (defaults to 6 hours).

You can configure how what is considered a 'stalled' job by defining how old the job is (the last time the job document was modified) relative to the current time. Configure in `system_config/crossbar.resources`, using the `job_recover_threshold_s` key (defaults to 1 hour). If a job is not completed, and hasn't been modified in over an hour, there's a good chance the job executor died. A new job executor will be started to pick up where the old one left off.

#### Schema

Schema for resources



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`emergency` | Determines if the resource represents emergency services | `boolean()` | `false` | `false` |  
`enabled` | Determines if the resource is currently enabled | `boolean()` | `true` | `false` |  
`flags.[]` |   | `string()` |   | `false` |  
`flags` | A list of flags that can be provided on the request and must match for the resource to be eligible | `array(string())` | `[]` | `false` |  
`flat_rate_blacklist` | Regex for determining if a number should not be eligible for flat-rate trunking | `string()` |   | `false` |  
`flat_rate_whitelist` | Regex for determining if the number is eligible for flat-rate trunking | `string()` |   | `false` |  
`format_from_uri` | When set to true requests to this resource will have a reformatted SIP From Header | `boolean()` |   | `false` |  
`formatters` | Schema for request formatters | `object()` |   | `false` |  
`from_uri_realm` | When formatting SIP From on outbound requests this can be used to override the realm | `string()` |   | `false` |  
`gateway_strategy` | The strategy of choosing gateways from list: sequential or random | `string('sequential' | 'random')` |   | `false` |  
`gateways.[].bypass_media` | The resource gateway bypass media mode | `boolean()` |   | `false` |  
`gateways.[].caller_id_type` | The type of caller id to use | `string('internal' | 'external' | 'emergency')` |   | `false` |  
`gateways.[].channel_selection` | Automatic selection of the channel within the span: ascending starts at 1 and moves up; descending is the opposite | `string('ascending' | 'descending')` | `ascending` | `false` |  
`gateways.[].codecs.[]` |   | `string('G729' | 'PCMU' | 'PCMA' | 'G722_16' | 'G722_32' | 'CELT_48' | 'CELT_64' | 'Speex' | 'GSM' | 'OPUS' | 'H261' | 'H263' | 'H264' | 'VP8')` |   | `false` |  
`gateways.[].codecs` | A list of single list codecs supported by this gateway (to support backward compatibility) | `array(string('G729' | 'PCMU' | 'PCMA' | 'G722_16' | 'G722_32' | 'CELT_48' | 'CELT_64' | 'Speex' | 'GSM' | 'OPUS' | 'H261' | 'H263' | 'H264' | 'VP8'))` |   | `false` |  
`gateways.[].custom_sip_headers.in` | Custom SIP Headers to be applied to calls inbound to Kazoo from the endpoint | [#/definitions/custom_sip_headers](#custom_sip_headers) |   | `false` |  
`gateways.[].custom_sip_headers.out` | Custom SIP Headers to be applied to calls outbound from Kazoo to the endpoint | [#/definitions/custom_sip_headers](#custom_sip_headers) |   | `false` |  
`gateways.[].custom_sip_headers.^[a-zA-z0-9_\-]+$` | The SIP header to add | `string()` |   | `false` |  
`gateways.[].custom_sip_headers` | A property list of SIP headers | `object()` |   | `false` |  
`gateways.[].custom_sip_interface` | The name of a custom SIP interface | `string()` |   | `false` |  
`gateways.[].enabled` | Determines if the resource gateway is currently enabled | `boolean()` | `true` | `false` |  
`gateways.[].endpoint_type` | What type of endpoint is this gateway | `string('sip' | 'freetdm' | 'skype' | 'amqp')` | `sip` | `false` |  
`gateways.[].force_port` | Allow request only from this port | `boolean()` | `false` | `false` |  
`gateways.[].format_from_uri` | When set to true requests to this resource gateway will have a reformatted SIP From Header | `boolean()` |   | `false` |  
`gateways.[].from_uri_realm` | When formatting SIP From on outbound requests this can be used to override the realm | `string()` |   | `false` |  
`gateways.[].invite_format` | The format of the DID needed by the underlying hardware/gateway | `string('route' | 'username' | 'e164' | 'npan' | '1npan')` | `route` | `false` |  
`gateways.[].invite_parameters.dynamic.[]` |   | `string()|string()|string('zone')|object()` |   |   |  
`gateways.[].invite_parameters.dynamic` | A list of properties that, if found on the inbound call, should be added as an INVITE parameter | `array()` |   | `false` |  
`gateways.[].invite_parameters.static.[]` |   | `string()` |   | `false` |  
`gateways.[].invite_parameters.static` | A list of static values that should be added as INVITE parameters | `array(string())` |   | `false` |  
`gateways.[].invite_parameters` |   | `object()` |   | `false` |  
`gateways.[].media.fax_option` | Is T.38 Supported? | `boolean()` |   | `false` |  
`gateways.[].media.rtcp_mux` | RTCP protocol messages mixed with RTP data | `boolean()` |   | `false` |  
`gateways.[].media` | The media parameters for the resource gateway | `object()` |   | `false` |  
`gateways.[].password` | SIP authentication password | `string(0..32)` |   | `false` |  
`gateways.[].port` | This resource gateway port | `integer()` | `5060` | `false` |  
`gateways.[].prefix` | A string to prepend to the dialed number or capture group of the matching rule | `string(0..64)` |   | `false` |  
`gateways.[].progress_timeout` | The progress timeout to apply to the resource gateway | `integer()` |   | `false` |  
`gateways.[].realm` | This resource gateway authentication realm | `string(0..64)` |   | `false` |  
`gateways.[].route` | A statically configured SIP URI to route all call to | `string()` |   | `false` |  
`gateways.[].server` | This resource gateway server | `string(1..128)` |   | `true` |  
`gateways.[].skype_interface` | The name of the Skype interface to route the call over | `string()` |   | `false` |  
`gateways.[].skype_rr` | Determines whether to round-robin calls amongst all interfaces (overrides "skype_interface" setting) | `boolean()` | `true` | `false` |  
`gateways.[].span` | The identity of the hardware on the media server | `string()` |   | `false` |  
`gateways.[].suffix` | A string to append to the dialed number or capture group of the matching rule | `string(0..64)` |   | `false` |  
`gateways.[].username` | SIP authentication username | `string(0..32)` |   | `false` |  
`gateways` | A list of gateways available for this resource | `array(object())` |   | `true` |  
`grace_period` | The amount of time, in seconds, to wait before starting another resource | `integer()` | `5` | `false` |  
`ignore_flags` | When set to true this resource is used if the rules/classifiers match regardless of flags | `boolean()` |   | `false` |  
`media` | Media options for resources | [#/definitions/endpoint.media](#endpointmedia) |   | `false` |  
`name` | A friendly name for the resource | `string(1..128)` |   | `true` |  
`require_flags` | When set to true this resource is ignored if the request does not specify outbound flags | `boolean()` |   | `false` |  
`rules.[]` |   | `string()` |   | `false` |  
`rules` | A list of regular expressions of which one must match for the rule to be eligible, they can optionally contain capture groups | `array(string())` | `[]` | `false` |  
`weight_cost` | A value between 0 and 100 that determines the order of resources when multiple can be used | `integer()` | `50` | `false` |  

### custom_sip_headers

Custom SIP headers applied to an INVITE


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`^[a-zA-z0-9_\-]+$` | The SIP header to add | `string()` |   | `false` |  

### endpoint.media

Schema for endpoint media options


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`audio.codecs.[]` |   | `string('OPUS' | 'CELT@32000h' | 'G7221@32000h' | 'G7221@16000h' | 'G722' | 'speex@32000h' | 'speex@16000h' | 'PCMU' | 'PCMA' | 'G729' | 'GSM' | 'CELT@48000h' | 'CELT@64000h' | 'G722_16' | 'G722_32' | 'CELT_48' | 'CELT_64' | 'Speex' | 'speex')` |   | `false` |  
`audio.codecs` | A list of audio codecs the endpoint supports | `array(string('OPUS' | 'CELT@32000h' | 'G7221@32000h' | 'G7221@16000h' | 'G722' | 'speex@32000h' | 'speex@16000h' | 'PCMU' | 'PCMA' | 'G729' | 'GSM' | 'CELT@48000h' | 'CELT@64000h' | 'G722_16' | 'G722_32' | 'CELT_48' | 'CELT_64' | 'Speex' | 'speex'))` |   | `false` |  
`audio` | The audio media parameters | `object()` | `{}` | `false` |  
`bypass_media` | Default bypass media mode (The string type is deprecated, please use this as a boolean) | `boolean() | string('auto' | 'false' | 'true')` |   | `false` |  
`encryption.enforce_security` | Is Encryption Enabled? | `boolean()` | `false` | `false` |  
`encryption.methods.[]` |   | `string('zrtp' | 'srtp')` |   | `false` |  
`encryption.methods` | Supported Encryption Types | `array(string('zrtp' | 'srtp'))` | `[]` | `false` |  
`encryption` | Encryption Parameters | `object()` | `{}` | `false` |  
`fax_option` | Is T.38 Supported? | `boolean()` |   | `false` |  
`ignore_early_media` | The option to determine if early media from the endpoint should always be ignored | `boolean()` |   | `false` |  
`progress_timeout` | The progress timeout to apply to the endpoint (seconds) | `integer()` |   | `false` |  
`video.codecs.[]` |   | `string('H261' | 'H263' | 'H264' | 'VP8')` |   | `false` |  
`video.codecs` | A list of video codecs the endpoint supports | `array(string('H261' | 'H263' | 'H264' | 'VP8'))` | `[]` | `false` |  
`video` | The video media parameters | `object()` | `{}` | `false` |  

### formatters

Schema for request formatters


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`^[[:alnum:]_]+$` | Key to match in the route request JSON | `array([#/definitions/formatters.format_options](#formattersformat_options)) | [#/definitions/formatters.format_options](#formattersformat_options)` |   | `false` |  

### formatters.format_options

Schema for formatter options


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`direction` | Only apply the formatter on the relevant request direction | `string('inbound' | 'outbound' | 'both')` |   | `false` |  
`match_invite_format` | Applicable on fields with SIP URIs. Will format the username portion to match the invite format of the outbound request. | `boolean()` |   | `false` |  
`prefix` | Prepends value against the result of a successful regex match | `string()` |   | `false` |  
`regex` | Matches against the value, with optional capture group | `string()` |   | `false` |  
`strip` | If set to true, the field will be stripped from the payload | `boolean()` |   | `false` |  
`suffix` | Appends value against the result of a successful regex match | `string()` |   | `false` |  
`value` | Replaces the current value with the static value defined | `string()` |   | `false` |  



## INVITE Parameters

The `INVITE` parameters object defines both static and dynamic parameters that should be added to the request URI.

Static parameters are added 'as-is' and can be any format.  However, they should follow the SIP standard for the header field format and should not include a semi-colon.

Dynamic parameters obtain the value from properties of the initiating call (requestor) if present, and are ignored if not. Dynamic parameters can be defined either as a string or an object.  When defined as a string the property is extracted from the requestor and if found the resulting value used without modification as an `INVITE` parameter.  When defined as an object both a tag as well as a key property must be defined.  The key property is used to extract the value from the requestor and the tag is appended as the `INVITE` parameter name.  By default the `INVITE` parameter name and value are separated by an equals sign but this can be overridden by providing a separator property.

For example, if a resource gateway contains the following object:

```
           "invite_parameters": {
               "dynamic": [
                   "custom_channel_vars.pass-through",
                   {
                       "tag": "id",
                       "key": "custom_channel_vars.account_id"
                   }
               ],
               "static": [
                   "npid"
               ]
           }
```

and assuming the requesting call has pass-through (with value `pass-through=0288`) as well as account_id (with value `XXXX`) custom channel variables it will result in an `INVITE` request URI such as:

```
INVITE sip:+14158867900@10.26.0.88;npid;id=XXXX;pass-through=0288 SIP/2.0
```


## Fetch

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

## Create a new resource

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

## Remove a resource

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

## Fetch a resource

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

## Change a resource

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

## Patch a resource

> PATCH /v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"custom_sip_headers":{"X-Reseller-ID":"a1b2c3"}}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "caller_id_options": {
            "type": "external"
        },
        "custom_sip_headers": {
            "X-Reseller-ID": "a1b2c3"
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


## Fetch a listing of jobs

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
            "resource_id":"{RESOURCE_ID}"
        },
        {
            "failures": 0,
            "successes": 1,
            "id": "201408-70766ed00a24",
            "status": "pending",
            "timestamp": 63575878379,
            "resource_id":"{RESOURCE_ID}"
        }
    ]
    "page_size": 2,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "start_key": 63573276761,
    "status": "success"
}
```

## Create a new job

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

## Fetch a job's status

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

## Create a new collection of resources

> PUT /v2/accounts/{ACCOUNT_ID}/resources/collection

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":[{...RESOURCE...}, {...RESOURCE...}]}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/collection
```

## Change a collection

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
            "{RESOURCE_ID}": "{ERROR_MESSAGE}"
        },
        "successes":{
            "{RESOURCE_ID}": "{RESOURCE_DOC}"
        }
    }
}
```
