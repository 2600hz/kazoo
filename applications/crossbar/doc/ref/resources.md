# Resources

## About Resources

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



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resources

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/resources

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resources/jobs

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/jobs
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/resources/jobs

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/jobs
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/resources/collection

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/collection
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/resources/collection

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/collection
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resources/jobs/{JOB_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/jobs/{JOB_ID}
```

