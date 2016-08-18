### Resources

#### About Resources

#### Schema

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
`media.audio.codecs` | A list of default codecs to use | `array(string('OPUS', 'CELT@32000h', 'G7221@32000h', 'G7221@16000h', 'G722', 'speex@32000h', 'speex@16000h', 'PCMU', 'PCMA', 'G729', 'GSM', 'CELT@48000h', 'CELT@64000h', 'G722_16', 'G722_32', 'CELT_48', 'CELT_64', 'Speex', 'speex'))` | `["PCMU"]` | `false`
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


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resources

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/resources

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resources/jobs

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/jobs
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/resources/jobs

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/jobs
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/resources/collection

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/collection
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/resources/collection

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/collection
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resources/jobs/{JOB_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resources/jobs/{JOB_ID}
```

