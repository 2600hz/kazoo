### Resources

#### About Resources

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`emergency` | Determines if the resource represents emergency services | `boolean` | `false` | `false`
`enabled` | Determines if the resource is currently enabled | `boolean` | `true` | `false`
`flags` | A list of flags that can be provided on the request and must match for the resource to be eligible | `array` | `[]` | `false`
`flags.[]` |   | `string` |   | `false`
`format_from_uri` | When set to true requests to this resource will have a re-formated SIP From Header | `boolean` |   | `false`
`from_uri_realm` | When formating SIP From on outbound requests this can be used to override the realm | `string` |   | `false`
`gateways` | A list of gateways avaliable for this resource | `array` |   | `true`
`gateways.[].bypass_media` | The resource gateway bypass media mode | `boolean` |   | `false`
`gateways.[].caller_id_type` | The type of caller id to use | `string` |   | `false`
`gateways.[].channel_selection` | Automatic selection of the channel within the span: ascending starts at 1 and moves up; descending is the opposite | `string` | `ascending` | `false`
`gateways.[].codecs` | A list of single list codecs supported by this gateway (to support backward compatibilty) | `array` |   | `false`
`gateways.[].codecs.[]` |   | `string` |   | `false`
`gateways.[].custom_sip_headers` |   | `object` | `{}` | `false`
`gateways.[].custom_sip_interface` | The name of a custom SIP interface | `string` |   | `false`
`gateways.[].emergency` | Determines if the resource gateway represents emergency services | `boolean` | `false` | `false`
`gateways.[].enabled` | Determines if the resource gateway is currently enabled | `boolean` | `true` | `false`
`gateways.[].endpoint_type` | What type of endpoint is this gateway | `string` | `sip` | `false`
`gateways.[].force_port` | Allow request only from this port | `boolean` | `false` | `false`
`gateways.[].format_from_uri` | When set to true requests to this resource gateway will have a re-formated SIP From Header | `boolean` |   | `false`
`gateways.[].from_uri_realm` | When formating SIP From on outbound requests this can be used to override the realm | `string` |   | `false`
`gateways.[].invite_format` | The format of the DID needed by the underlying hardware/gateway | `string` | `route` | `false`
`gateways.[].media` | The media parameters for the resource gateway | `object` |   | `false`
`gateways.[].media.fax_option` | Support T.38 | `boolean` |   | `false`
`gateways.[].password` | SIP authentication password | `string` |   | `false`
`gateways.[].port` | This resource gateway port | `integer` | `5060` | `false`
`gateways.[].prefix` | A string to prepend to the dialed number or capture group of the matching rule | `string` |   | `false`
`gateways.[].progress_timeout` | The progress timeout to apply to the resource gateway | `integer` |   | `false`
`gateways.[].realm` | This resource gateway authentication realm | `string` |   | `false`
`gateways.[].route` | A staticly configured SIP URI to route all call to | `string` |   | `false`
`gateways.[].server` | This resource gateway server | `string` |   | `true`
`gateways.[].skype_interface` | The name of the Skype interface to route the call over | `string` |   | `false`
`gateways.[].skype_rr` | Determines whether to round-robin calls amongst all interfaces (overrides "skype_interface" setting) | `boolean` | `true` | `false`
`gateways.[].span` | The identity of the hardware on the media server | `string` |   | `false`
`gateways.[].suffix` | A string to append to the dialed number or capture group of the matching rule | `string` |   | `false`
`gateways.[].username` | SIP authentication username | `string` |   | `false`
`grace_period` | The amount of time, in seconds, to wait before starting another resource | `integer` | `5` | `false`
`media` | The default resouce media parameters applied if not present to all specified gateways | `object` | `{}` | `false`
`media.audio` | The default audio media parameters | `object` | `{}` | `false`
`media.audio.codecs` | A list of default codecs to use | `array` | `PCMU` | `false`
`media.audio.codecs.[]` |   | `string` |   | `false`
`media.bypass_media` | Default bypass media mode | `boolean` |   | `false`
`media.fax_option` | Support T.38 | `boolean` |   | `false`
`media.video` | The default video media parameters | `object` | `{}` | `false`
`media.video.codecs` | A list of default codecs to use | `array` | `[]` | `false`
`media.video.codecs.[]` |   | `string` |   | `false`
`name` | A friendly name for the resource | `string` |   | `true`
`require_flags` | When set to true this resource is ignored if the request does not specify outbound flags | `boolean` |   | `false`
`rules` | A list of regular expressions of which one must match for the rule to be eligible, they can optionally contain capture groups | `array` | `[]` | `false`
`rules.[]` |   | `string` |   | `false`
`weight_cost` | A value between 0 and 100 that determines the order of resources when multiple can be used | `integer` | `50` | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNTID}/resources

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/resources
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/resources

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/resources
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/resources/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/resources/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/resources/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/resources/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/resources/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/resources/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/resources/jobs

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/resources/jobs
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/resources/jobs

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/resources/jobs
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/resources/collection

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/resources/collection
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/resources/collection

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/resources/collection
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/resources/jobs/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/resources/jobs/{ID}
```

