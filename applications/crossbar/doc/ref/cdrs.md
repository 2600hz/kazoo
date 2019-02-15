# Cdrs

## About Cdrs

#### Schema

Call Detail Records



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`app_name` | The Kazoo application that issued the CDR | `string()` |   | `false` |  
`app_version` | The internal Kazoo version number of the application that issued the CDR | `string()` |   | `false` |  
`billing_seconds` | The number of seconds the call leg can be billed for (typically from when the call leg is answered | `integer()` |   | `false` |  
`call_direction` | Direction of the call, relative to the media switch | `string('inbound' | 'outbound')` |   | `false` |  
`call_id` | Unique identifier of the call leg | `string()` |   | `true` |  
`callee_id_name` | The indicated name of the callee | `string()` |   | `false` |  
`callee_id_number` | The indicated number of the callee | `string()` |   | `false` |  
`caller_id_name` | The indicated name of the caller | `string()` |   | `false` |  
`caller_id_number` | The indicated number of the caller | `string()` |   | `false` |  
`custom_application_vars` | Any custom-set values | `object()` |   | `false` |  
`custom_channel_vars` | Kazoo-specific key/value pairs set on the channel | `object()` |   | `false` |  
`custom_sip_headers.in` | Custom SIP Headers to be applied to calls inbound to Kazoo from the endpoint | [#/definitions/custom_sip_headers](#custom_sip_headers) |   | `false` |  
`custom_sip_headers.out` | Custom SIP Headers to be applied to calls outbound from Kazoo to the endpoint | [#/definitions/custom_sip_headers](#custom_sip_headers) |   | `false` |  
`custom_sip_headers.^[a-zA-z0-9_\-]+$` | The SIP header to add | `string()` |   | `false` |  
`custom_sip_headers` | A property list of SIP headers | `object()` |   | `false` |  
`digits_dialed` | All the DTMF tones detected on this leg of the call | `string()` |   | `false` |  
`disposition` | Who sent the SIP BYE message | `string()` |   | `false` |  
`duration_seconds` | The duration of the call leg, in seconds | `integer()` |   | `false` |  
`fax_bad_rows` |   | `string()` |   | `false` |  
`fax_ecm_used` |   | `string()` |   | `false` |  
`fax_result_code` |   | `string()` |   | `false` |  
`fax_result_text` |   | `string()` |   | `false` |  
`fax_success` |   | `string()` |   | `false` |  
`fax_total_pages` |   | `string()` |   | `false` |  
`fax_transfer_rate` |   | `string()` |   | `false` |  
`fax_transferred_pages` |   | `string()` |   | `false` |  
`from` | Built by Kazoo, depending on direction, to represent the From user | `string()` |   | `false` |  
`from_tag` | SIP From TAG | `string()` |   | `false` |  
`from_uri` | The From SIP URI | `string()` |   | `false` |  
`hangup_cause` | The reason for the call leg's termination | `string()` |   | `false` |  
`hangup_code` | The SIP hangup code, if available | `string()` |   | `false` |  
`interaction_id` | correlating ID among related call legs | `string()` |   | `false` |  
`local_sdp` | The SDP negotiated by the local agent | `string()` |   | `false` |  
`media_server` | The hostname of the media server that processed the call | `string()` |   | `false` |  
`node` | The ecallmgr which issued the CDR | `string()` |   | `false` |  
`other_leg_call_id` | If this leg was bridged, the call-id of the opposite leg | `string()` |   | `false` |  
`other_leg_caller_id_name` | Caller ID name of the bridged leg | `string()` |   | `false` |  
`other_leg_caller_id_number` | Caller ID number of the bridged leg | `string()` |   | `false` |  
`other_leg_destination_number` | Dialed number of the other leg | `string()` |   | `false` |  
`other_leg_direction` | direction of the other leg, relative to the media server | `string()` |   | `false` |  
`presence_id` | ID used in NOTIFY SIP messages | `string()` |   | `false` |  
`remote_sdp` | The SDP negotiated by the remote agent | `string()` |   | `false` |  
`request` | Built by Kazoo this is the processed request URI | `string()` |   | `false` |  
`ringing_seconds` | How many seconds the leg was ringing (pre-answer) | `integer()` |   | `false` |  
`timestamp` | UTC timestamp, in Gregorian seconds, of when the CDR was generated | `integer()` |   | `false` |  
`to` | Built by Kazoo, depending on direction, to represent the To user | `string()` |   | `false` |  
`to_tag` | SIP TO Tag | `string()` |   | `false` |  
`to_uri` | The To SIP URI | `string()` |   | `false` |  
`user_agent` | User agent header from SIP packet | `string()` |   | `false` |  

### custom_sip_headers

Custom SIP headers applied to an INVITE


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`^[a-zA-z0-9_\-]+$` | The SIP header to add | `string()` |   | `false` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/cdrs

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/cdrs/{CDR_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs/{CDR_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/cdrs/summary

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs/summary
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/cdrs/interaction

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs/interaction
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/cdrs/legs/{INTERACTION_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs/legs/{INTERACTION_ID}
```

