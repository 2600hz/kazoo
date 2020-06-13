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
`channel_call_state` | Media Server channel call state | `string()` |   | `false` |  
`channel_created_time` | Media Server channel creation time | `integer()` |   | `false` |  
`channel_name` | Media Server channel name | `string()` |   | `false` |  
`channel_state` | Media Server channel state | `string()` |   | `false` |  
`custom_application_vars` | Any custom-set values | `object()` |   | `false` |  
`custom_channel_vars` | Kazoo-specific key/value pairs set on the channel | `object()` |   | `false` |  
`custom_sip_headers.in` | Custom SIP Headers to be applied to calls inbound to Kazoo from the endpoint | [#/definitions/custom_sip_headers](#custom_sip_headers) |   | `false` |  
`custom_sip_headers.out` | Custom SIP Headers to be applied to calls outbound from Kazoo to the endpoint | [#/definitions/custom_sip_headers](#custom_sip_headers) |   | `false` |  
`custom_sip_headers.^[a-zA-z0-9_\-]+$` | The SIP header to add | `string()` |   | `false` |  
`custom_sip_headers` | A property list of SIP headers | `object()` |   | `false` |  
`digits_dialed` | All the DTMF tones detected on this leg of the call | `string()` |   | `false` |  
`disposition` | Who sent the SIP BYE message | `string()` |   | `false` |  
`duration_seconds` | The duration of the call leg, in seconds | `integer()` |   | `false` |  
`event_category` | KAZOO specific key (Event-Category) included on each payload it publishes | `string()` |   | `false` |  
`event_name` | KAZOO specific key (Event-Name) included on each payload it publishes | `string()` |   | `false` |  
`fax_bad_rows` | Number of rows that failed to transfer | `string()` |   | `false` |  
`fax_ecm_used` | Was ECM (error correction mode) used on the fax | `string()` |   | `false` |  
`fax_result_code` | Media Server's result code of the transmission | `string()` |   | `false` |  
`fax_result_text` | Error String, if any, or 'OK' if successful | `string()` |   | `false` |  
`fax_success` | Whether the fax was considered a success or not | `string()` |   | `false` |  
`fax_total_pages` | Number of pages in the fax | `string()` |   | `false` |  
`fax_transfer_rate` | Baud of the fax transfer | `string()` |   | `false` |  
`fax_transferred_pages` | Number of pages transferred | `string()` |   | `false` |  
`from` | Built by Kazoo, depending on direction, to represent the From user | `string()` |   | `false` |  
`from_tag` | SIP From TAG | `string()` |   | `false` |  
`from_uri` | The From SIP URI | `string()` |   | `false` |  
`hangup_cause` | The reason for the call leg's termination | `string()` |   | `false` |  
`hangup_code` | The SIP hangup code, if available | `string()` |   | `false` |  
`interaction_id` | correlating ID among related call legs | `string()` |   | `false` |  
`interaction_key` | Unique portion of the interaction ID | `string()` |   | `false` |  
`interaction_time` | Timestamp of the creation of the interaction ID | `integer()` |   | `false` |  
`local_sdp` | The SDP negotiated by the local agent | `string()` |   | `false` |  
`media_server` | The hostname of the media server that processed the call | `string()` |   | `false` |  
`msg_id` | KAZOO specific key (Msg-Id) assigned to each payload it publishes | `string()` |   | `false` |  
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
`switch_hostname` | Media Server hostname (as reported by the switch) | `string()` |   | `false` |  
`switch_nodename` | Media Server node name (as known in ecallmgr) | `string()` |   | `false` |  
`switch_uri` | Media Server URI | `string()` |   | `false` |  
`switch_url` | Media Server URL | `string()` |   | `false` |  
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

