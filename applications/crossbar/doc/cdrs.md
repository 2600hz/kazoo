# Call Detail Records

CDRs (Call Detail Records) provide a summary view of a call leg.

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

### Prerequisites

By default requests are [paginated](basics.md#pagination). The default page size is 50 records though it can vary based on the system's configuration. It is recommended to fetch CDRs by page to not overload the system on particularly busy accounts.

Clients should use the `next_start_key` to move to the next page on subsequent requests.

### Filtering results.

KAZOO provides [filters](filters.md) to filter out rows from the results. This can be helpful if you're looking for rows related to each other (like the same `presence_id`, for instance).

### Timestamps

KAZOO timestamps, unless otherwise noted, are in [Gregorian seconds](basics.md#timestamps). Alternative timestamp formats are generally noted by the field name (e.g. `unix_timestamp`, `rfc1036`, or `iso8601`).

### Fetch request

> GET /v2/accounts/{ACCOUNT_ID}/cdrs

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs
```

#### Fetch CDRs within a time range

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs?created_from={FROM_TIMESTAMP}&created_to={TO_TIMESTAMP}
```

#### Convert timestamps to local timezone (vs UTC)

Get CDRs and update datetime field to local time zone (using seconds for timeoffset from UTC time) - Timestamps must still be in Gregorian time:

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs?created_from={FROM_TIMESTAMP}&created_to={TO_TIMESTAMP}&utc_offset={SECONDS_OFFSET}
```

#### Fetch as CSV instead of JSON

```shell
curl -v -X GET \
    -H "Accept: text/csv" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs
```

#### Fetch as CSV with defined filename

Using request headers:

```shell
curl -v -X GET \
    -H "Accept: text/csv" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "X-File-Name: {FILE_NAME}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs
```

Alternatively on the querystring:

```shell
curl -v -X GET \
    -H "Accept: text/csv" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs?file_name={FILE_NAME}
```

## Fetch a CDR's details

> GET /v2/accounts/{ACCOUNT_ID}/cdrs/{CDR_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs/{CDR_ID}
```

## Fetch interaction summary

> GET /v2/accounts/{ACCOUNT_ID}/cdrs/interaction

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs/interaction
```

## Fetch all legs related to an interaction

Crossbar cdrs was extended to provide simplified interaction call detail records. It groups all CDRs that interacted with each other to form a list of calls.

> GET /v2/accounts/{ACCOUNT_ID}/cdrs/legs/{INTERACTION_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs/legs/{INTERACTION_ID}
```

## Variations

You can select CDRs/interactions for a specific user by adding them to the URI:

> GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/cdrs

## Notes on fields

Some fields need a little more explanation to help you understand what they are telling you about the call leg.

* `call_direction` - direction of the leg, relative to the media switch
  * `inbound` - leg came into the media switch (typically the A-leg)
  * `outbound` - leg started on the media switch (typically the B-leg)
* `hangup_cause` - The reason why the call leg ended. See the [FreeSWITCH Hangup Causes](http://wiki.freeswitch.org/wiki/Hangup_causes) page for descriptions.
* `billing_seconds` - How many seconds of the call are billable (post answer, normally)
* `to` - Depends on the direction of the leg
  * outbound - Uses the presence-id or else it uses the SIP Request address
  * inbound - the SIP To header

### Kazoo-specific properties

These are properties set by Kazoo for internal purposes. These are the properties found under the `custom_channel_vars` property at the top-level of the CDR JSON object. The non-exhaustive list of properties:

* `account_id` - Account ID this leg belongs to
* `authorizing_id` - Document ID used to authorize this call leg
* `authorizing_type` - Type of document used to authorize call
  * `device` - the call leg is to/from a known Kazoo device
  * `mobile` - the call leg is to/from a known Kazoo mobile device
  * `resource` - the call leg is from a known offnet carrier
  * `outbound_fax`
* `bridge_id` - Typically the A-leg's call-id; helps with tracking transfers
* `ecallmgr_node` - Which ecallmgr node is processing the call leg
* `fetch_id` - The dialplan XML fetch ID from FreeSWITCH
* `realm` - the SIP realm of the account
* `resource_id` - Resource ID used for the leg; typically a carrier, local or global, that the call was routed to
* `username` - the SIP username of the endpoint that started the leg

### Billing-related Properties

These properties relate to how the leg was rated and billed. Some of these properties are not accessible via Crossbar, but may exist on the CDR

* `reseller_billing` - tag describing what billing was used for the reseller
* `reseller_id` - Account ID of the reseller for the account of this leg
* `account_billing` - tag describing what billing was used for the account
* `rate` - Rate of the call
* `base_cost` - How much the call costs to start (if per-minute)
* `rate_name` - Name of the rate doc used
* `surcharge` - Surcharge added to the leg
* `rate_minimum` - Minimum number of seconds to bill for
* `rate_increment` - Increment of seconds to bill for

### Fax-specific Properties

These properties may exist on a CDR for a fax request (inbound or outbound):

* `fax_transfer_rate` - Baud of the fax transfer
* `fax_bad_rows` - Number of rows that failed to transfer
* `fax_total_pages` - Number of pages in the fax (see `fax_transferred_pages` for how many made it)
* `fax_transferred_pages` - Number of pages transferred
* `fax_ecm_used` - Was ECM (error correction mode) used on the fax
* `fax_result_text` - Error String, if any, or 'OK' if successful
* `fax_result_code` - [Result code](http://wiki.freeswitch.org/wiki/Variable_fax_result_code) of the transmission
* `fax_success` - boolean for whether the fax was considered a success
* `fax_t38` - boolean for whether the fax T.38 was used

### Resource Properties

All resource properties are set by the Stepswitch application.

* `resource_type` - This property attempts to explain the reason the resource was used.  The possible values are:
    * offnet-origination - Inbound call from a resource/carrier to a Kazoo account
    * offnet-termination - Outbound call from a Kazoo account to a resource/carrier
    * onnet-origination - Inbound call from a Kazoo account to another Kazoo account
    * onnet-termination - Outbound call from a Kazoo account to another Kazoo account
* `global_resource` - This boolean is TRUE when the channel has been created by processing a resource from the offnet database, and FALSE if the resource is from an account database (local).
* `resource_id` - This is resource document id used to create the channel
