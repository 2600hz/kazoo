/*
Section: Crossbar
Title: CDRs
Language: en-US
Version: 3.21
*/

CDRs (Call Detail Records) provide a summary view of a call leg.

## CDR structure

The CDR is a list of attributes related to the processing and execution of a call leg. There are a number of properties that will always exist and a handful of fields that will conditionally exist, depending on the type of call made. There is also a sub-object, under the key 'Custom-Channel-Vars', that represent Kazoo-specific key/value pairs.

### Default Properties

These properties should appear in both the summary and detail view for all CDRs:

* call\_id - identifier for the call leg
* call\_direction - direction of the leg, relative to the media switch
  * inbound - leg came into the media switch (typically the A-leg)
  * outbound - leg started on the media switch (typically the B-leg)
* duration\_seconds - how long the call lasted
* hangup\_cause - The reason why the call leg ended. See the [FreeSWITCH Hangup Causes](http://wiki.freeswitch.org/wiki/Hangup_causes) page for descriptions.
* timestamp - UTC timestamp (in gregorian seconds) of when the CDR was generated

### Conditional Properties

The existence of these properties will vary depending on the nature of the call. This list is not meant to be exhaustive:

* billing\_seconds - How many seconds of the call are billable (post answer, normally)
* callee\_id\_name - Name of the callee
* callee\_id\_number - Number of the callee
* caller\_id\_name - Name of the caller
* caller\_id\_number - Number of the caller
* channel\_authorized - Boolean of whether the channel was authorized to continue
* disposition - Information about how the leg ended
  * SUCCESS - The leg terminated, while bridged, normally (successfully)
  * ANSWER - The leg terminated normally without being bridged
  * ORIGINATOR\_CANCEL - Caller hung up
  * DELAYED NEGOTIATION -
* from - Depends on the direction of the leg
  * outbound - finds the value from the caller id number and realm if the SIP From URI is missing
  * inbound - Checks against the presence ID or From-URI if available
* from\_uri - the SIP From header
* hangup\_code - the SIP code of the hangup, if available
* inception - From where was the call started
  * on-net - Call is from a known account's device
  * off-net - Call is from outside of the cluster
* local\_sdp - SDP information for the local (media server) side
* media\_server - The handling media server hostname
* other\_leg\_call\_id - if the call was bridged, the call-id of the other leg
* other\_leg\_caller\_id\_name - Caller ID name of the bridged leg
* other\_leg\_caller\_id\_number - Caller ID number of the bridged leg
* other\_leg\_destination\_number - The dialed number of the other leg
* other\_leg\_direction - direction, relative to the media server, of the other leg (should be the opposite of call\_direction).
* presence\_id - Presence ID used to send NOTIFYs if needed
* remote\_sdp - SDP information for the remote side (the endpoint's side)
* request - SIP Request URI
* ringing\_seconds - how many seconds the endpoint was ringing before answering
* to - Depends on the direction of the leg
  * outbound - Uses the presence-id or else it uses the SIP Request address
  * inbound - the SIP To header
* to\_uri - SIP To header
* user\_agent - User Agent of the endpoint
* bridge\_id - Bridge ID

### Kazoo-specific Properties

These are properties set by Kazoo for internal purposes. These are the properties found under the _custom\_channel\_vars_ property at the top-level of the CDR JSON object. The non-exhaustive list of properties:

* account\_id - Account ID this leg belongs to
* authorizing\_id - Document ID used to authorize this call leg
* authorizing\_type - Type of ducument used to authorize call
  * device - the call leg is to/from a known Kazoo device
  * resource - the call leg is from a known offnet carrier
  * outbound\_fax
* bridge\_id - Typically the A-leg's call-id; helps with tracking transfers
* ecallmgr\_node - Which ecallmgr node is processing the call leg
* fetch\_id - The dialplan XML fetch ID from FreeSWITCH
* realm - the SIP realm of the account
* resource\_id - Resource ID used for the leg; typically a carrier, local or global, that the call was routed to
* username - the SIP username of the endpoint that started the leg

### Billing-related Properties

These properties relate to how the leg was rated and billed. Some of these properties are not accessible via Crossbar, but may exist on the CDR

* reseller\_billing - tag describing what billing was used for the reseller
* reseller\_id - Account ID of the reseller for the account of this leg
* account\_billing - tag describing what billing was used for the account
* rate - Rate of the call
* base\_cost - How much the call costs to start (if per-minute)
* rate_name - Name of the rate doc used
* surcharge - Surcharge added to the leg
* rate\_minimum - Minimum number of seconds to bill for
* rate_increment - Increment of seconds to bill for

### Fax-specific Properties

These properties may exist on a CDR for a fax request (inbound or outbound):

* fax\_transfer\_rate - Baud of the fax transfer
* fax\_bad\_rows - Number of rows that failed to transfer
* fax\_total\_pages - Number of pages in the fax (see fax\_transferred\_pages for how many made it)
* fax\_transferred\_pages - Number of pages transferred
* fax\_ecm\_used - Was ECM (error correction mode) used on the fax
* fax\_result\_text - Error String, if any, or 'OK' if successful
* fax\_result\_code - [Result code](http://wiki.freeswitch.org/wiki/Variable_fax_result_code) of the transmission
* fax\_success - boolean for whether the fax was considered a success

## Crossbar

Using Crossbar to modify metaflows is very simple. There are only three GETs

* GET /v1/accounts/{account\_id}/cdrs - Gets the current CDRs for the account
* GET /v1/accounts/{account\_id}/cdrs/{cdr\_id} - Gets details of the CDR
* GET /v1/accounts/{account\_id}/users/{user\_id}/cdrs - Gets the current CDRs for the user

### Sample Requests

#### _GET_ - Fetch account CDRs:

    curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs

    {"auth_token": "{AUTH_TOKEN}",
     "data": [
         {"authorizing_id": "9c3a10ee4d311feec43333fdf6d715b7",
          "billing_seconds": "7",
          "bridge_id": "{BRIDGE_ID}",
          "call_id": "{CALL_ID},
          "call_priority": "",
          "call_type": "",
          "callee_id_name": "Internal Device 2",
          "callee_id_number": "0000",
          "caller_id_name": "Internal Device 0",
          "caller_id_number": "0000",
          "calling_from": "0000",
          "cost": "0",
          "datetime": "2015-06-26 12:28:35",
          "dialed_number": "1002",
          "direction": "inbound",
          "duration_seconds": "8",
          "from": "test_user@{ACCOUNT_REALM}",
          "hangup_cause": "NORMAL_CLEARING",
          "id": "YYYYMM-{CALL_ID}",
          "iso_8601": "\"2015-06-26\"",
          "other_leg_call_id": "{OTHER_LEG_CALL_ID}",
          "owner_id": "",
          "rate": "0.0",
          "rate_name": "",
          "recording_url": "",
          "request": "1002@{ACCOUNT_REALM}",
          "reseller_call_type": "",
          "reseller_cost": "0",
          "rfc_1036": "\"Fri, 26 Jun 2015 12:28:35 GMT\"",
          "timestamp": "63602540915",
          "to": "1002@10.0.1.141",
          "unix_timestamp": "1435321715"
         }
         ,{"authorizing_id": "0306f08da52079c1223e4fa84c415a68",
           "billing_seconds": "3",
           "bridge_id": "{BRIDGE_ID}",
           "call_id": "{CALL_ID}",
           "call_priority": "",
           "call_type": "",
           "callee_id_name": "Internal Device 2",
           "callee_id_number": "0000",
           "caller_id_name": "",
           "caller_id_number": "0000000000",
           "calling_from": "0000",
           "cost": "0",
           "datetime": "2015-06-26 12:28:39",
           "dialed_number": "device_ZYcHDwktNB",
           "direction": "outbound",
           "duration_seconds": "4",
           "from": "test_user@{ACCOUNT_REALM}",
           "hangup_cause": "NORMAL_CLEARING",
           "id": "YYYYMM-{CALL_ID}",
           "iso_8601": "\"2015-06-26\"",
           "other_leg_call_id": "{OTHER_LEG_CALL_ID}"
           "owner_id": "",
           "rate": "0.0",
           "rate_name": "",
           "recording_url": "",
           "request": "device_ZYcHDwktNB@{ACCOUNT_REALM}",
           "reseller_call_type": "",
           "reseller_cost": "0",
           "rfc_1036": "\"Fri, 26 Jun 2015 12:28:39 GMT\"",
           "timestamp": "63602540919",
           "to": "device_ZYcHDwktNB@{ACCOUNT_REALM}",
           "unix_timestamp": "1435321719"
         }
         ...
     ],
     "next_start_key": "63602563961",
     "page_size": 50,
     "request_id": "{REQUEST_ID}",
     "start_key": "63602603243",
     "status": "success"
    }

#### _GET_ - Fetch user CDRs:

This will fetch only the CDRs associated with the user.

    curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/cdrs

#### _GET_ - Grouped CDRs

It is possible to group CDRs to see which legs go together. Add the query string parameter `grouped=true` to have Crossbar group the legs together.

The resulting JSON will be of the form:

    {"A_LEG_CALL_ID":[{A_LEG_CDR}, {B_LEG_CDR}, ...]
     ,"A_LEG_CALL_ID":[{A_LEG_CDR}]
     ,...
    }

Where `{A_LEG_CALL_ID}` is the leg identified as the A leg, and the list of CDR objects is ordered by leg. The `{A_LEG_CDR}` object will also have a flag `is_a_leg=true` added.

**Important**: this is only available as a JSON response; CSV is not supported!

The full request:

    curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs?grouped=true

    {"auth_token": "543b90ddea74af1b214d84bf71443cc4",
     "data": [
        {"69767aeb-9c69-45d3-bf3f-792b7175a12c": [
            {"authorizing_id": "e7d89a1d901d3b10a0b06675f5ef49c1",
             "billing_seconds": "1",
             "bridge_id": "testCfSubstituteTrue-bCgAm7aG",
             "call_id": "{CALL_ID}",
             "call_priority": "",
             "call_type": "",
             "callee_id_name": "Internal Device 2",
             "callee_id_number": "0000",
             "caller_id_name": "External Device 0",
             "caller_id_number": "000000000",
             "calling_from": "000000000",
             "cost": "0",
             "datetime": "2015-06-26 12:26:47",
             "dialed_number": "1003",
             "direction": "inbound",
             "duration_seconds": "8",
             "from": "device_SsrwT1dxAd@{ACCOUNT_REALM}",
             "hangup_cause": "NORMAL_CLEARING",
             "id": "201506-{CALL_ID}",
             "is_a_leg": true,
             "iso_8601": "\"2015-06-26\"",
             "other_leg_call_id": "{OTHER_LEG_CALL_ID}",
             "owner_id": "",
             "rate": "0.0",
             "rate_name": "",
             "recording_url": "",
             "request": "1003@nodomain.com",
             "reseller_call_type": "",
             "reseller_cost": "0",
             "rfc_1036": "\"Fri, 26 Jun 2015 12:26:47 GMT\"",
             "timestamp": "63602540807",
             "to": "1003@nodomain.com",
             "unix_timestamp": "1435321607"
         }
         ,{"authorizing_id": "0306f08da52079c1223e4fa84c415a68",
           "billing_seconds": "1",
           "bridge_id": "{BRIDGE_ID}",
           "call_id": "{CALL_ID}",
           "call_priority": "",
           "call_type": "",
           "callee_id_name": "Internal Device 2",
           "callee_id_number": "0000",
           "caller_id_name": "External Device 0",
           "caller_id_number": "000000000",
           "calling_from": "000000000",
           "cost": "0",
           "datetime": "2015-06-26 12:26:48",
           "dialed_number": "device_ZYcHDwktNB",
           "direction": "outbound",
           "duration_seconds": "7",
           "from": "1003-b",
           "hangup_cause": "NORMAL_CLEARING",
           "id": "201506-{CALL_ID}",
           "iso_8601": "\"2015-06-26\"",
           "other_leg_call_id": "{OTHER_LEG_CALL_ID}",
           "owner_id": "",
           "rate": "0.0",
           "rate_name": "",
           "recording_url": "",
           "request": "device_ZYcHDwktNB@{ACCOUNT_REALM}",
           "reseller_call_type": "",
           "reseller_cost": "0",
           "rfc_1036": "\"Fri, 26 Jun 2015 12:26:48 GMT\"",
           "timestamp": "63602540808",
           "to": "device_ZYcHDwktNB@{ACCOUNT_REALM}",
           "unix_timestamp": "1435321608"
         }
        ]
       }
      ],
     "next_start_key": "63602563961",
     "page_size": 50,
     "request_id": "{REQUEST_ID}",
     "start_key": "63602603916",
     "status": "success"
    }

####  _GET_ - Fetch a CDR:

    curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs/{CDR_ID}

#### _GET_ - Fetch a time-range of CDRs

    curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs?created_from={FROM_TIMESTAMP}&created_to={TO_TIMESTAMP}

&tip All timestamps will be in Gregorian seconds (not Unix epoch).

#### _GET_ - Fetch account CDRs in csv format:

    curl -v -X GET \
    -H "Accept: text/csv" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs
