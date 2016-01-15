

CDRs (Call Detail Records) provide a summary view of a call leg.

#### CDR structure

The CDR is a list of attributes related to the processing and execution of a call leg. There are a number of properties that will always exist and a handful of fields that will conditionally exist, depending on the type of call made. There is also a sub-object, under the key 'Custom-Channel-Vars', that represent Kazoo-specific key/value pairs.

##### Default Properties

These properties should appear in both the summary and detail view for all CDRs:

* call\_id - identifier for the call leg
* call\_direction - direction of the leg, relative to the media switch
  * inbound - leg came into the media switch (typically the A-leg)
  * outbound - leg started on the media switch (typically the B-leg)
* duration\_seconds - how long the call lasted
* hangup\_cause - The reason why the call leg ended. See the [FreeSWITCH Hangup Causes](http://wiki.freeswitch.org/wiki/Hangup_causes) page for descriptions.
* timestamp - UTC timestamp (in gregorian seconds) of when the CDR was generated

##### Conditional Properties

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

##### Kazoo-specific Properties

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

##### Billing-related Properties

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

##### Fax-specific Properties

These properties may exist on a CDR for a fax request (inbound or outbound):

* fax\_transfer\_rate - Baud of the fax transfer
* fax\_bad\_rows - Number of rows that failed to transfer
* fax\_total\_pages - Number of pages in the fax (see fax\_transferred\_pages for how many made it)
* fax\_transferred\_pages - Number of pages transferred
* fax\_ecm\_used - Was ECM (error correction mode) used on the fax
* fax\_result\_text - Error String, if any, or 'OK' if successful
* fax\_result\_code - [Result code](http://wiki.freeswitch.org/wiki/Variable_fax_result_code) of the transmission
* fax\_success - boolean for whether the fax was considered a success
* fax\_t38 - boolean for whether the fax T.38 was used

#### Crossbar

Using Crossbar to query cdrs is very simple. There are 3 different GETs

* GET /v1/accounts/{account\_id}/cdrs - Gets the current CDRs for the account
* GET /v1/accounts/{account\_id}/cdrs/{cdr\_id} - Gets details of the CDR
* GET /v1/accounts/{account\_id}/users/{user\_id}/cdrs - Gets the current CDRs for the user

##### Sample Requests

###### _GET_ - Fetch account CDRs:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/cdrs

###### _GET_ - Fetch user CDRs:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/users/{USER_ID}/cdrs

######  _GET_ - Fetch a CDR:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/cdrs/{CDR_ID}

###### _GET_ - Fetch a time-range of CDRs

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/cdrs?created_from={FROM_TIMESTAMP}&created_to={TO_TIMESTAMP}

&tip All timestamps will be in Gregorian seconds (not Unix epoch).

###### _GET_ - Fetch account CDRs in csv format:

    curl -v -X GET -H "Accept: text/csv" -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/cdrs

### Interaction CDRs

Crossbar cdrs was extended to provide simplified interaction call detail records. It groups all CDRs that interacted with eachouther to form a list of calls.

* GET /v1/accounts/{account\_id}/cdrs/interaction - Gets the current interaction CDRs for the account
* GET /v1/accounts/{account\_id}/cdrs/interaction/{cdr\_id} - Gets details of the interaction CDR, usually first leg
* GET /v1/accounts/{account\_id}/cdrs/legs/{cdr\_id} - Gets CDRs for the legs of call interaction
* GET /v1/accounts/{account\_id}/users/{user\_id}/cdrs/interaction - Gets the current interaction CDRs for the user
