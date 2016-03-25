

SBC-level per-account and per-device access lists allow setting individual IP-based access filtering rules which
significantly increases security for users working on-premise.

Rules can be applied at account level or at individual device level

#### API

"access\_lists" API works both at `accounts` and `devices` documents level.

Sections:

* access_lists - root element
    * order - order of rules: can be "allow,deny" or "deny,allow", just like in Apache configuration file
    * cidrs - array containing IPv4 subnet addresses in CIDR notation that should be allowed or denied (cidr array looks much like one in ecallmgr configuration document)
    * user\_agent - regex for user_agent field specified in SIP packet. Useful for protecting hardware phone accounts from various brute-force attacks

##### Account access lists URI

`/v1/accounts/{ACCOUNT_ID}/access_lists`

This URI is used to manipulate the access_lists for the entire account.

###### _GET_ - Fetch account-level access lists:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/access_lists

###### _POST_ - Update account-level access lists:

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v1/accounts/{ACCOUNT_ID}/access_lists -d '{"data": {"order": "allow,deny","cidrs": ["127.0.0.3/32"]}}'

###### _DELETE_ - Remove account-level access lists:

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/access_lists

##### Device access lists URI

`/v1/accounts/{ACCOUNT_ID}/{THINGS}/{THING_ID}/access_lists`

Here, `{THINGS}` would be "accounts" or "devices" and `{THING_ID}` would be a device or account id. Let's look at adding access lists to a device.

###### _GET_ - Fetch device-level access lists:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/access_lists

###### _POST_ - Update device-level access lists:

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v1/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/access_lists -d '{"data": {"order": "allow,deny","cidrs": ["127.0.0.3/32"]}}'

###### _DELETE_ - Remove device-level access lists:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/access_lists
