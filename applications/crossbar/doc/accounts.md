### Accounts

#### About Accounts

Accounts are the container for most things in Kazoo. They typically represent an office, business, family, etc. Kazoo arranges accounts into a tree structure, where parent accounts can access their sub accounts but not their ancestor accounts.

#### About the Account Tree

Since accounts can be the child of 0 or more parent accounts, it is necessary to track each account's lineage. This is tracked in the account document (_id = ID of the account) in the `pvt_tree` array. The order of the list is from most-ancestral to parent.

So given `"pvt_tree":["1", "2", "3"]`, it can be determined that "3" is the parent account, "2" the grand-parent, and "1" is the great-grandparent. `"pvt_tree":[]` indicates the master (or Highlander) account; there should only be one!

#### Schema

Accounts represent tenants or customers on the system. Each account represents an individual dataset or sandbox that only one tenant can access. The data set is architecturally independent from other tenants.

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`call_restriction` | Account level call restrictions for each available number classification | `object` | `{}` | `false`
`call_waiting` |   | [#/definitions/call_waiting](#call_waiting) |   | `false`
`caller_id` | The account default caller ID parameters | `object` | `{}` | `false`
`dial_plan` | A list of default rules used to modify dialed numbers | `object` | `{}` | `false`
`do_not_disturb` |   | `object` |   | `false`
`do_not_disturb.enabled` | The default value for do-not-disturb | `boolean` |   | `false`
`enabled` | Determines if the account is currently enabled | `boolean` | `true` | `false`
`language` | The language for this account | `string` | `en-us` | `false`
`metaflows` |   | [#/definitions/metaflows](#metaflows) |   | `false`
`music_on_hold` | The default music on hold parameters | `object` | `{}` | `false`
`music_on_hold.media_id` | The ID of a media object that should be used as the default music on hold | `string(0..128)` |   | `false`
`name` | A friendly name for the account | `string(1..128)` |   | `true`
`org` | Full legal name of the organization | `string` |   | `false`
`preflow` | Each property provides functionality that can be applied to calls using the callflow application | `object` | `{}` | `false`
`preflow.always` | The ID of a callflow to always execute prior to processing the callflow with numbers/patterns matching the request | `string` |   | `false`
`realm` | The realm of the account, ie: 'account1.2600hz.com' | `string(4..253)` |   | `false`
`ringtones` | Ringtone Parameters | `object` | `{}` | `false`
`ringtones.external` | The alert info SIP header added when the call is from internal sources | `string(0..256)` |   | `false`
`ringtones.internal` | The alert info SIP header added when the call is from external sources | `string(0..256)` |   | `false`
`timezone` | The default timezone | `string(5..32)` |   | `false`
`voicemail` |   | `object` |   | `false`
`voicemail.notify` |   | `object` |   | `false`
`voicemail.notify.callback` |   | [#/definitions/notify.callback](#notifycallback) |   | `false`


##### call_waiting

Parameters for server-side call waiting

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`enabled` | Determines if server side call waiting is enabled/disabled | `boolean` |   | `false`

##### caller_id

Defines caller ID settings based on the type of call being made

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`emergency` | The caller ID used when a resource is flagged as 'emergency' | `object` |   | `false`
`emergency.name` | The caller id name for the object type | `string(0..35)` |   | `false`
`emergency.number` | The caller id name for the object type | `string(0..35)` |   | `false`
`external` | The default caller ID used when dialing external numbers | `object` |   | `false`
`external.name` | The caller id name for the object type | `string(0..35)` |   | `false`
`external.number` | The caller id name for the object type | `string(0..35)` |   | `false`
`internal` | The default caller ID used when dialing internal extensions | `object` |   | `false`
`internal.name` | The caller id name for the object type | `string(0..35)` |   | `false`
`internal.number` | The caller id name for the object type | `string(0..35)` |   | `false`

##### dialplans

Permit local dialing by converting the dialed number to a routable form

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`system` | List of system dial plans | `array()` |   | `false`

##### metaflow

A metaflow node defines a module to execute, data to provide to that module, and one or more children to branch to

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` | Children metaflows | `object` |   | `false`
`children./.+/` |   | [#/definitions/metaflow](#metaflow) |   | `false`
`data` | The data/arguments of the metaflow module | `object` |   | `false`
`module` | The name of the metaflow module to execute at this node | `string(1..64)` |   | `true`

##### metaflows

Actions applied to a call outside of the normal callflow, initiated by the caller(s)

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`binding_digit` | What DTMF will trigger the collection and analysis of the subsequent DTMF sequence | `string('1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '*', '#')` | `*` | `false`
`digit_timeout` | How long to wait between DTMF presses before processing the collected sequence (milliseconds) | `integer` |   | `false`
`listen_on` | Which leg(s) of the call to listen for DTMF | `string('both', 'self', 'peer')` |   | `false`
`numbers` | A list of static numbers with their flows | `object` |   | `false`
`numbers./^[0-9]+$/` |   | [#/definitions/metaflow](#metaflow) |   | `false`
`patterns` | A list of patterns with their flows | `object` |   | `false`
`patterns./.+/` |   | [#/definitions/metaflow](#metaflow) |   | `false`

##### notify.callback

Schema for a callback options

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`attempts` | How many attempts without answer will system do | `integer` |   | `false`
`disabled` | Determines if the system will call to callback number | `boolean` |   | `false`
`interval_s` | How long will system wait between call back notification attempts | `integer` |   | `false`
`number` | Number for callback notifications about new messages | `string` |   | `false`
`schedule` | Schedules interval between callbacks | `array(integer)` |   | `false`
`timeout_s` | How long will system wait for answer to callback | `integer` |   | `false`



##### call_waiting

Parameters for server-side call waiting

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`enabled` | Determines if server side call waiting is enabled/disabled | `boolean` |   | `false`

##### caller_id

Defines caller ID settings based on the type of call being made

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`emergency` | The caller ID used when a resource is flagged as 'emergency' | `object` |   | `false`
`emergency.name` | The caller id name for the object type | `string(0..35)` |   | `false`
`emergency.number` | The caller id name for the object type | `string(0..35)` |   | `false`
`external` | The default caller ID used when dialing external numbers | `object` |   | `false`
`external.name` | The caller id name for the object type | `string(0..35)` |   | `false`
`external.number` | The caller id name for the object type | `string(0..35)` |   | `false`
`internal` | The default caller ID used when dialing internal extensions | `object` |   | `false`
`internal.name` | The caller id name for the object type | `string(0..35)` |   | `false`
`internal.number` | The caller id name for the object type | `string(0..35)` |   | `false`

##### dialplans

Permit local dialing by converting the dialed number to a routable form

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`system` | List of system dial plans | `array()` |   | `false`

##### metaflow

A metaflow node defines a module to execute, data to provide to that module, and one or more children to branch to

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`children` | Children metaflows | `object` |   | `false`
`children./.+/` |   | [#/definitions/metaflow](#metaflow) |   | `false`
`data` | The data/arguments of the metaflow module | `object` |   | `false`
`module` | The name of the metaflow module to execute at this node | `string(1..64)` |   | `true`

##### metaflows

Actions applied to a call outside of the normal callflow, initiated by the caller(s)

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`binding_digit` | What DTMF will trigger the collection and analysis of the subsequent DTMF sequence | `string('1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '*', '#')` | `*` | `false`
`digit_timeout` | How long to wait between DTMF presses before processing the collected sequence (milliseconds) | `integer` |   | `false`
`listen_on` | Which leg(s) of the call to listen for DTMF | `string('both', 'self', 'peer')` |   | `false`
`numbers` | A list of static numbers with their flows | `object` |   | `false`
`numbers./^[0-9]+$/` |   | [#/definitions/metaflow](#metaflow) |   | `false`
`patterns` | A list of patterns with their flows | `object` |   | `false`
`patterns./.+/` |   | [#/definitions/metaflow](#metaflow) |   | `false`

##### notify.callback

Schema for a callback options

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`attempts` | How many attempts without answer will system do | `integer` |   | `false`
`disabled` | Determines if the system will call to callback number | `boolean` |   | `false`
`interval_s` | How long will system wait between call back notification attempts | `integer` |   | `false`
`number` | Number for callback notifications about new messages | `string` |   | `false`
`schedule` | Schedules interval between callbacks | `array(integer)` |   | `false`
`timeout_s` | How long will system wait for answer to callback | `integer` |   | `false`

#### Aliases for Account API Requests

Account IDs are 32-character identifiers (as are most identifiers in Kazoo) which can increase the size of the URL. Once you've authenticated, your user's account identifier is encoded in the auth token you received. If you plan to make requests against that account, you can use `/v2/account/...` instead of `/v2/accounts/{ACCOUNT_ID}/...` for your requests. So fetching a list of devices from your account could be `GET /v2/account/devices`, for instance.

#### Create a new child account

Puts the created account under the account of the owner of the `{AUTH_TOKEN}`. This is a shortcut for `PUT /v2/accounts/{AUTH_ACCOUNT_ID}`

> PUT /v2/accounts

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"name":"child account"}}' \
    http://{SERVER}:8000/v2/accounts
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "billing_mode": "manual",
        "call_restriction": {},
        "caller_id": {},
        "created": 63621662701,
        "dial_plan": {},
        "enabled": true,
        "id": "{ACCOUNT_ID}",
        "is_reseller": false,
        "language": "en-us",
        "music_on_hold": {},
        "name": "child account",
        "preflow": {},
        "realm": "aeac33.sip.2600hz.com",
        "reseller_id": "undefined",
        "ringtones": {},
        "superduper_admin": false,
        "timezone": "America/Los_Angeles",
        "wnm_allow_additions": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Remove an account

> DELETE /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "billing_mode": "manual",
        "call_restriction": {},
        "caller_id": {},
        "created": 63621662701,
        "dial_plan": {},
        "enabled": true,
        "id": "{ACCOUNT_ID}",
        "is_reseller": false,
        "language": "en-us",
        "music_on_hold": {},
        "name": "child account",
        "preflow": {},
        "realm": "aeac33.sip.2600hz.com",
        "reseller_id": "undefined",
        "ringtones": {},
        "superduper_admin": false,
        "timezone": "America/Los_Angeles",
        "wnm_allow_additions": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Fetch the account doc

> GET /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "billing_mode": "manual",
        "call_restriction": {},
        "caller_id": {},
        "created": 63621662701,
        "dial_plan": {},
        "enabled": true,
        "id": "{ACCOUNT_ID}",
        "is_reseller": false,
        "language": "en-us",
        "music_on_hold": {},
        "name": "child account",
        "preflow": {},
        "realm": "aeac33.sip.2600hz.com",
        "reseller_id": "undefined",
        "ringtones": {},
        "superduper_admin": false,
        "timezone": "America/Los_Angeles",
        "wnm_allow_additions": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Patch the account doc

> PATCH /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"some_key":"some_value"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "billing_mode": "manual",
        "call_restriction": {},
        "caller_id": {},
        "created": 63621662701,
        "dial_plan": {},
        "enabled": true,
        "id": "{ACCOUNT_ID}",
        "is_reseller": false,
        "language": "en-us",
        "music_on_hold": {},
        "name": "child account",
        "preflow": {},
        "realm": "aeac33.sip.2600hz.com",
        "reseller_id": "undefined",
        "ringtones": {},
        "some_key":"some_value",
        "superduper_admin": false,
        "timezone": "America/Los_Angeles",
        "wnm_allow_additions": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Change the account doc

> POST /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data": {"billing_mode": "manual","call_restriction": {},"caller_id": {},"created": 63621662701,"dial_plan": {},"enabled": true,"is_reseller": false,"language": "en-us","music_on_hold": {},"name": "child account","preflow": {},"realm": "aeac33.sip.2600hz.com","reseller_id": "undefined","ringtones": {},"some_key":"some_value","superduper_admin": false,"timezone": "America/Los_Angeles","wnm_allow_additions": false}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "billing_mode": "manual",
        "call_restriction": {},
        "caller_id": {},
        "created": 63621662701,
        "dial_plan": {},
        "enabled": true,
        "id": "{ACCOUNT_ID}",
        "is_reseller": false,
        "language": "en-us",
        "music_on_hold": {},
        "name": "child account",
        "preflow": {},
        "realm": "aeac33.sip.2600hz.com",
        "reseller_id": "undefined",
        "ringtones": {},
        "some_key":"some_value",
        "superduper_admin": false,
        "timezone": "America/Los_Angeles",
        "wnm_allow_additions": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Create a new child account

Puts the created account under `{ACCOUNT_ID}`

> PUT /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"name":"child account"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "billing_mode": "manual",
        "call_restriction": {},
        "caller_id": {},
        "created": 63621662701,
        "dial_plan": {},
        "enabled": true,
        "id": "{CHILD_ACCOUNT_ID}",
        "is_reseller": false,
        "language": "en-us",
        "music_on_hold": {},
        "name": "child account",
        "preflow": {},
        "realm": "aeac33.sip.2600hz.com",
        "reseller_id": "undefined",
        "ringtones": {},
        "superduper_admin": false,
        "timezone": "America/Los_Angeles",
        "wnm_allow_additions": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Fetch the parent account IDs

> GET /v2/accounts/{ACCOUNT_ID}/parents

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/parents
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "id": "{PARENT_ACCOUNT_ID}",
            "name": "{PARENT_ACCOUNT_NAME}"
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Fetch an account's ancestor tree

> GET /v2/accounts/{ACCOUNT_ID}/tree

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tree
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "id": "{PARENT_ACCOUNT_ID}",
            "name": "{PARENT_ACCOUNT_NAME}"
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Fetch the account's API key

The API key is used by the `api_auth` API to obtain an `auth_token`. This is intended for use by applications talking to kazoo and provides a mechanism for authentication that does not require storing a username and password in the application. The API key can be obtained via the accounts API's endpoint `api_key`.

> GET /v2/accounts/{ACCOUNT_ID}/api_key

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
     http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/api_key
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "api_key": "{API_KEY}"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Fetch sibling accounts

By default a user account under an admin/reseller account can view all the other accounts under that reseller. If you would like current account only will be able to query its child accounts' sibling and not other accounts then set `allow_sibling_listing` in `system_config/crossbar.accounts` to `false`. Admin account can unrestrictedly list siblings.

> GET /v2/accounts/{ACCOUNT_ID}/siblings

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/siblings
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "descendants_count": 1,
            "id": "{ACCOUNT_ID}",
            "name": "{ACCOUNT_NAME}",
            "realm": "{ACCOUNT_REALM}"
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "start_key": "",
    "status": "success"
}
```

#### Fetch all descendants of an account

This will include children, grandchildren, etc

> GET /v2/accounts/{ACCOUNT_ID}/descendants

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/descendants
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "id": "{CHILD_ACCOUNT}",
            "name": "{CHILD_NAME}",
            "realm": "{CHILD_REALM}",
            "tree": [
                "{ACCOUNT_ID}"
            ]
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "start_key": "",
    "status": "success"
}
```

#### Fetch immediate children of an account

> GET /v2/accounts/{ACCOUNT_ID}/children

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/children
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "id": "{CHILD_ACCOUNT}",
            "name": "{CHILD_NAME}",
            "realm": "{CHILD_REALM}",
            "tree": [
                "{ACCOUNT_ID}"
            ]
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "start_key": "",
    "status": "success"
}
```

#### Demote a reseller

Requires superduper admin auth token

> DELETE /v2/accounts/{ACCOUNT_ID}/reseller

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/reseller
```

#### Promote a reseller

Requires superduper admin auth token

> PUT /v2/accounts/{ACCOUNT_ID}/reseller

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/reseller
```


#### Move an account

An account can only be moved by a "superduper_admin" or  if enabled by anyone above the desired account.

You can enable that feature by editing the document `crossbar.accounts` in your `system_config` database and set the value to `tree`.

Key | Value | Description
--- | ----- | -----------
`allow_move` | enum("tree", "superduper_admin") | Who can move a sub-account

> POST /v2/accounts/{ACCOUNT_ID}/move

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"to": "{ACCOUNT_ID_DESTINATION}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/move
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "billing_mode": "manual",
        "call_restriction": {},
        "caller_id": {},
        "created": 63621662701,
        "dial_plan": {},
        "enabled": true,
        "id": "{ACCOUNT_ID}",
        "is_reseller": false,
        "language": "en-us",
        "music_on_hold": {},
        "name": "child account",
        "preflow": {},
        "realm": "aeac33.sip.2600hz.com",
        "reseller_id": "undefined",
        "ringtones": {},
        "superduper_admin": false,
        "timezone": "America/Los_Angeles",
        "wnm_allow_additions": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```
