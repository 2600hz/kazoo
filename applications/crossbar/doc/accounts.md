### Accounts

#### About Accounts

Accounts are the container for most things in Kazoo. They typically represent an office, business, family, etc. Kazoo arranges accounts into a tree structure, where parent accounts can access their sub accounts but not their ancestor accounts.

#### About the Account Tree

Since accounts can be the child of 0 or more parent accounts, it is necessary to track each account's lineage. This is tracked in the account document (_id = ID of the account) in the `pvt_tree` array. The order of the list is from most-ancestral to parent.

So given `"pvt_tree":["1", "2", "3"]`, it can be determined that "3" is the parent account, "2" the grand-parent, and "1" is the great-grandparent. `"pvt_tree":[]` indicates the master (or Highlander) account; there should only be one!

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`call_restriction` | Account level call restrictions for each available number classification | `object` | `{}` | `false`
`call_waiting` |   | `#/definitions/call_waiting` |   | `false`
`caller_id` | The account default caller ID parameters | `object` | `{}` | `false`
`dial_plan` | A list of default rules used to modify dialed numbers | `object` | `{}` | `false`
`do_not_disturb` |   | `object` |   | `false`
`do_not_disturb.enabled` | The default value for do-not-disturb | `boolean` |   | `false`
`enabled` | Determines if the account is currently enabled | `boolean` | `true` | `false`
`language` | The language for this account | `string` | `en-us` | `false`
`metaflows` |   | `#/definitions/metaflows` |   | `false`
`music_on_hold` | The default music on hold parameters | `object` | `{}` | `false`
`music_on_hold.media_id` | The ID of a media object that should be used as the default music on hold | `string(0..128)` |   | `false`
`name` | A friendly name for the account | `string(1..128)` |   | `true`
`org` | Full legal name of the organization | `string` |   | `false`
`preflow` | Each property provides functionality that can be applied to calls using the callflow application | `object` | `{}` | `false`
`preflow.always` | The ID of a callflow to always execute prior to processing the callflow with numbers/patterns matching the request | `string` |   | `false`
`realm` | The realm of the account, ie: 'account1.2600hz.com' | `string(4..253)` |   | `false`
`ringtones` |   | `object` | `{}` | `false`
`ringtones.external` | The alert info SIP header added when the call is from internal sources | `string(0..256)` |   | `false`
`ringtones.internal` | The alert info SIP header added when the call is from external sources | `string(0..256)` |   | `false`
`timezone` | The default timezone | `string(5..32)` | `America/Los_Angeles` | `false`
`voicemail` |   | `object` |   | `false`
`voicemail.notify` |   | `object` |   | `false`
`voicemail.notify.callback` |   | `#/definitions/notify.callback` |   | `false`


#### Create a new child account

Puts the created account under the account of the owner of the `{AUTH_TOKEN}`. This is a shortcut for `PUT /v2/accounts/{AUTH_ACCOUNT_ID}`

> PUT /v2/accounts

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"name":"child account"}}' \
    http://{SERVER}:8000/v2/accounts
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
    "revision": "undefined",
    "status": "success"
}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

#### Create a new child account

Puts the created account under `{ACCOUNT_ID}`

> PUT /v2/accounts/{ACCOUNT_ID}

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"name":"child account"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
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
    "revision": "undefined",
    "status": "success"
}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/parents

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/parents
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/tree

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tree
```

#### Fetch the account's API key

The API key is used by the `api_auth` API to obtain an `auth_token`. This is intended for use by applications talking to kazoo and provides a mechanism for authentication that does not require storing a username and password in the application. The API key can be obtained via the accounts API's endpoint `api_key`.

> GET /v2/accounts/{ACCOUNT_ID}/api_key

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/api_key
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/siblings

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/siblings
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/descendants

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/descendants
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/children

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/children
```

#### Move an account

An account can only be moved by a "superduper_admin" or  if enabled by anyone above the desired account.

You can enable that feature by editing the document `crossbar.accounts` in you `system_config` database and set the value to `tree`.

```json
    "allow_move": "tree" // Default to "superduper_admin"
```

> POST /v2/accounts/{ACCOUNT_ID}/move

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"to": "{ACCOUNT_ID_DESTINATION}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/move
```
