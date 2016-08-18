### Accounts

#### About Accounts

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


#### Create

> PUT /v2/accounts

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/parents

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/parents
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/tree

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tree
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/api_key

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/api_key
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/siblings

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/siblings
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/descendants

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/descendants
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/children

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/children
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/reseller

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/reseller
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/reseller

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/reseller
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/move

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/move
```

