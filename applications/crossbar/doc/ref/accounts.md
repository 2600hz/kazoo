### Accounts

#### About Accounts

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`call_restriction` | Account level call restrictions for each available number classification | `object` | `{}` | `false`
`call_waiting` |   |   |   | `false`
`caller_id` | The account default caller ID parameters | `object` | `{}` | `false`
`dial_plan` | A list of default rules used to modify dialed numbers | `object` | `{}` | `false`
`do_not_disturb` |   | `object` |   | `false`
`do_not_disturb.enabled` | The default value for do-not-disturb | `boolean` |   | `false`
`enabled` | Determines if the account is currently enabled | `boolean` | `true` | `false`
`language` | The language for this account | `string` | `en-us` | `false`
`metaflows` |   |   |   | `false`
`music_on_hold` | The default music on hold parameters | `object` | `{}` | `false`
`music_on_hold.media_id` | The ID of a media object that should be used as the default music on hold | `string` |   | `false`
`name` | A friendly name for the account | `string` |   | `true`
`org` | Full legal name of the organization | `string` |   | `false`
`preflow` | Each property provides functionality that can be applied to calls using the callflow application | `object` | `{}` | `false`
`preflow.always` | The ID of a callflow to always execute prior to processing the callflow with numbers/patterns matching the request | `string` |   | `false`
`realm` | The realm of the account, ie: 'account1.2600hz.com' | `string` |   | `false`
`ringtones` |   | `object` | `{}` | `false`
`ringtones.external` | The alert info SIP header added when the call is from internal sources | `string` |   | `false`
`ringtones.internal` | The alert info SIP header added when the call is from external sources | `string` |   | `false`
`timezone` | The default timezone | `string` | `America/Los_Angeles` | `false`


#### Create

> PUT /v2/accounts

```curl
curl -v http://{SERVER}:8000//v2/accounts
```

#### Remove

> DELETE /v2/accounts/AccountId

```curl
curl -v http://{SERVER}:8000//v2/accounts/AccountId
```

#### Fetch

> GET /v2/accounts/AccountId

```curl
curl -v http://{SERVER}:8000//v2/accounts/AccountId
```

#### Patch

> PATCH /v2/accounts/AccountId

```curl
curl -v http://{SERVER}:8000//v2/accounts/AccountId
```

#### Change

> POST /v2/accounts/AccountId

```curl
curl -v http://{SERVER}:8000//v2/accounts/AccountId
```

#### Create

> PUT /v2/accounts/AccountId

```curl
curl -v http://{SERVER}:8000//v2/accounts/AccountId
```

#### Fetch

> GET /v2/accounts/{ID}/Path

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ID}/Path
```

#### Change

> POST /v2/accounts/{ID}/move

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ID}/move
```

