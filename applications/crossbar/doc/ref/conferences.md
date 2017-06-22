### Conferences

#### About Conferences

#### Schema

Schema for conferences



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`bridge_password` | the password used for a conference bridge | `string()` |   | `false`
`bridge_username` | the username used for a conference bridge | `string()` |   | `false`
`caller_controls` | caller controls (config settings) | `string()` |   | `false`
`conference_numbers.[]` |   | `string()` |   | `false`
`conference_numbers` | Defines conference numbers that can be used by members or moderators | `array(string())` | `[]` | `false`
`focus` | This is a read-only property indicating the media server hosting the conference | `string()` |   | `false`
`max_members_media` | Media to play when the conference is full | `string()` |   | `false`
`max_participants` | The maximum number of participants that can join | `integer()` |   | `false`
`member.join_deaf` | Determines if a member will join deaf | `boolean()` | `false` | `false`
`member.join_muted` | Determines if a member will join muted | `boolean()` | `true` | `false`
`member.numbers.[]` |   | `string()` |   | `false`
`member.numbers` | Defines the conference (call in) number(s) for members | `array(string())` | `[]` | `false`
`member.pins.[]` |   | `string()` |   | `false`
`member.pins` | Defines the pin number(s) for members | `array(string())` | `[]` | `false`
`member` | Defines the discovery (call in) properties for a member | `object()` | `{}` | `false`
`moderator.join_deaf` | Determines if a moderator will join deaf | `boolean()` | `false` | `false`
`moderator.join_muted` | Determines if a moderator will join muted | `boolean()` | `false` | `false`
`moderator.numbers.[]` |   | `string()` |   | `false`
`moderator.numbers` | Defines the conference (call in) number(s) for moderators | `array(string())` | `[]` | `false`
`moderator.pins.[]` |   | `string()` |   | `false`
`moderator.pins` | Defines the pin number(s) for moderators | `array(string())` | `[]` | `false`
`moderator` | Defines the discovery (call in) properties for a moderator | `object()` | `{}` | `false`
`moderator_controls` | profile on the switch for controlling the conference as a moderator | `string()` |   | `false`
`name` | A friendly name for the conference | `string(1..128)` |   | `false`
`owner_id` | The user ID who manages this conference | `string(32)` |   | `false`
`play_entry_tone` | Whether to play an entry tone, or the entry tone to play | `boolean() | string()` |   | `false`
`play_exit_tone` | Whether to play an exit tone, or the exit tone to play | `boolean() | string()` |   | `false`
`play_name` | Do we need to announce new conference members? | `boolean()` | `false` | `false`
`play_welcome` | Whether to play the welcome prompt | `boolean()` |   | `false`
`profile` | The XML profile name used to configure the conference | `string()` |   | `false`
`require_moderator` | does the conference require a moderator | `boolean()` |   | `false`
`wait_for_moderator` | should members wait for a moderator before joining the conference | `boolean()` |   | `false`



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/conferences

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/conferences

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants/{PARTICIPANT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants/{PARTICIPANT_ID}
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants/{PARTICIPANT_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants/{PARTICIPANT_ID}
```

