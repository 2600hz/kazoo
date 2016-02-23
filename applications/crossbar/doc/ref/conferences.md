### Conferences

#### About Conferences

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`conference_numbers` | Defines conference numbers that can be used by members or moderators | `array` | `[]` | `false`
`conference_numbers.[]` |   | `string` |   | `false`
`focus` | This is a read-only property indicating the media server hosting the conference | `string` |   | `false`
`member` | Defines the discovery properties for a member | `object` | `{}` | `false`
`member.join_deaf` | Determines if a member will join deaf | `boolean` | `false` | `false`
`member.join_muted` | Determines if a member will join muted | `boolean` | `true` | `false`
`member.numbers` | Defines the conference number(s) for members | `array` | `[]` | `false`
`member.numbers.[]` |   | `string` |   | `false`
`member.pins` | Defines the pin number(s) for members | `array` | `[]` | `false`
`member.pins.[]` |   | `string` |   | `false`
`moderator` | Defines the discovery properties for a moderator | `object` | `{}` | `false`
`moderator.join_deaf` | Determines if a moderator will join deaf | `boolean` | `false` | `false`
`moderator.join_muted` | Determines if a moderator will join muted | `boolean` | `false` | `false`
`moderator.numbers` | Defines the conference number(s) for moderators | `array` | `[]` | `false`
`moderator.numbers.[]` |   | `string` |   | `false`
`moderator.pins` | Defines the pin number(s) for moderators | `array` | `[]` | `false`
`moderator.pins.[]` |   | `string` |   | `false`
`name` | A friendly name for the conference | `string` |   | `false`
`owner_id` | The user ID who manages this conference | `string` |   | `false`
`play_name` | Do we need to announce new conference members? | `boolean` | `false` | `false`
`profile` | The XML profile name used to configure the conference | `string` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNTID}/conferences

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/conferences
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/conferences

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/conferences
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/conferences/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/conferences/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/conferences/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/conferences/{ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNTID}/conferences/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/conferences/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/conferences/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/conferences/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/conferences/{ID}/details

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/conferences/{ID}/details
```

