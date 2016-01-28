### Connectivity

#### About Connectivity

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`account` | Information that applies to the account as a whole | `object` |   | `false`
`account.auth_realm` | The realm any device in the account will use to authenticate with | `string` |   | `false`
`account.caller_id` |   | `object` |   | `false`
`account.caller_id.cid_name` |   | `string` |   | `false`
`account.caller_id.cid_number` |   | `string` |   | `false`
`account.emergency_caller_id` |   | `object` |   | `false`
`account.emergency_caller_id.cid_name` |   | `string` |   | `false`
`account.emergency_caller_id.cid_number` |   | `string` |   | `false`
`account.trunks` | The number of two-way trunks this account has purchased | `integer` |   | `false`
`name` | Human-friendly name of the trunkstore account | `string` |   | `false`
`servers` |   | `array` | `[]` | `false`
`servers.[].DIDs` |   | `object` |   | `false`
`servers.[].auth` |   | `object` |   | `true`
`servers.[].auth.auth_method` | What type of auth mechanism to use | `string` | `password` | `true`
`servers.[].auth.auth_password` | Password of the user@auth_realm | `string` |   | `false`
`servers.[].auth.auth_user` | Username for authentication | `string` |   | `false`
`servers.[].auth.ip` | IP address for this device | `string` |   | `false`
`servers.[].auth.port` | Port to send SIP traffic for the remote device | `integer` |   | `false`
`servers.[].name` | Human-friendly name of the server | `string` |   | `false`
`servers.[].options` |   |   |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNTID}/connectivity

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/connectivity
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/connectivity

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/connectivity
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/connectivity/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/connectivity/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/connectivity/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/connectivity/{ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNTID}/connectivity/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/connectivity/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/connectivity/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/connectivity/{ID}
```

