### Connectivity

#### About Connectivity

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`account` | Information that applies to the account as a whole | `object` |   | `false`
`account.auth_realm` | The realm any device in the account will use to authenticate with | `string(1..)` |   | `false`
`account.caller_id` |   | `object` |   | `false`
`account.caller_id.cid_name` |   | `string(0..35)` |   | `false`
`account.caller_id.cid_number` |   | `string(0..35)` |   | `false`
`account.emergency_caller_id` |   | `object` |   | `false`
`account.emergency_caller_id.cid_name` |   | `string(0..35)` |   | `false`
`account.emergency_caller_id.cid_number` |   | `string(0..35)` |   | `false`
`account.trunks` | The number of two-way trunks this account has purchased | `integer` |   | `false`
`name` | Human-friendly name of the trunkstore account | `string` |   | `false`
`servers` |   | `array(object)` | `[]` | `false`
`servers.[].DIDs` |   | `object` |   | `false`
`servers.[].DIDs.^\+?\d*$` |   | `object` |   | `false`
`servers.[].DIDs.^\+?\d*$.caller_id` |   | `object` |   | `false`
`servers.[].DIDs.^\+?\d*$.caller_id.cid_name` |   | `string(1..35)` |   | `true`
`servers.[].DIDs.^\+?\d*$.caller_id.cid_number` |   | `string(1..35)` |   | `true`
`servers.[].DIDs.^\+?\d*$.failover` | Route inbound call to another destination if this server fails to handle the call | `object` |   | `false`
`servers.[].DIDs.^\+?\d*$.failover.e164` | An E.164 formatted DID to dial for failover | `string` |   | `false`
`servers.[].DIDs.^\+?\d*$.failover.sip` | A SIP URI (sip:user@host) to call for failover | `string` |   | `false`
`servers.[].DIDs.^\+?\d*$.force_outbound` |   | `boolean` | `false` | `false`
`servers.[].DIDs.^\+?\d*$.options` |   | `array(string)` |   | `false`
`servers.[].DIDs.^\+?\d*$.options.[]` |   | `string` |   | `false`
`servers.[].auth` |   | `object` |   | `true`
`servers.[].auth.auth_method` | What type of auth mechanism to use | `string('password', 'Password', 'IP', 'ip')` | `password` | `true`
`servers.[].auth.auth_password` | Password of the user@auth_realm | `string(1..)` |   | `false`
`servers.[].auth.auth_user` | Username for authentication | `string(1..)` |   | `false`
`servers.[].auth.ip` | IP (sip) address for this device | `string` |   | `false`
`servers.[].auth.port` | Port to send SIP traffic for the remote device | `integer` |   | `false`
`servers.[].name` | Human-friendly name of the server | `string(1..)` |   | `false`
`servers.[].options` |   | `object` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/connectivity

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/connectivity
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/connectivity

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/connectivity
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/connectivity/{CONNECTIVITY_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/connectivity/{CONNECTIVITY_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/connectivity/{CONNECTIVITY_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/connectivity/{CONNECTIVITY_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/connectivity/{CONNECTIVITY_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/connectivity/{CONNECTIVITY_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/connectivity/{CONNECTIVITY_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/connectivity/{CONNECTIVITY_ID}
```

