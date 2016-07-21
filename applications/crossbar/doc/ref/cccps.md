### Cccps

#### About Cccps

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`active` | Show's whether CID/PIN active | `boolean` | `false` | `false`
`cid` | CID to authorize | `string` |   | `false`
`max_concurent_calls_per_user` | Calls per user limit. Counts all user's legs and compares to max_concurent_calls_per_user multiplied by 2 | `integer` |   | `false`
`pin` | PIN to authorize | `string` |   | `false`
`retain_cid` | Pass initial caller number to the callee | `boolean` |   | `false`
`user_id` | The ID of the user object that 'owns' cid/pin | `string(32)` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/cccps

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cccps
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/cccps

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cccps
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/cccps/{ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cccps/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/cccps/{ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cccps/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/cccps/{ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cccps/{ID}
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/cccps/{ID}

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cccps/{ID}
```

