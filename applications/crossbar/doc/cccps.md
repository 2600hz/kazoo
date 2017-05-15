### Cccps

#### About Cccps

#### Schema

Calling cards callback platform user's info



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`active` | Show's whether CID/PIN active | `boolean` | `false` | `false`
`cid` | CID to authorize | `string` |   | `false`
`max_concurent_calls_per_user` | Calls per user limit. Counts all user's legs and compares to max_concurent_calls_per_user multiplied by 2 | `integer` |   | `false`
`pin` | PIN to authorize | `string` |   | `false`
`retain_cid` | Pass initial caller number to the callee | `boolean` |   | `false`
`user_id` | The ID of the user object that 'owns' cid/pin | `string(32)` |   | `false`


#### List items

> GET /v2/accounts/{ACCOUNT_ID}/cccps

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cccps
```
