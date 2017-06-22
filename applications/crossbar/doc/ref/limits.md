### Limits

#### About Limits

#### Schema

Limit an account's ability to place concurrent calls using flat rate trunks



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`allow_prepay` | Determines if the account would like to allow per-minute calls if they have no available credit | `boolean()` | `true` | `false`
`burst_trunks` | The number of two-way, flat-rate trunks used only if no other trunks are available | `integer()` |   | `false`
`calls` | A hard limit for the total number calls | `integer()` |   | `false`
`inbound_trunks` | The number of inbound, flat-rate trunks | `integer()` |   | `false`
`outbound_trunks` | The number of outbound, flat-rate trunks | `integer()` |   | `false`
`resource_consuming_calls` | A hard limit for the number of resource consuming calls | `integer()` |   | `false`
`twoway_trunks` | The number of two-way, flat-rate trunks | `integer()` |   | `false`



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/limits

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/limits
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/limits

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/limits
```

