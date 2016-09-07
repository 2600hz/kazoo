### Blacklists

#### About Blacklists

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`name` | A friendly name for the temporal rule set | `string(1..128)` |   | `true`
`numbers` | Map of caller id number to block | `object` | `{}` | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/blacklists

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/blacklists

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists/{BLACKLIST_ID}
```

