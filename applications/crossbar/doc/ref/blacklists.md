### Blacklists

#### About Blacklists

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`name` | A friendly name for the temporal rule set | `string(1..128)` |   | `true`
`numbers` | Map of caller id number to block | `object` | `{}` | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/blacklists

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/blacklists

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/blacklists/{ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/blacklists/{ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists/{ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/blacklists/{ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/blacklists/{ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/blacklists/{ID}
```

