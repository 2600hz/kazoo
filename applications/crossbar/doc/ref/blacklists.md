### Blacklists

#### About Blacklists

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`name` | A friendly name for the temporal rule set | `string` |   | `true`
`numbers` | Map of caller id number to block | `object` | `{}` | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNTID}/blacklists

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/blacklists
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/blacklists

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/blacklists
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/blacklists/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/blacklists/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/blacklists/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/blacklists/{ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNTID}/blacklists/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/blacklists/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/blacklists/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/blacklists/{ID}
```

