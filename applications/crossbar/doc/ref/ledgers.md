### Ledgers

#### About Ledgers

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`amount` | Ledger amount | `integer` |   | `true`
`description` | Useful description for ledger | `string` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNTID}/ledgers

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/ledgers
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/ledgers/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/ledgers/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/ledgers/{ID}/debit

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/ledgers/{ID}/debit
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/ledgers/{ID}/credit

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/ledgers/{ID}/credit
```

