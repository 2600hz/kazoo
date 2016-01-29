### Ledgers

#### About Ledgers

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`amount` | Ledger amount | `integer` |   | `true`
`description` | Useful description for ledger | `string` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/ledgers

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ledgers
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/ledgers/{ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ledgers/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/ledgers/{ID}/debit

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ledgers/{ID}/debit
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/ledgers/{ID}/credit

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ledgers/{ID}/credit
```

