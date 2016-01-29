### Transactions

#### About Transactions

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/transactions

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/transactions/subscriptions

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/subscriptions
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/transactions/monthly_recurring

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/monthly_recurring
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/transactions/debit

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/debit
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/transactions/current_balance

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/current_balance
```

