### Transactions

#### About Transactions

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/transactions

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/transactions/subscriptions

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/subscriptions
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/transactions/monthly_recurring

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/monthly_recurring
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/transactions/debit

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/debit
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/transactions/credit

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/credit
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/transactions/current_balance

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/current_balance
```

