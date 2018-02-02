# Transactions

## About Transactions

## Schema



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/transactions

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/transactions/subscriptions

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/subscriptions
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/transactions/monthly_recurring

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/monthly_recurring
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/transactions/debit

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/debit
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/transactions/credit

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/credit
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/transactions/current_balance

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/current_balance
```

