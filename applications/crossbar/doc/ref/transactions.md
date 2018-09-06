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

> GET /v2/accounts/{ACCOUNT_ID}/transactions/{TRANSACTION_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/{TRANSACTION_ID}
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/transactions/sale

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/sale
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/transactions/refund

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/refund
```

