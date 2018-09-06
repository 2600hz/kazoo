# Transactions

## About

The transactions endpoint allows you to list debits and credits made to a specified account.

## List Transactions

> GET /v2/accounts/{ACCOUNT_ID}/transactions

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {"description": "monthly rollup",
         "id": "09dd02e20e07dbb65401802ba20cfb32",
         "amount": 10.179999999999999716,
         "reason": "database_rollup",
         "type": "credit",
         "created": 63598331974,
         "version": 2,
         "code": 9999
        }
        ,{"metadata": {
            "auth_account_id": "{AUTH_ACCOUNT_ID}"
        },
        "id": "7dd1c20894587e9cbacb2d7fa2de80ab",
        "amount": 1.0,
        "reason": "admin_discretion",
        "type": "debit",
        "created": 63598591394,
        "version": 2,
        "code": 3005
     }],
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
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
