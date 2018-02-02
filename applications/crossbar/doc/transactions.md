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

## Get current balance

> GET /v2/accounts/{ACCOUNT_ID}/transactions/current_balance

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/current_balance
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "balance": 9.18
     },
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

## Get monthly recurring transactions

Useful query string options:

- `created_from={TIMESTAMP}`
- `created_to={TIMESTAMP}`
- `reason=only_calls`

> GET /v2/accounts/{ACCOUNT_ID}/transactions/monthly_recurring

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/monthly_recurring
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
     },
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

## Get subscriptions

> GET /v2/accounts/{ACCOUNT_ID}/transactions/subscriptions

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/subscriptions
```

## Credit an account

> PUT /v2/accounts/{ACCOUNT_ID}/transactions/credit

Only for super duper administrators and resellers.

Super admin can add `"credit_type": "free"` field and change`"reason": "admin discretion"`to avoid bookkeeper and add credit "for free".

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {
        "amount": 1,
        "reason": "manual_addition",
        "description": "Wire transfer, Invoice #1, dated by 01/01/2016"
    }}' \
http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/credit
```


## Debit an account

Only for super duper administrators and resellers.

> DELETE /v2/accounts/{ACCOUNT_ID}/transactions/debit

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"amount": 1}}'  \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/transactions/debit
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "metadata": {
            "auth_account_id": "{ACCOUNT_ID}"
        },
        "id": "d478a0f74865c8512b71daf82b602b7a",
        "amount": 1.0,
        "reason": "admin_discretion",
        "type": "debit",
        "created": 63598603319,
        "version": 2,
        "code": 3005
     },
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```
