### Services

#### About Services

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/services

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services
```

#### Update the billing ID

> POST /v2/accounts/{ACCOUNT_ID}/services

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {
        "billing_id":"{BILLING_ID}"
    }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services
```

#### Fetch the audit logs for the account

> GET /v2/accounts/{ACCOUNT_ID}/services/audit

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/audit
```
