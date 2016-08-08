### Rate_limits

#### About Rate_limits

#### Schema



#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/rate_limits

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/rate_limits
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/rate_limits

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/rate_limits
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/rate_limits

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/rate_limits
```

