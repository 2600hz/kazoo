### Alerts

#### About Alerts

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/alerts

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/alerts
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/alerts

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/alerts
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/alerts/{ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/alerts/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/alerts/{ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/alerts/{ID}
```

