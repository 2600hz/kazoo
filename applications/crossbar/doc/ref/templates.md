### Templates

#### About Templates

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/templates

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/templates
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/templates/{ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/templates/{ID}
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/templates/{ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/templates/{ID}
```

