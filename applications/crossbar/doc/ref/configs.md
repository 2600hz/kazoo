### Configs

#### About Configs

#### Schema



#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/configs/{ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/configs/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/configs/{ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/configs/{ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/configs/{ID}

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/configs/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/configs/{ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/configs/{ID}
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/configs/{ID}

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/configs/{ID}
```

