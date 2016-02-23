### Comments

#### About Comments

#### Schema



#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/comments

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/comments
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/comments

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/comments
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/comments

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/comments
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/comments/{ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/comments/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/comments/{ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/comments/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/comments/{ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/comments/{ID}
```

