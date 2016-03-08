### Channels

#### About Channels

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/channels

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/channels
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/channels/{UUID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/channels/{UUID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/channels/{UUID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/channels/{UUID}
```

