### Allotments

#### About Allotments

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/allotments

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/allotments
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/allotments

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/allotments
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/allotments/consumed

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/allotments/consumed
```

