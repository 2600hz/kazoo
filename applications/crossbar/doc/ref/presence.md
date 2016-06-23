### Presence

#### About Presence

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/presence

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/presence
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/presence

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/presence
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/presence/{EXTENSION}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/presence/{EXTENSION}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/presence/report-{REPORT_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/presence/report-{REPORT_ID}
```

