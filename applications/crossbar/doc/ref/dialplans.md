### Dialplans

#### About Dialplans

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`system` | List of system dial plans | `array()` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/dialplans

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/dialplans
```

