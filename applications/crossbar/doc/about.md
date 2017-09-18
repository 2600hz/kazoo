### About

#### About About

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/about

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/about
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "erlang_version": "19",
        "ports": 21,
        "processes": 1816,
        "used_memory": 89615664,
        "version": "4.0.0"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```
