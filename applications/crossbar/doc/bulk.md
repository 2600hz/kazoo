### Bulk

#### About Bulk

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/bulk

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"ids": ["{ID1}", "{ID2}", "{ID3}"]}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/bulk
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        ...
    ],
    "page_size": 0,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```
