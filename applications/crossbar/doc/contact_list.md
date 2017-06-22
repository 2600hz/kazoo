### Contact List

#### About Contact List

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/contact_list

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/contact_list
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "external_number": "+14157775555",
            "internal_number": "1000",
            "name": "John Quux"
        }
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```
