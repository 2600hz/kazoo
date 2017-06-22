### Apps Link

#### About Apps Link

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/apps_link/authorize

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/apps_link/authorize
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "account": {
            "account_id": "{ACCOUNT_ID}",
            "account_name": "{ACCOUNT_NAME}",
            "is_master": false,
            "is_reseller": false,
            "language": "en-us",
            "reseller_id": "6b71cb72c876b5b1396a335f8f8a2594"
        },
        "auth_token": {
            "account_id": "{ACCOUNT_ID}",
            "account_name": "{ACCOUNT_NAME}",
            "apps": [
                {
                    "api_url": "http://localhost:8000/v2",
                    "id": "44c076738144b5f4f80542ce49035a27",
                    "label": "Accounts Manager",
                    "name": "accounts"
                },
                {
                    "api_url": "http://localhost:8000/v2",
                    "id": "06815c7173aafa575bcf3c68ee124a77",
                    "label": "Callflows",
                    "name": "callflows"
                },
                {
                    "api_url": "http://localhost:8000/v2",
                    "id": "41f5eba03d33fc10740df7f541745f1d",
                    "label": "Number Manager",
                    "name": "numbers"
                },
                {
                    "api_url": "http://localhost:8000/v2",
                    "id": "3eb4f9230f95a7a60ac825021ad0affe",
                    "label": "Smart PBX",
                    "name": "voip"
                },
                {
                    "api_url": "http://localhost:8000/v2",
                    "id": "bbd90ad61193b2a9a3529b4f78038b5e",
                    "label": "Webhooks",
                    "name": "webhooks"
                }
            ],
            "is_master": false,
            "is_reseller": false,
            "language": "en-us",
            "method": "cb_user_auth",
            "owner_id": "8e248327b85591955749e53ea45b6baa",
            "reseller_id": "6b71cb72c876b5b1396a335f8f8a2594"
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```
