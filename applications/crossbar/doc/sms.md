### Sms

#### About Sms

SMS api endpoint allows sending a text message.

#### Schema

sms document



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`body` | text message | `string(1..700)` |   | `true` |  
`from` | caller-id-number, taken from user if absent | `string()` |   | `false` |  
`to` | callee-id-number | `string()` |   | `true` |  



## Sending a text message

> PUT /v2/accounts/{ACCOUNT_ID}/sms

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"from": "+15551112222", "to": "+15552221111", "body": "wave if you received this"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/sms
```

```json
{
    "request_id": "{REQUEST_ID}",
    "tokens": {
        "consumed": 1,
        "remaining": 100
    },
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "message_id": "{REQUEST_ID}",
        "from": "+15551112222",
        "to": "+15552221111",
        "body": "waive if you received this",
        "custom_vars": {
            "account_id": "{ACCOUNT_ID}",
            "reseller_id": "{RESELLER_ID}",
            "authorizing_type": "account",
            "authorizing_id": "{ACCOUNT_ID}"
        },
        "route_type": "offnet"
    },
    "status": "success"
}
```
