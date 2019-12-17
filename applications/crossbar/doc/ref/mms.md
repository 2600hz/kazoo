# Mms

## About Mms

#### Schema

mms document



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`body` | mime encoded mms message | `string(1..)` |   | `false` |  
`from` | caller-id-number, taken from user if absent | `string()` |   | `false` |  
`to` | callee-id-number | `string()` |   | `true` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/mms

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/mms
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/mms

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/mms
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/mms/{SMS_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/mms/{SMS_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/mms/{SMS_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/mms/{SMS_ID}
```

