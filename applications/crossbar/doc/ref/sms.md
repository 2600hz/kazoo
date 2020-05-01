# Sms

## About Sms

#### Schema

sms document



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`body` | text message | `string(1..700)` |   | `true` |  
`from` | caller-id-number, taken from user if absent | `string()` |   | `false` |  
`to` | callee-id-number | `string()` |   | `true` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/sms

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/sms
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/sms

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/sms
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/sms/{SMS_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/sms/{SMS_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/sms/{SMS_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/sms/{SMS_ID}
```

