# Allotments

## About Allotments

#### Schema

Create buckets of minutes per time-period



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`^\w+$.amount` |   | `integer()` |   | `false` |  
`^\w+$.cycle` |   | `string('minutely' | 'hourly' | 'daily' | 'weekly' | 'monthly')` |   | `false` |  
`^\w+$.group_consume.[]` |   | `string()` |   | `false` |  
`^\w+$.group_consume` |   | `array(string())` |   | `false` |  
`^\w+$.increment` |   | `integer()` |   | `false` |  
`^\w+$.minimum` |   | `integer()` |   | `false` |  
`^\w+$.no_consume_time` |   | `integer()` |   | `false` |  
`^\w+$` |   | `object()` |   | `false` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/allotments

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/allotments
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/allotments

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/allotments
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/allotments/consumed

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/allotments/consumed
```

