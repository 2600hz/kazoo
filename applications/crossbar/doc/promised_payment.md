### Promised_payment

#### About Promised_payment
Promised payment is posptpay limited in time.  
It have start time and duration time. Start time and duration specified in seconds.   
If this function enabled for account, he could "arm" it in any time. After arming, this function can be "disarnmed" only if account have positive balance (prepeay + postpay).  
When function armed account have additional amount of money for per_minute calls. Money available only if function enabled ("enabled": true), armed ("armded":true) and current time within working range ( start > now > start+duration ).

#### Restrictions
Reseller/SuperAdmin can change all parameters for any of his descendant account.  
Account can only change "amount", "duration" and "armed" parameters.  
"amount" and "duration" cannot exceed values in "max_amount" and "max_duration".  
When account changing "armed" to "true" - "satart" value automaticaly changed to current timestamp.  
System-wide prameters stored in "system_config / jonny5 / { node_name | default } / default_promised_payment.  

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`amount` |   | `number` | `0.0` | `true`
`armed` |   | `boolean` | `false` | `true`
`duration` |   | `integer` | `0` | `true`
`enabled` |   | `boolean` | `false` | `true`
`max_amount` |   | `number` | `0.0` | `true`
`max_duration` |   | `integer` | `86400` | `true`
`start` |   | `integer` | `0` | `true`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/promised_payment

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/promised_payment
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/promised_payment

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/promised_payment
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/promised_payment

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/promised_payment
```

