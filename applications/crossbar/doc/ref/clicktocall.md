# Clicktocall

## About Clicktocall

#### Schema

Click-to-call allows you to create URLs that can be POSTed to with a phone number or SIP URI and create a phone call from the provided contact information to a destination you have pre-determined.



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`auth_required` | Determines if this click to call requires valid auth-tokens when invoked | `boolean()` | `true` | `false` |  
`caller_id_number` | Explicitly set caller id number | `string()` |   | `false` |  
`dial_first` | Determines what will be dialed first: extension or contact | `string('extension' | 'contact')` |   | `false` |  
`extension` | The extension to connect to when the click to call is invoked | `string()` |   | `true` |  
`name` | A friendly name for the click to call | `string(1..128)` |   | `true` |  
`outbound_callee_id_name` | Callee ID Name of the device calling out to the contact number | `string()` |   | `false` |  
`outbound_callee_id_number` | Callee ID Number of the device calling out to the contact number | `string()` |   | `false` |  
`throttle` | The rate that this click to call can be invoked | `integer()` |   | `false` |  
`whitelist.[]` |   | `string(1..)` |   | `false` |  
`whitelist` | A list of regular expressions that the click to call can dial to | `array(string(1..))` |   | `false` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/clicktocall

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/clicktocall

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/history

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/history
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/connect

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/connect
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/connect

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/clicktocall/{C2C_ID}/connect
```

