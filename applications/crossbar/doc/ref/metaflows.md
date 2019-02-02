# Metaflows

## About Metaflows

#### Schema

Actions applied to a call outside of the normal callflow, initiated by the caller(s)



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`binding_digit` | What DTMF will trigger the collection and analysis of the subsequent DTMF sequence | `string('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' | '*' | '#')` | `*` | `false` |  
`digit_timeout` | How long to wait between DTMF presses before processing the collected sequence (milliseconds) | `integer()` |   | `false` |  
`listen_on` | Which leg(s) of the call to listen for DTMF | `string('both' | 'self' | 'peer')` |   | `false` |  
`numbers./^[0-9]+$/` | A metaflow node defines a module to execute, data to provide to that module, and one or more children to branch to | [#/definitions/metaflow](#metaflow) |   | `false` |  
`numbers` | A list of static numbers with their flows | `object()` |   | `false` |  
`patterns./.+/` | A metaflow node defines a module to execute, data to provide to that module, and one or more children to branch to | [#/definitions/metaflow](#metaflow) |   | `false` |  
`patterns` | A list of patterns with their flows | `object()` |   | `false` |  

### metaflow

A metaflow node defines a module to execute, data to provide to that module, and one or more children to branch to


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`children./.+/` | A metaflow node defines a module to execute, data to provide to that module, and one or more children to branch to | [#/definitions/metaflow](#metaflow) |   | `false` |  
`children` | Children metaflows | `object()` |   | `false` |  
`data` | The data/arguments of the metaflow module | `object()` | `{}` | `false` |  
`module` | The name of the metaflow module to execute at this node | `string(1..64)` |   | `true` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/metaflows

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/metaflows
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/metaflows

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/metaflows
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/metaflows

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/metaflows
```

