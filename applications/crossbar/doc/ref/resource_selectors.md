# Resource Selectors

## About Resource Selectors

#### Schema

Schema for resource selector document



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`name` | Selector name | `string()` |   | `true` |  
`resource` | Resource ID | `string()` |   | `true` |  
`selector` | Selector data | `string()` |   | `true` |  
`start_time` | Start time (Gregorian seconds) | `integer()` |   | `false` |  
`stop_time` | Stop time (Gregorian seconds) | `integer()` |   | `false` |  
`value` | Extra selector data | `string()` |   | `false` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/{UUID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/{UUID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/resource_selectors/{UUID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/{UUID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/resource_selectors/{UUID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/{UUID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/resource
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/name

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/name
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/rules

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/rules
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/resource_selectors/rules

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/rules
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}/resource/{RESOURCE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}/resource/{RESOURCE_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/name/{SELECTOR_NAME}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/name/{SELECTOR_NAME}
```

