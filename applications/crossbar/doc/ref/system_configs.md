# System Configs

## About System Configs

#### Schema

Schema for system_config documents



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`(?!id\b)(?!default\b)^.+@.+$` | Node-specific settings - these take highest precedence | `object()` |   | `false` |  
`(?!id\b)(?!default\b)^[a-zA-Z0-9.]+$` | Zone-specific settings - these are checked if a node-specific setting is not defined | `object()` |   | `false` |  
`default` | default settings that apply to all nodes/zones if not defined | `object()` | `{}` | `true` |  



## Fetch

> GET /v2/system_configs

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs
```

## Fetch

> GET /v2/system_configs/{SYSTEM_CONFIG_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}
```

## Create

> PUT /v2/system_configs/{SYSTEM_CONFIG_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}
```

## Change

> POST /v2/system_configs/{SYSTEM_CONFIG_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}
```

## Patch

> PATCH /v2/system_configs/{SYSTEM_CONFIG_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}
```

## Remove

> DELETE /v2/system_configs/{SYSTEM_CONFIG_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}
```

## Fetch

> GET /v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}
```

## Change

> POST /v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}
```

## Patch

> PATCH /v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}
```

## Remove

> DELETE /v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}
```

