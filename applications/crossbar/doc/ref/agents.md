# Agents

## About Agents

## Schema



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/stats_summary

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/stats_summary
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/stats

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/stats
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/status

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/status
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/restart

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/restart
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/queue_status

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/queue_status
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/queue_status

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/queue_status
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/status

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/status
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/status

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/status
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/status/{USER_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/status/{USER_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/agents/status/{USER_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/status/{USER_ID}
```

