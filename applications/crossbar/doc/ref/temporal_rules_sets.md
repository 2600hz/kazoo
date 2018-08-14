# Temporal Rules Sets

## About Temporal Rules Sets

#### Schema

Schema for a temporal rules sets



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`name` | A friendly name for the temporal rule set | `string(1..128)` |   | `true` | `supported`
`temporal_rules.[]` |   | `string()` |   | `false` | `supported`
`temporal_rules` | Temporal Rules | `array(string())` |   | `false` | `supported`



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules_sets
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules_sets
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}
```

