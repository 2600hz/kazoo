# Temporal Rules Sets

## About Temporal Rules Sets

A temporal rule set is a collection of temporal rules that can be used in a callflow to match more that one rule. And can also be re-used.

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

## Create a new rule set

> PUT /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data": {"name": "July","temporal_rules": ["{RULE_ID}","{RULE_ID}"]}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules_sets
```

## Fetch a rule set

> GET /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}
```

## Change a rule set

> POST /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data": {"name": "July","temporal_rules": ["{RULE_ID}","{RULE_ID}"]}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}
```

## Patch a rule set

> PATCH /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}
```

## Remove a rule set

> DELETE /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{TEMPORAL_RULE_SET}
```
