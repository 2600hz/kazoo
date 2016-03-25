### Temporal_rules_sets

#### About Temporal_rules_sets

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`name` | A friendly name for the temporal rule set | `string(1..128)` |   | `true`
`temporal_rules` | Rules | `array(string)` |   | `false`
`temporal_rules.[]` |   | `string` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules_sets
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules_sets
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{ID}

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules_sets/{ID}
```

