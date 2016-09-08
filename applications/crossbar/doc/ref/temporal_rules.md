### Temporal_rules

#### About Temporal_rules

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`cycle` | The recurrence cycle for this rule | `string('date', 'daily', 'weekly', 'monthly', 'yearly')` |   | `true`
`days` | The recurrence days for this rule | `array(integer)` |   | `false`
`interval` | The recurrence interval for this rule | `integer` | `1` | `false`
`month` | The recurrence month for this rule | `integer` |   | `false`
`name` | A friendly name for the temporal rule | `string(1..128)` |   | `true`
`ordinal` | The recurrence ordinal for this rule | `string('every', 'first', 'second', 'third', 'fourth', 'fifth', 'last')` |   | `false`
`start_date` | The date that any recurrence should be calculated as starting on | `integer` | `62586115200` | `false`
`time_window_start` | Seconds from the start of a day to stop considering this rule valid | `integer` |   | `false`
`wdays` | The recurrence weekdays for this rule | `array(string('monday', 'tuesday', 'wednesday', 'wensday', 'thursday', 'friday', 'saturday', 'sunday'))` |   | `false`
`wdays.[]` |   | `string` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/temporal_rules

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/temporal_rules

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}
```

