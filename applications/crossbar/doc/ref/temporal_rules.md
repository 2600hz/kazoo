# Temporal Rules

## About Temporal Rules

#### Schema

Schema for a temporal rules



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`cycle` | The recurrence cycle for this rule | `string('date' | 'daily' | 'weekly' | 'monthly' | 'yearly')` |   | `true` | `supported`
`days` | The recurrence days for this rule | `array(integer())` |   | `false` | `supported`
`enabled` | Whether the rule is enabled | `boolean()` |   | `false` |  
`interval` | The recurrence interval for this rule | `integer()` | `1` | `false` | `supported`
`month` | The recurrence month for this rule | `integer()` |   | `false` | `supported`
`name` | A friendly name for the temporal rule | `string(1..128)` |   | `true` | `supported`
`ordinal` | The recurrence ordinal for this rule | `string('every' | 'first' | 'second' | 'third' | 'fourth' | 'fifth' | 'last')` |   | `false` | `supported`
`start_date` | The date that any recurrence should be calculated as starting on | `integer()` | `62586115200` | `false` | `supported`
`time_window_start` | Seconds from the start of a day to consider this rule valid | `integer()` |   | `false` | `supported`
`time_window_stop` | Seconds from the start of a day to stop considering this rule valid | `integer()` |   | `false` | `supported`
`wdays.[]` |   | `string('monday' | 'tuesday' | 'wednesday' | 'wensday' | 'thursday' | 'friday' | 'saturday' | 'sunday')` |   | `false` | `supported`
`wdays` | The recurrence weekdays for this rule | `array(string('monday' | 'tuesday' | 'wednesday' | 'wensday' | 'thursday' | 'friday' | 'saturday' | 'sunday'))` |   | `false` | `supported`



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/temporal_rules

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/temporal_rules

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules/{TEMPORAL_RULE_ID}
```

