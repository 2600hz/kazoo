### Temporal_rules

#### About Temporal_rules

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`cycle` | The recurrence cycle for this rule | `string` |   | `true`
`days` | The recurrence days for this rule | `array` |   | `false`
`interval` | The recurrence interval for this rule | `integer` | `1` | `false`
`month` | The recurrence month for this rule | `integer` |   | `false`
`name` | A friendly name for the temporal rule | `string` |   | `true`
`ordinal` | The recurrence ordinal for this rule | `string` |   | `false`
`start_date` | The date that any recurrence should be calculated as starting on | `integer` | `62586115200` | `false`
`time_window_start` | Seconds from the start of a day to stop considering this rule valid | `integer` |   | `false`
`wdays` | The recurrence weekdays for this rule | `array` |   | `false`
`wdays.[]` |   | `string` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNTID}/temporal_rules

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/temporal_rules
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/temporal_rules

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/temporal_rules
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/temporal_rules/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/temporal_rules/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/temporal_rules/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/temporal_rules/{ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNTID}/temporal_rules/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/temporal_rules/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/temporal_rules/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/temporal_rules/{ID}
```

