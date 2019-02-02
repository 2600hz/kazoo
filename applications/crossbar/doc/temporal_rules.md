# Temporal Rules

## About Temporal Rules

Temporal rules provide a flexible way to configure time-based Call routing, e.g. open hours, holidays, close hours, etc...

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



### Notes on fields

#### `enabled`

Unless you need to override a time of day rule (for example keep an office open longer) keep the property unset.

#### `start_date`

It is recommended that a start date always be set to some time in the past if this control is not required to ensure it takes effect on the next cycle.

Setting this property is especially important when using an interval other than 1. For example if the rule should be applied every other year and the start date is in 2010, then it will be active on 2010, 2012, 2014, etc. However, if the start date was in 2011 then it will be active on 2011, 2013, 2015, etc.

#### `ordinal`

Not all months have a fifth occurrence of a weekday; the rule is ignored if that is the case.

#### `cycle`

When `cycle` is `date`, the rule only considers `start_date` and matches it against the current day.

#### `days`

The `days` array is only valid when `cycle` is `yearly` or `monthly`.

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/temporal_rules

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules
```
```json
{
   "auth_token":"{AUTH_TOKEN}",
   "status":"success",
   "request_id":"{REQUEST_ID}",
   "revision":"{REVISION}",
   "data":[
      {
         "id":"{TEMPORAL_RULE_ID}",
         "name":"Business Hours"
      },
      {
         "id":"{TEMPORAL_RULE_ID}",
         "name":"Holiday"
      }
   ]
}
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/temporal_rules

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"time_window_start":0,"time_window_stop":86400,"days":[25],"name":"Christmas","cycle":"yearly","start_date":62586115200,"month":12,"ordinal":"every","interval":1}}'
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/temporal_rules
```
```json
{
   "auth_token":"{AUTH_TOKEN}",
   "status":"success",
   "request_id":"{REQUEST_ID}",
   "revision":"{REVISION}",
   "data":{
      "time_window_start":0,
      "time_window_stop":86400,
      "days":[25],
      "name":"Christmas",
      "cycle":"yearly",
      "start_date":62586115200,
      "month":12,
      "ordinal":"every",
      "interval":1
   }
}
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
