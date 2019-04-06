# Rates Task

Background job to manage `ratedeck` DB.

!!! note
    Only super admin can use this module.

## About rates

Rates are stored in a database named after the `ratedeck_id` attribute. This defaults to `ratedeck` if not supplied.

An account can be assigned a ratedeck; if no ratedeck is assigned, the default ratedeck database is used. The account will be queried first, then the reseller, before selecting the default ratedeck.

## Fields

Name | Description | Required
---- | ----------- | --------
`account_id`|reseller's account (see first below)|
`description`|description for rate|
`direction`|direction of call leg ("inbound", "outbound"), if not set - rate matches both directions|
`iso_country_code`|[ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1#Officially_assigned_code_elements) code for prefix's country|
`prefix`|prefix for match DID number| `true`
`pvt_rate_cost`|internal rate cost, used for `weight` calculation|
`pvt_rate_surcharge`|internal rate surcharge|
`rate_cost`|per minute cost| `true`
`rate_increment`|billing "steps" for rate|
`rate_minimum`|minimum call duration|
`rate_name`|short name for rate, if this field not set it will be generated from `prefix`, `iso_country_code` and `direction` fields|
`rate_nocharge_time`|"free" call time, if call duration less then this value (seconds), then call not charged|
`rate_surcharge`|charge amount on connect (answer)|
`rate_version`|rate version|
`ratedeck_id`| ratedeck name, assigned to account via service plan|
`weight`|when found several rates with same prefix, used rate with higher weight. If not set - calculated from `prefix` length and `rate_cost` (`pvt_rate_cost`)|
`caller_id_numbers`|string of caller id prefixes separated by ":". For ex.: 441:442:443 It will be converted into ["^\\+?441.+$", "^\\+?442.+$", "^\\+?443.+$"]|
`routes`|string of either a single route "^\+?44.+$" or a json array string '["^\\+?123$","^\\+?456$"]'|

CSV files for all actions use the same list of fields. Names of fields match the names of keys in the CouchDB [rate document](../../crossbar/doc/rates.md#schema).

!!! note
    For `import` and `delete` actions, value of `account_id` from CSV file will be ignored, value for this field is taken from task Account-ID .

!!! note
    `options` field can not be defined via CSV file (because its values are lists).

`routes` is automatically generated from `prefix` as the default or the json decode fails to parse the json array. Example: `prefix` 380 generates `routes` - `["^\\+?380.+$"]`.

## Actions

### Import

Import rates from CSV.

First, query the tasks API to learn what fields must be present in your CSV:

```shell
curl -v \
-H "X-Auth-Token: {AUTH_TOKEN}" \
'http://{SERVER}:8000/v2/tasks?category=rates&action=import'
```

```json
{
    "data": {
        "tasks": {
            "rates": {
                "import": {
                    "description": "Bulk-import rates to a specified ratedeck",
                    "doc": "Creates rates from file",
                    "expected_content": "text/csv",
                    "mandatory": [
                        "prefix",
                        "rate_cost"
                    ],
                    "optional": [
                        "account_id",
                        "caller_id_numbers",
                        "carrier",
                        "description",
                        "direction",
                        "internal_rate_cost",
                        "iso_country_code",
                        "options",
                        "rate_increment",
                        "rate_minimum",
                        "rate_name",
                        "rate_nocharge_time",
                        "rate_suffix",
                        "rate_surcharge",
                        "rate_version",
                        "ratedeck_id",
                        "routes",
                        "weight"
                    ]
                }
            }
        }
    }
}
```

You can see `prefix` and `rate_cost` are mandatory fields and must be in the CSV header line. You can also name columns after the optional fields.

!!! note
    The `direction` field defaults to both `inbound` and `outbound`, meaning calls in and out will be rated accordingly. Alternatively, you can create a `direction` column in your CSV to specify the direction.

* Create the task (upload the ratedeck):

```shell
curl -v -X PUT \
-H "X-Auth-Token: {AUTH_TOKEN}" \
-H "Content-type: text/csv" \
--data-binary @rates.csv \
'http://{SERVER}:8000/v2/tasks?category=rates&action=import'
```
```json
{
  "auth_token": "{AUTH_TOKEN}",
  "data": {
    "_read_only": {
      "account_id": "{ACCOUNT_ID}",
      "action": "import",
      "auth_account_id": "{ACCOUNT_ID}",
      "category": "rates",
      "created": 63693042154,
      "id": "{TASK_ID}",
      "status": "pending",
      "total_count": 5
    }
  },
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "timestamp": "{TIMESTAMP}",
  "version": "4.2.29"
}
```

* Start the task (begin importing rates):

```shell
curl -v -X PATCH \
-H "X-Auth-Token: {AUTH_TOKEN}" \
'http://{SERVER}:8000/v2/tasks/{TASK_ID}'
```
```json
{
  "auth_token": "{AUTH_TOKEN}",
  "data": {
    "_read_only": {
      "account_id": "{ACCOUNT_ID}",
      "action": "import",
      "auth_account_id": "{ACCOUNT_ID}",
      "category": "rates",
      "created": 63693042154,
      "id": "{TASK_ID}",
      "node": "{VM}@{HOSTNAME}",
      "start_timestamp": 63693042179,
      "status": "executing",
      "total_count": 101914
    }
  },
  "node": "{NODE_HASH}",
  "page_size": 1,
  "request_id": "{REQUEST_ID}",
  "revision": "automatic",
  "status": "success",
  "timestamp": "{TIMESTAMP}",
  "version": "4.2.29"
}
```

!!! note
    If the rate exists already (based on `prefix`, `iso_country_code`, and `rate_suffix` if present), it will be updated with the new value(s) in the CSV.

* Query the task's status:

```shell
curl -v -X GET \
   -H "X-Auth-Token: {AUTH_TOKEN}" \
   'http://{SERVER}:8000/v2/tasks/{TASK_ID}'
```

When `data.status` changes from `executing`, the task is completed.

Once the rate import is done, check out the [rates API](../../crossbar/doc/rates.md) to see how to rate a DID via the API.

!!! note
    By default, there is a generous pause built into the system to avoid overloading the system. You can speed up task processing by decreasing the pause, in milliseconds (at the expense of more database load): `sup kapps_config set_default tasks wait_after_row_ms 100`

#### Sample CSV

As noted before, query the API to see what fields must be defined and what fields are optional:

```shell
curl -v \
-H "X-Auth-Token: {AUTH_TOKEN}" \
'http://{SERVER}:8000/v2/tasks?category=rates&action=import'
```

```json
{
    "data": {
        "tasks": {
            "rates": {
                "import": {
                    "description": "Bulk-import rates to a specified ratedeck",
                    "doc": "Creates rates from file",
                    "expected_content": "text/csv",
                    "mandatory": [
                        "prefix",
                        "rate_cost"
                    ],
                    "optional": [
                        "account_id",
                        "caller_id_numbers",
                        "carrier",
                        "description",
                        "direction",
                        "internal_rate_cost",
                        "iso_country_code",
                        "options",
                        "rate_increment",
                        "rate_minimum",
                        "rate_name",
                        "rate_nocharge_time",
                        "rate_suffix",
                        "rate_surcharge",
                        "rate_version",
                        "ratedeck_id",
                        "routes",
                        "weight"
                    ]
                }
            }
        }
    }
}
```

The CSV must then define a header row with at least the mandatory fields defined.

```csv
"prefix","rate_cost","rate_name"
1,0.1,"US/Canada Default"
1415,0.05,"San Francisco"
```

Prefixes will match against the E164 number with the `+` stripped. Longest prefix matched wins.

Any fields not in the **mandatory** or **optional** list will be ignored. Order of the columns is irrelevant.

### Export

Export all rates into CSV.

* Start the export

```shell
curl -v -X PUT \
   -H "X-Auth-Token: {AUTH_TOKEN}" \
   'http://{SERVER}:8000/v2/tasks/?category=rates&action=export'
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "_read_only": {
            "account_id": "{ACCOUNT_ID}",
            "action": "export",
            "auth_account_id": "{AUTH_ACCOUNT_ID}",
            "category": "rates",
            "created": 63652613701,
            "id": "{TASK_ID}",
            "status": "pending"
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "undefined",
    "status": "success"
}
```

* Start the export task:

```shell
curl -v -X PATCH \
-H "X-Auth-Token: {AUTH_TOKEN}" \
'http://{SERVER}:8000/v2/tasks/{TASK_ID}'
```

* Save the CSV

Once the task has completed, save the CSV locally:

```shell
curl -v -X GET \
   -o export.csv
   -H "Accept: text/csv"
   -H "X-Auth-Token: $AUTH_TOKEN"
   'http://{SERVER}:8000/v2/tasks/{TASK_ID}?csv_name=out.csv'
```

### Delete

Delete rates for prefixes in CSV file. `prefix` is a mandatory field.

The rate will be deleted only if all defined fields in CSV file match the appropriate keys in rate document. An undefined field in CSV file means "match any".
