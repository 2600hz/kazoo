# Rates

Manage ratedeck DB.
Category - "rates".
Only superadmin can use this module.

## Fields

CSV files for all actions use the same list of fields. Name of field match the name of keys in CouchDB rate document.

Available fields:
* `account_id` - reseller's account (see **Note 1** bellow)
* `description` - description for rate
* `direction` - direction of call leg ("inbound", "outbound"), if not set - rate matches both directions
* `iso_country_code` - [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1#Officially_assigned_code_elements) code for prefix's country
* `prefix` - prefix for match DID number
* `pvt_rate_cost` - internal rate cost, used for `weight` calculation
* `pvt_rate_surcharge` - internal rate surcharge
* `rate_cost` - per minute cost
* `rate_increment` - billing "steps" for rate
* `rate_minimum` - minimum call duration
* `rate_name` - short name for rate, if this field not set it will be generated from `prefix`, `iso_country_code` and `direction` fields
* `rate_nocharge_time` - "free" call time, if call duration less then this value (seconds), then call not charged
* `rate_surcharge` - charge amount on connect (answer)
* `rate_version` - rate version
* `ratedeck_name` -  ratedeck name, assigned to account via service plan
* `weight` - when found several rates with same prefix, used rate with higher weight. If not set - calculated from `prefix` length and `rate_cost` (`pvt_rate_cost`)

**Note 1**: for `import` & `delete` actions, value of `account_id` from CSV file will be ignored, value for this field is taken from task Account-ID .

**Note 2**: `routes` and `options` fields can not be defined via CSV file (because their values are lists).
`routes` is automatically generated from `prefix`. Example: `prefix` 380 generates `routes` - `["^\\+?380.+$"]`.

## Actions

### Import

Import rates from CSV.
`prefix` and `rate_cost` are mandatory fields.

```shell
curl -v -X PUT \
-H "X-Auth-Header: {AUTH_TOKEN}" \
-H "Content-type: text/csv" \
--data-binary @rates.csv \
'http://{SERVER}:8000/v2/tasks?category=rates&action=import'
```

### Export

Export all rates into CSV.
No input file.

### Delete

Delete rates for prefixes in CSV file.
`prefix` is mandatory field.
Rate deleted only if all defined fields in CSV file match appropriate keys in rate document. Undefined field in CSV file means "match any".
