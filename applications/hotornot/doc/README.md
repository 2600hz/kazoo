
# Hot or Not *Call rating*

This application serves `rate_req` requests.

Rates stored in "ratedeck" database.  
Each rate is separate doceument:
```JSON
{
    "_id": "f36cac061205f7a0f7fde6b4d3370bf9",
    "_rev": "1-1b67fb507c69c35200178a9b1e0e6cfc",
    "routes": [
        "^\\+?1.+$"
    ],
    "weight": "1",
    "rate_name": "outbound_US_1",
    "pvt_type": "rate",
    "prefix": "1",
    "rate_cost": "1.27",
    "ratedeck_name": "ratedeck_1",
    "iso_country_code": "US",
    "description": "USA prefix 1",
    "direction": "outbound",
    "rate_increment": "60",
    "rate_mnimum": "60",
    "rate_nocharge_time": "0",
    "options": [
        "Opt1",
        "Opt2"
    ]
}
```

Schema for this documents described in [cb_rates](../../crossbar/doc/rates.md) module.

## Configuration

Key | Description | Type | Default
--- | ----------- | ---- | -------
`use_trie` | Use in-memory prefix dictionary for search rates | `boolean` | `false`
`filter_list` | List additional filters (after prefix match) | `array(string)` | `["direction", "route_options", "routes"]`
`filter_list.[]` |  | `string` |
`sort_by_weight` | Sort matched filters by `weight`(`true`) or by `rate_cost` (`false`) | `boolean` | `true`
`default_rate_cost` | Cost used when `rate_cost` is not defined in rate | `float` | `0.0`
`default_rate_surcharge` | Surcharge used when `rate_surcharge` is not defined in rate | `float` | `0.0`
`default_rate_minimum` | Minimum call duration when `rate_minimum` is not defined in rate | `integer` | `60`
`default_rate_increment` | Increment call duration when `rate_increment` is not defined in rate | `integer` | `60`
`default_rate_nocharge_time` | No chanrge time when `rate_nocharge_time`  is not defined in rate | `integer` | `0`

## Trie

## Filters

`filter_list` option is used to define additional filters when matching rates.

### direction

If `direction` filter defined in `filter_list` then "Direction" value from rate request compared with `direction` value in rates. Undefined value in rate meanse "match any direction".

### route_options

If `route_options` filter defined in `filter_list` then "Options", "Outbound-Flags" and resource ID from rate request compared with `options` value in rates. Rate matched when all flags in "Options", "Outbound-Flags" and resource ID exist in `options` list in rate. Empty or undefined `options` list in rate means "match any route options".

### routes

If `routes` filter defined in `filter_list` then "To-DID" from rate request compared with list of regexp's in `routes` value of rates. Rate matched if any of regexp's matched. Empty regexp's list or undefined value in rate means "match nobody" and this rate will never be used.

### ratedeck_name

If `ratedeck_name` filter defined in `filter_list` then ratedeck assigned to account compared with `ratedeck_name` value of rates. Ratedeck name assigned to account via service plan. More info [here](../../../core/kazoo_services/doc/ratedeck.md). If rate request doesn't have "Account-ID" or account doesn't have ratedeck name in their service plan, then it match rates with undefined `ratedeck_name` value.

### reseller

If `reseller` filter defined in `filter_list` then reseller's account ID compared  with `account_id` value of rates. If rate request doesn't have "Account-ID" then it match rates with undefined `account_id` value.

### version

If `version` filter defined in `filter_list` then `rate_version` from `system_config/hotornot` compared with `rate_version` value of rates.
