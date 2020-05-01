# Telnyx Carrier Module

## Documentation

* [Telnyx general documentation](https://developers.telnyx.com/docs/api/v1/numbers)

## Configuration

### Configuring number manager

To enable KAZOO to use the telnyx module you will first need to update the `number_manager` document in the `system_config` database.

In the configuration section that applies to your crossbar servers (or the default) add `"knm_telnyx"` to the `carrier_modules` parameter.

For example:

```json
       "carrier_modules": [
           "knm_local",
           "knm_inventory",
           "knm_managed",
           "knm_reserved",
           "knm_reserved_reseller"
           "knm_telnyx"
       ]
```


### Configuring the telnyx module

```json
{
   "_id": "number_manager.telnyx",
   "_rev": "XXXXXXXXXXXXX",
   "default": {
       "debug": "true",
       "enable_provisioning": "true",
       "monthly_recurring_cost": "1",
       "sandbox_provisioning": "true",
       "should_filter_rates": "false",
       "should_keep_best_effort": "false",
       "token": "pasteyourtoken here",
       "upfront_cost": "false",
       "user": "emailaddress here"
   }
}
```

!!! note
    Only set debug to true if u want to debug, creates a telnyx debug file on your server /tmp

Kazoo defaults to area codes with a minimum length of 3 numbers, and its done in the database schemas, so if you search for numbers with fewer digits, it will give an error.

One workaround is to set the area code `minLength` property to `1` in `find_numbers.json` in `crossbar/priv/couchdb/schemas` on your server and then run `sup crossbar_maintenance update_schemas` to persist it to the db.
