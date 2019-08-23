
# knm_bics integration

## Documentation
* [BICS Inventory API Reference](https://mynumbers.api.bics.com/documentation/#/Cloud%20Communications/MyNumbers%20API#api-Stock-get_available_numbers-0)
* [BICS Number Ordering API Reference](https://mynumbers.api.bics.com/documentation/#/Cloud%20Communications/MyNumbers%20API#api-Order-order_numbers_in_bulk-0)

## system_config
#### Configuring number manager

To enable kazoo number manager to use the bics module you will need to first update the `number_manager` document in the `system_config` database.

In the configuration section that applies to your crossbar servers (or the default) add `"knm_bics"` to the `carrier_modules` parameter.

For example:

```json
       "carrier_modules": [
           "knm_local",
           "knm_inventory",
           "knm_managed",
           "knm_reserved",
           "knm_reserved_reseller",
           "knm_bics"
       ]
```

### Configuring the BICS module
```json
{
    "default": {
        "api_token": "{AUTH_TOKEN}",
        "api_url": "{API URL}"
    },
    "pvt_type": "config",
    "pvt_account_id": "system_config",
    "pvt_account_db": "system_config"
}
```

## Integration

### URIs

By default the base uri is set to the production endpoint, `https://mynumbers.api.bics.com`. 
If you want to work against their sandbox you should configure the url to `https://mynumbers.api.bics.com/sandbox`
