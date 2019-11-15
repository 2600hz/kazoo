
# knm_voxbone integration

## Documentation
* [voxbone Inventory API Reference](https://docs.voxbone.com/#/reference/inventory)
* [voxbone Invetory Ordering API Reference](https://docs.voxbone.com/#/reference/ordering/get-update-or-delete-carts/list-number-inventory)

## system_config
#### Configuring number manager

To enable kazoo number manager to use the voxbone module you will need to first update the `number_manager` document in the `system_config` database.

In the configuration section that applies to your crossbar servers (or the default) add `"knm_voxbone"` to the `carrier_modules` parameter.

For example:

```json
       "carrier_modules": [
           "knm_local",
           "knm_inventory",
           "knm_managed",
           "knm_reserved",
           "knm_reserved_reseller"
           "knm_voxbone"
       ]
```

The voxbone integration requires making multiple requests to the customer portal so you experience search timeouts you may want to tune the `number_search_timeout_ms` value in `number_manager` document as well.

```json
    "number_search_timeout_ms": 10000,
```

### Configuring the voxbone module
```json
{
    "default": {
        "username": "{USERNAME}",
        "password": "{PASSWORD}",
        "api_key": "{API KEY}",
        "environment": "production | sandbox | beta",
        "page_size": 20
    },
    "pvt_type": "config",
    "pvt_account_id": "system_config",
    "pvt_account_db": "system_config"
}
```

## Integration
### Environments

#### Production
Interacts with the production Voxbone platform. Uses Basic Authentication.

#### Sandbox
Intended for simulating ordering and address verification specifically. You can test ordering without being charged and the sandbox resets and resyncs to your production data each night.

#### Beta(Coming Soon)
Intended for new and upcoming functionality on Voxbone platform. APIs coming soon are : Emergency Service Activation, Number Porting, real-time CDRs. These endpoints use key based authentication.


#### URIs

BaseURL = https://{ENVIRONMENT_URI}/ws-voxbone/services/rest/

* Production - api.voxbone.com
* Sandbox - sandbox.voxbone.com
* Beta - beta.voxbone.com

#### Sample Request

```shell
curl -u username:password -H "Content-type: application/json" -H "Accept: application/json" "https://api.voxbone.com/ws-voxbone/services/rest/inventory/country?countryCodeA3=BEL&pageNumber=0&pageSize=1"

```


### Error Codes
#### HTTP Error Codes

voxbone rate limits to 20 req/s by IP Address and will return one of the following codes on limit:

* 509 - Bandwidth Exceeded
* 429 - Too Many Requests
