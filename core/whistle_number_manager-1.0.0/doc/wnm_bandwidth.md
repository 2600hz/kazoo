### Whistle Number Manager Bandwidth

The whistle number manager Bandwidth module can be used to integrate Kazoo with http://www.bandwidth.com/

#### Configuring number manager

To enable whistle number manager to use the Bandwidth module you will need to first update the number_manager document in the system_config database.

In the configuration section that applies to your crossbar servers (or the default) add "wnm_bandwidth" to the carrier_modules parameter.

For example:
```
       "carrier_modules": [
           "wnm_local",
           "wnm_bandwidth"
       ]
```

Optionally, you can also set the port in default module to Bandwidth if your port requests are primarily processed by Bandwidth.  This will cause numbers in completed port in requests to be associated with the Bandwidth module.
```
       "porting_module_name": "wnm_bandwidth"
```

#### Configuring the Bandwidth module

Once the module is enabled in number manager the first requests to it will generate a new configuration document called "number_manager.bandwidth" in the system_config database.

This document has the following parameters.

* numbers_api_url: This is the API URL to use when issuing activation or standard number searches to Bandwidth.  There is little reason to change this from the default.
* developer_key: This is the API key provided to you from Bandwidth to access the "numbers_api_url" above.  You must update this for the module to work properly.
* debug: If this parameter is set to 'true' then each request and reply from Bandwidth will be written as a text file to /tmp/bandwidth.log for developers.
* enable_provisioning: If this parameter is set to 'true' then the ability to active numbers will be enabled.  If set to 'false' then the module will only perform number searches.
* sandbox_provisioning: This parameter is intended to be used with 'enable_provisioning'.  If set to 'true' then all number provisioning will pretend to be successful without actually activating.
* order_name_prefix: This is an arbitrary string that is added to the "order-id" of a number provisioning request to bandwidth to identify which numbers where purchased by the Kazoo system.
* endpoints: This is an array of IP addresses that should be provided in the number provisioning request to bandwidth.  It should be the available Kamailio servers in the zone (or cluster if the zone is not redundant).
* tollfree_login: This is an optional parameter, if you want to be able to search for tollfree numbers (and purchase them) then this needs to be your username from "my.bandwidth.com".
* tollfree_password: This is an optional parameter, if you want to be able to search for tollfree numbers (and purchase them) then this needs to be your password from "my.bandwidth.com".

Example:
```
{
   "_id": "number_manager.bandwidth",
   "default": {
       "numbers_api_url": "https://api.bandwidth.com/public/v2/numbers.api",
       "developer_key": "XXXXXX",
       "debug": false,
       "sandbox_provisioning": false,
       "enable_provisioning": true,
       "order_name_prefix": "East coast Kazoo",
       "endpoints": [
           "127.0.0.1",
           "127.0.0.2"
       ],
       "tollfree_login": "XXX@XXX.XXX",
       "tollfree_password": "XXX"
   }
}

```

#### Tollfree Support

Currently, tollfree searching and purchasing utilizes an older Bandwidth API.  This was due to missing functionality in the prior version of their API set and as such the search will take significantly longer than typical number searching.  However, Bandwidth does have a new API that we are migrating to and should resolve this issue (it will likely deprecate tollfree_login and tollfree_password).
