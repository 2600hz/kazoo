### Kazoo Number Manager Bandwidth (NEW API)

The kazoo number manager Bandwidth2 module can be used to integrate Kazoo with http://www.bandwidth.com/ using the new api.

#### Configuring number manager

To enable kazoo number manager to use the Bandwidth module you will need to first update the `number_manager` document in the `system_config` database.

In the configuration section that applies to your crossbar servers (or the default) add `"knm_bandwidth2"` to the `carrier_modules` parameter.

For example:

```json
       "carrier_modules": [
           "knm_local",
           "knm_bandwidth2"
       ]
```

Optionally, you can also set the `port in` default module to Bandwidth if your port requests are primarily processed by Bandwidth.
This will cause numbers in completed port in requests to be associated with the Bandwidth module.

```json
       "port_in_module_name": "knm_bandwidth2"
```

#### Configuring the Bandwidth module

Once the module is enabled in number manager the first requests to it will generate a new configuration document called `"number_manager.bandwidth2"` in the `system_config` database.

This document has the following parameters.

* `account_id`: this is the account-id that is provided by bandwidth for your account
* `api_username`: username to log into the api with
* `api_password`: password to log into the api with
* `site_id`: the site-id to use when purchasing numbers
* `sip_peer`: the sip peer to use when purchasing numbers
* `debug`: If this parameter is set to `true` then each request and reply from Bandwidth will be written as a text file to /tmp/bandwidth.log for developers.
* `enable_provisioning`: If this parameter is set to `true` then the ability to active numbers will be enabled.  If set to `false` then the module will only perform number searches.
* `sandbox_provisioning`: intended to be used with `enable_provisioning`.  If set to `true` then all number provisioning will pretend to be successful without actually activating.
* `order_name_prefix`: an arbitrary string that is added to the `"order-id"` field of a number provisioning request to bandwidth to identify which numbers where purchased by the Kazoo system.

Example:

```json
{
   "_id": "number_manager.bandwidth2",
   "default": {
       "account_id": "20902288",
       "api_username": "XXXXXX",
       "api_password": "XXXXXX",
       "site_id": "1234",
       "sip_peer": "234222",
       "debug": false,
       "sandbox_provisioning": false,
       "enable_provisioning": true,
       "order_name_prefix": "East coast Kazoo",
   }
}
```

#### Obtaining Site-ID and Sip-Peer

There are sup commands that will list the Site-ID's and Sip-Peers that are associated with your account to enable you to more easily find these values.

Once you have correctly configured the `api_username`, `api_password`, and `account_id` you may use the following sup commands:

```shell
sup knm_bandwidth2 sites
```

This will list all sites for your account by ID and Name. Then you may use:

```shell
sup knm_bandwidth2 peers {SITE_ID}
```

To list all sip_peers that are configured for a given site.
