### Kazoo Number Manager VoIP Innovations

The kazoo number manager VoIP Innovations module can be used to integrate Kazoo with http://voipinnovations.com/

#### Configuring number manager

To enable kazoo number manager to use the VoIP Innovations module you will need to first update the `number_manager` document in the `system_config` database.

In the configuration section that applies to your crossbar servers (or the default) add `"knm_voip_innovations"` to the carrier_modules parameter.

For example:

```json
       "carrier_modules": [
           "knm_local",
           "knm_voip_innovations"
       ]
```

#### Configuring the VoIP Innovations module

Once the module is enabled in number manager the first requests to it will generate a new configuration document called `"number_manager.voip_innovations"` in the system_config database.

This document has the following parameters.

* `login`: your VoIP Innovations login (string).
* `password`: your VoIP Innovations secret (string).
* `endpoint_group`: a usable endpoint group tied to your VoIP Innovations account (integer as a string).
* `debug`: when set to `true`, requests & responses will be written to /tmp/voipinnovations.xml (for developers).
* `sandbox_provisioning`: when set to `true`, provisioning of numbers only target VoIP Innovations' sandbox environment.
* `enable_provisioning`: when set to `false`, API calls will not reach VoIP Innovations but will return an error instead.

Example:

```json
{
   "_id": "number_manager.voip_innovations",
   "default": {
       "login": "XXXXX",
       "password": "YYYYY",
       "endpoint_group": "ZZZZZZ",
       "debug": false,
       "sandbox_provisioning": false,
       "enable_provisioning": true
   }
}
```

#### Tollfree Support

VoIP Innovations does not allow to provision tollfree DIDs.
