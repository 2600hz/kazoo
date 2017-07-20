
# Pusher
pusher app allows kazoo to send a push message to a device when the device is the target of a bridge call, so that the device can "wake up", register and receive the call.

pusher listens for reg_success messages, checks if the user-agent supports push messages and updates the endpoint_id with a pusher object used in the construction of an endpoint for that device (or a failover endpoint in case of an unregistered device).

freeswitch will send the failover to kamailio, kamailio uses kazoo_query to call pusher to send the real push message, waits for the registration and then completes the call.

if the device is already registered, and the client is alive, kamailio will allow the SIP transaction to continue and the call will be handled as usual

## Configuration

### Erlang VM

For APNs (Apple Push Notification service), the configuration file for the Erlang VM running the pusher application must include some configuration parameters. These parameters are required by the apns4erl dependency. See the [apns4erl Github](https://github.com/inaka/apns4erl).

```
[
 {apns, [{apple_host,       "api.development.push.apple.com"} %% or "api.push.apple.com" for production
        ,{apple_port,       443}
        ,{certdata,         <<"">>} %% blank default required, loaded on-the-fly in pm_apple
        ,{keydata,          <<"">>} %% blank default required, loaded on-the-fly in pm_apple
        ,{timeout,          10000}

        ,{apns_id,          'undefined'}
        ,{apns_expiration,  0}
        ,{apns_priority,    10}
        ,{apns_topic,       "com.example.app.voip"} %% app bundle ID + .voip (PushKit)
        ,{apns_collapse_id, 'undefined'}
        ]
 }
 ,...
]
```

### System Config

* `modules` : list of modules to load on app start.

```
"modules": [
       "pm_apple",
       "pm_google"
   ],

```

* `User-Agents`: list of user agents to check for pusher properties.

```
 "User-Agents": {
       "Linphone": {
           "regex": "^Linphone",
           "properties": {
               "Token-App": "app-id",
               "Token-Type": "pn-type",
               "Token-ID": "pn-tok"
           }
       }
   }
```

### Maintenance
in order for the push services from apple / google to work they need to be configured with application secrets / certificates. the app used in the push message is taken from Token-App.

* `sup pusher_maintenance add_google_app(AppId, Secret)`
* `sup pusher_maintenance add_apple_app(AppId, CertFile)`
