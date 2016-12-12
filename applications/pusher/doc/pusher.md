
# Pusher
pusher app allows kazoo to send a push message to a device when the device is the target of a bridge call and is not registered, so that the device can "wake up", register and receive the call.

pusher listens for reg_success messages, checks if the user-agent supports push messages and updates the endpoint_id with a pusher object used in the construction of a failover endpoint for that device.

freeswitch will send the failover to kamailio, kamailio uses kazoo_query to call pusher to send the real push message, waits for the registration and then completes the call.

## Configuration

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
