# Pusher

Pusher app allows kazoo to send a push message to a device when the device is the target of a bridge call, so that the device can "wake up", register and receive the call.

Pusher listens for `reg_success` messages, checks if the user-agent supports push messages and updates the `endpoint_id` with a pusher object used in the construction of an endpoint for that device (or a failover endpoint in case of an unregistered device).

FreeSWITCH will send the failover to Kamailio, Kamailio uses kazoo_query to call pusher to send the real push message, waits for the registration and then completes the call.

If the device is already registered, and the client is alive, Kamailio will allow the SIP transaction to continue and the call will be handled as usual

## Configuration

### System Config

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

In order for the push services from Apple / Firebase to work they need to be configured with application secrets / certificates. The app used in the push message is taken from Token-App.

* `sup pusher_maintenance add_firebase_app(AppId, Secret)`
* `sup pusher_maintenance add_apple_app(AppId, CertFile)` (uses the default APNs host: api.push.apple.com)
* `sup pusher_maintenance add_apple_app(AppId, CertFile, Host)` (uses a custom APNs host, i.e. api.development.push.apple.com)
