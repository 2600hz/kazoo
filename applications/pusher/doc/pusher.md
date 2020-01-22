
# Pusher
pusher app allows kazoo to send a push message to a device when the device is the target of a bridge call, so that the device can "wake up", register and receive the call.

pusher listens for reg_success messages, checks if the user-agent supports push messages and updates the endpoint_id with a pusher object used in the construction of an endpoint for that device (or a failover endpoint in case of an unregistered device).

freeswitch will send the failover to kamailio, kamailio uses kazoo_query to call pusher to send the real push message, waits for the registration and then completes the call.

if the device is already registered, and the client is alive, kamailio will allow the SIP transaction to continue and the call will be handled as usual

## Quick Start

### What you need
#### 1. Apple cert in PEM file format (text) and google / firebase push authentication token.  Verify those credentials with third-party push test web site.. i.e. www.pushtry.com

You can also use the following curl command to test apple push.

```curl
curl -v \
-d '{"aps":{"alert":"text","sound":"default"}}' \
-H "apns-topic:[app-id]" \
-H "apns-expiration: 1" \
-H "apns-priority: 10" \
--http2 \
--cert voip_pushcert.pem \
https://api.push.apple.com/3/device/[devicetoken]
```

#### 2. Application IDs associated with cert / token
  example: *org.myorg.myapp*.

#### 3. User-Agent header which is sent by the Application in *REGISTER* message.
  example: *MyApp iOS 1.0* 

### Configuration

#### 1. Start Application
 * `sup kapps_controller start_app pusher`
 
#### 2. Configure Kamailio
 * Enable *PUSHER-ROLE*  in `local.cfg` and restart kamailio

#### 3. Register your application
 * `sup pusher_maintenance add_apple_app org.myorg.myapp /tmp/myapp.pem`

#### 4. Update User-Agent Mapping

 * `sup kapps_config set_json pusher User-Agents.MyApp '{"regex":"^MyApp","properties":{"Token-App":"app-id","Token-Type":"pn-type","Token-ID":"pn-tok"}}'`
 
 You can also set these one at a time
 
* `sup kapps_config set_default pusher User-Agents.MyApp.regex ^MyApp`
* `sup kapps_config set_default pusher User-Agents.MyApp.properties.Token-App app-id`
* `sup kapps_config set_default pusher User-Agents.MyApp.properties.Token-Type pn-type`
* `sup kapps_config set_default pusher User-Agents.MyApp.properties.Token-ID pn-tok`

#### 5. Open your Application and REGISTER the device

Sample REGISTER message

```
REGISTER sip:sip.kazoo.io SIP/2.0
...
Contact: <sip:user@192.168.1.1>;reg-id=1;app-id=*org.myorg.myapp*;pn-tok=*token*;pn-type=*apple*
User-Agent: MyApp iOS 1.0
...
```

The device document will be updated with a `pusher` object with the collected properties

#### 6. Make a Call

Unregister the device, and make a call from another device that will be delivered to your app.

## Configuration

### System Config

* `User-Agents`: list of user agents to check for pusher properties.

```
 "User-Agents": {
       "MyApp": {
           "regex": "^MyApp",
           "properties": {
               "Token-App": "app-id",
               "Token-Type": "pn-type",
               "Token-ID": "pn-tok"
           }
       }
   }
```

The properties identify the *fields*  in the contact header where pusher looks for the value, the following properties are mandatory.
   * Token-App
   * Token-Type
   * Token-ID

### Maintenance

In order for the push services from Apple / Firebase to work they need to be configured with application secrets / certificates. The app used in the push message is taken from Token-App.

* The Apple cert needs to be in PEM format (text, no private key encryption)

* `sup pusher_maintenance add_firebase_app AppId Secret`
* `sup pusher_maintenance add_apple_app AppId CertFile` (uses the default APNs host: api.push.apple.com)
* `sup pusher_maintenance add_apple_app AppId CertFile Host` (uses a custom APNs host, i.e. api.development.push.apple.com)
