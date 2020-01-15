
# Pusher
pusher app allows kazoo to send a push message to a device when the device is the target of a bridge call, so that the device can "wake up", register and receive the call.

pusher listens for reg_success messages, checks if the user-agent supports push messages and updates the endpoint_id with a pusher object used in the construction of an endpoint for that device (or a failover endpoint in case of an unregistered device).

freeswitch will send the failover to kamailio, kamailio uses kazoo_query to call pusher to send the real push message, waits for the registration and then completes the call.

if the device is already registered, and the client is alive, kamailio will allow the SIP transaction to continue and the call will be handled as usual

## What you need

1. Apple cert in PEM file format (text) and google / firebase push authentication token.  Verify those credentials with third-party push test web site.. i.e. www.pushtry.com

You can also use the following curl command to test apple push.

curl -v \
-d '{"aps":{"alert":"text","sound":"default"}}' \
-H "apns-topic:[app-id]" \
-H "apns-expiration: 1" \
-H "apns-priority: 10" \
--http2 \
--cert voip_pushcert.pem \
https://api.push.apple.com/3/device/[devicetoken]

* GCM is deprecated. FCM is used for Android app push. pn-type=firebase should be used instead pn-type=google


2. Mobile app IDs associated with cert / token

3. User-Agent header which is used by the mobile app. i.e. MyMobileApp, PushApp, etc.. It must be able to be recognized in REGISTER message.

4. App ID used in both Apple and Google app development. i.e com.app.mymobileapp  This value shows up as 'app-id'

Sample Contact header in REGISTER message

Contact: <sip:user_1234567@192.168.1.1:61767;transport=TCP;ob>;reg-id=1;+sip.instance="<urn:uuid:00000000-0000-0000-0000-00004e05965e>";app-id=com.app.mymobileapp;pn-tok=34fretrwgtrvtrebyenbytrwvtrebytenytbyrtebytebytebyten yhtevtrw;pn-type=apple

## Configuration

### System Config

1. start pusher and enable pusher role in kamailio
../kamailio/local.cfg

2. Initialize pusher app.

[root@apps001 ~]# sup kapps_controller start_app pusher
{ok,[fcm,gcm,base64url,gun,apns,pusher]}
[root@apps001 ~]# sup kapps_controller running_apps
[blackhole,callflow,cdr,conference,crossbar,fax,hangups,media_mgr,milliwatt,omnipresence,pivot,pusher,registrar,reorder,stepswitch,sysconf]

pusher doc in system_config will be created.


3. Add Apple and google/firebase credentials into pusher doc using sup commands. The Apple cert needs to be in PEM (text, no encryption) format and should be located in the server.

sup pusher_maintenance add_apple_app  [app-id] /etc/kamailio/cert/[voip_qa].pem api.development.push.apple.com
sup pusher_maintenance add_apple_app [app-id] /etc/kazoo/kamailio/certs/[voip_pushcert].pem

sup pusher_maintenance add_google_app [app-id] [firebase token]






