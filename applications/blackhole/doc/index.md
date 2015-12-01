/*
Section: Blackhole
Title: Blackhole
Language: en-US
*/

# Blackhole *Realtime HTTP Websocket Events*



## Setting up

1. Start the Blackhole Crossbar module
    * `sup whapps_controller restart_app blackhole`
1. Find your Account ID (e.g. `4b31dd1d32ce6d249897c06332375d65`)
1. [Obtain an Auth Token](https://2600hz.atlassian.net/wiki/display/APIs/Generating+an+Authentication+Token) (e.g. `7b70f69a2a4976d80bfa0382894d1553`)
1. Copy the **Example Client** code into an HTML file (named e.g. `kazoo_example_ws_client.html`)
    1. Replace `{BLACKHOLE_IP_ADDRESS}` with your Kazoo server's IP address
    1. Replace default `5555` with the port number you can configure in **sysconfig > blackhole > port** (integer)
    1. Replace the `{ACCOUNT_ID}` and `{AUTH_TOKEN}` fields with your data
1. You're all set!

Now access `kazoo_example_ws_client.html` with your favourite Web browser and open the Javascript console.
* You should be able to see whether your client connected to Kazoo
* Make a call: the events your client is listening for will appear in the console!

From here, you can write your own Javascript callbacks, triggered everytime a registered event is sent from Kazoo.

## Example Client
```html
<html>
  <head>
    <script src="http://cdnjs.cloudflare.com/ajax/libs/socket.io/0.9.6/socket.io.min.js"></script>
  </head>
  <body>
    <script>
        var socket = io.connect('http://{BLACKHOLE_IP_ADDRESS}:5555');

        socket.emit('subscribe', {
            account_id: '{ACCOUNT_ID}',
            auth_token: '{AUTH_TOKEN}',
            binding: 'call.CHANNEL_CREATE.*'
        });

        socket.emit('subscribe', {
            account_id: '{ACCOUNT_ID}',
            auth_token: '{AUTH_TOKEN}',
            bindings: ['call.CHANNEL_ANSWER.*', 'call.CHANNEL_DESTROY.*']
        });

        socket.emit('subscribe', {
            account_id: accountId,
            auth_token: token,
            bindings: ['doc_created.*.user.*', 'doc_edited.*.user.*']
        });

        socket.on('CHANNEL_CREATE', function(EventJObj) {
            console.log(EventJObj);
        });

        socket.on('CHANNEL_ANSWER', function(EventJObj) {
            console.log(EventJObj);
        });

        socket.on('CHANNEL_DESTROY', function(EventJObj) {
            console.log(EventJObj);
        });

        socket.on('doc_created_user', function(EventJObj) {
            console.log(EventJObj);
        });

        socket.on('doc_edited_user', function(EventJObj) {
            console.log(EventJObj);
        });

        // socket.emit('unsubscribe', {
        //     account_id: accountId,
        //     auth_token: token,
        //     bindings: ['doc_created.*.user.*', 'doc_edited.*.user.*']
        // });

        // socket.emit('unsubscribe_all', {
        //     account_id: accountId,
        //     auth_token: token
        // });

        var Events = ['connect', 'error', 'disconnect', 'reconnect', 'reconnect_attempt', 'reconnecting', 'reconnect_error', 'reconnect_failed'];

        for (var idx in Events) {
            socket.on(Events[idx], function(_evt) {
                console.log('=ERROR REPORT==== ' + Events[idx]);
            });
        }
    </script>
  </body>
</html>
```

You can add one or multiple bindings by using:

``` javascript
// For one use: binding
socket.emit('subscribe', {
    account_id: '{ACCOUNT_ID}',
    auth_token: '{AUTH_TOKEN}',
    binding: 'doc_edited.*.user.*'
});

// For multiple use: bindings
socket.emit('subscribe', {
    account_id: '{ACCOUNT_ID}',
    auth_token: '{AUTH_TOKEN}',
    bindings: ['doc_edited.*.user.*', 'doc_deleted.*.user.*']
});
```

You can also add a friendly name and some metadata to any subscribe command.

``` javascript
socket.emit('subscribe', {
    account_id: '{ACCOUNT_ID}',
    auth_token: '{AUTH_TOKEN}',
    name: "My new socket",
    metadata: {
        test: "test"
    },
    binding: 'doc_edited.*.user.*'
});
```


To remove unnecessary bindings use 'unsubscribe' event:

For particular subscription:
```
socket.emit('unsubscribe', { account_id: '{ACCOUNT_ID}', auth_token: '{AUTH_TOKEN}', binding: 'call.CHANNEL_CREATE.*' });
```

For all previous subscriptions:
```
socket.emit('unsubscribe', { auth_token: '{AUTH_TOKEN}' });
```


### The EventJObj data structure

The Blackhole application listens to events from AMQP.
It will send an event to you through Websockets if there is an active binding that matches this event.
To learn more about how they are routed from your Kazoo cluster to this app, [read on on `wh_hook`](https://github.com/2600hz/kazoo/tree/master/core/whistle_apps-1.0.0/src).

Events are plain AMQP event messages.
Here are a few complete `call.*.*` JSON events:
```json
{
    "name": "CHANNEL_CREATE",
    "args": {
        "App-Name": "ecallmgr",
        "App-Version": "0.8.0",
        "Call-Direction": "inbound",
        "Call-ID": "at6dmu4ffk97vnlp6qfq",
        "Caller-ID-Name": "user_wpxnx7am9w",
        "Caller-ID-Number": "user_wpxnx7am9w",
        "Custom-Channel-Vars": {
            "Account-ID": "4b8c6fec4b2597882c0390202d195419",
            "Account-Name": "wefwefwefwef",
            "Account-Realm": "wefwefwefwef.2600hz.com",
            "Authorizing-ID": "d509114b66efabf32dfd78dc464b46dd",
            "Authorizing-Type": "device",
            "Owner-ID": "699ed7e3d583e2577ac017c575f87262",
            "Realm": "wefwefwefwef.2600hz.com",
            "Register-Overwrite-Notify": "false",
            "Suppress-Unregister-Notifications": "false",
            "Username": "user_wpxnx7am9w"
        },
        "Custom-SIP-Headers": {
            "X-AUTH-IP": "192.168.25.203"
        },
        "Disposition": "DELAYED NEGOTIATION",
        "Event-Category": "call_event",
        "Event-Name": "CHANNEL_CREATE",
        "From": "356@wefwefwefwef.2600hz.com",
        "From-Tag": "l530ecbu2f",
        "From-Uri": "user_wpxnx7am9w@wefwefwefwef.2600hz.com",
        "Media-Server": "wef.2600hz.com",
        "Msg-ID": "1443672756993309",
        "Node": "ecallmgr@wef.2600hz.com",
        "Presence-ID": "356@wefwefwefwef.2600hz.com",
        "Request": "*97@wefwefwefwef.2600hz.com",
        "Switch-URI": "sip:192.168.25.216:11000",
        "Switch-URL": "sip:mod_sofia@192.168.25.216:11000",
        "Timestamp": 63610891956,
        "To": "*97@wefwefwefwef.2600hz.com",
        "To-Uri": "*97@wefwefwefwef.2600hz.com"
    }
}
```

```json
{
    "name": "CHANNEL_ANSWER",
    "args": {
        "App-Name": "ecallmgr",
        "App-Version": "0.8.0",
        "Call-Direction": "inbound",
        "Call-ID": "at6dmu4ffk97vnlp6qfq",
        "Caller-ID-Name": "web phone",
        "Caller-ID-Number": "356",
        "Custom-Channel-Vars": {
            "Account-ID": "4b8c6fec4b2597882c0390202d195419",
            "Account-Name": "wefwefwefwef",
            "Account-Realm": "wefwefwefwef.2600hz.com",
            "Application-Name": "callflow",
            "Application-Node": "whistle_apps@wef.2600hz.com",
            "Authorizing-ID": "d509114b66efabf32dfd78dc464b46dd",
            "Authorizing-Type": "device",
            "Bridge-ID": "at6dmu4ffk97vnlp6qfq",
            "Caller-Privacy-Name": "false",
            "Caller-Privacy-Number": "false",
            "Channel-Authorized": "true",
            "Ecallmgr-Node": "ecallmgr@wef.2600hz.com",
            "Fetch-ID": "a60aea8c-67f2-11e5-8465-3390f8e3460f",
            "Global-Resource": "false",
            "Owner-ID": "699ed7e3d583e2577ac017c575f87262",
            "Realm": "wefwefwefwef.2600hz.com",
            "Register-Overwrite-Notify": "false",
            "Suppress-Unregister-Notifications": "false",
            "Username": "user_wpxnx7am9w"
        },
        "Custom-SIP-Headers": {
            "X-AUTH-IP": "192.168.25.203"
        },
        "Disposition": "EARLY MEDIA",
        "Event-Category": "call_event",
        "Event-Name": "CHANNEL_ANSWER",
        "From": "356@wefwefwefwef.2600hz.com",
        "From-Tag": "l530ecbu2f",
        "From-Uri": "user_wpxnx7am9w@wefwefwefwef.2600hz.com",
        "Media-Server": "wef.2600hz.com",
        "Msg-ID": "1443672757935421",
        "Node": "ecallmgr@wef.2600hz.com",
        "Presence-ID": "356@wefwefwefwef.2600hz.com",
        "Request": "*97@wefwefwefwef.2600hz.com",
        "Switch-URI": "sip:192.168.25.216:11000",
        "Switch-URL": "sip:mod_sofia@192.168.25.216:11000",
        "Timestamp": 63610891957,
        "To": "*97@wefwefwefwef.2600hz.com",
        "To-Uri": "*97@wefwefwefwef.2600hz.com"
    }
}
```

```json
{
    "name": "CHANNEL_DESTROY",
    "args": {
        "App-Name": "ecallmgr",
        "App-Version": "0.8.0",
        "Billing-Seconds": "31",
        "Call-Direction": "inbound",
        "Call-ID": "at6dmu4ffk97vnlp6qfq",
        "Caller-ID-Name": "web phone",
        "Caller-ID-Number": "356",
        "Custom-Channel-Vars": {
            "Account-ID": "4b8c6fec4b2597882c0390202d195419",
            "Account-Name": "wefwefwefwef",
            "Account-Realm": "wefwefwefwef.2600hz.com",
            "Application-Name": "callflow",
            "Application-Node": "whistle_apps@wef.2600hz.com",
            "Authorizing-ID": "d509114b66efabf32dfd78dc464b46dd",
            "Authorizing-Type": "device",
            "Bridge-ID": "at6dmu4ffk97vnlp6qfq",
            "Caller-Privacy-Name": "false",
            "Caller-Privacy-Number": "false",
            "Channel-Authorized": "true",
            "Ecallmgr-Node": "ecallmgr@wef.2600hz.com",
            "Fetch-ID": "a60aea8c-67f2-11e5-8465-3390f8e3460f",
            "Global-Resource": "false",
            "Owner-ID": "699ed7e3d583e2577ac017c575f87262",
            "Realm": "wefwefwefwef.2600hz.com",
            "Register-Overwrite-Notify": "false",
            "Suppress-Unregister-Notifications": "false",
            "Username": "user_wpxnx7am9w"
        },
        "Custom-SIP-Headers": {
            "X-AUTH-IP": "192.168.25.203"
        },
        "Disposition": "ANSWER",
        "Duration-Seconds": "32",
        "Event-Category": "call_event",
        "Event-Name": "CHANNEL_DESTROY",
        "From": "356@wefwefwefwef.2600hz.com",
        "From-Tag": "l530ecbu2f",
        "From-Uri": "user_wpxnx7am9w@wefwefwefwef.2600hz.com",
        "Hangup-Cause": "NORMAL_CLEARING",
        "Media-Server": "wef.2600hz.com",
        "Msg-ID": "1443672788333353",
        "Node": "ecallmgr@wef.2600hz.com",
        "Presence-ID": "356@wefwefwefwef.2600hz.com",
        "Remote-SDP": "v=0\r\no=- 7274649247909803147 2 IN IP4 192.168.25.203\r\ns=-\r\nt=0 0\r\na=group:BUNDLE audio\r\na=msid-semantic: WMS CS9KeiNShVWxihCq6BmFZ7r1FrWsts6UfkcQ\r\nm=audio 49382 UDP/TLS/RTP/SAVPF 111 103 104 9 0 8 106 105 13 126\r\nc=IN IP4 192.168.25.203\r\na=rtpmap:111 opus/48000/2\r\na=fmtp:111 minptime=10; useinbandfec=1\r\na=rtpmap:103 ISAC/16000\r\na=rtpmap:104 ISAC/32000\r\na=rtpmap:9 G722/8000\r\na=rtpmap:0 PCMU/8000\r\na=rtpmap:8 PCMA/8000\r\na=rtpmap:106 CN/32000\r\na=rtpmap:105 CN/16000\r\na=rtpmap:13 CN/8000\r\na=rtpmap:126 telephone-event/8000\r\na=rtcp:59374 IN IP4 192.168.25.203\r\na=candidate:889533752 1 udp 2122265343 fde9:8246:57d3:df4f:8d39:2fa0:21a2:cc43 58565 typ host generation 0\r\na=candidate:2922620760 1 udp 2122194687 192.168.25.203 49382 typ host generation 0\r\na=candidate:889533752 2 udp 2122265342 fde9:8246:57d3:df4f:8d39:2fa0:21a2:cc43 49383 typ host generation 0\r\na=candidate:2922620760 2 udp 2122194686 192.168.25.203 59374 typ host generation 0\r\na=candidate:2072431048 1 tcp 1518285567 fde9:8246:57d3:df4f:8d39:2fa0:21a2:cc43 0 typ host tcptype active generation 0\r\na=candidate:3769654184 1 tcp 1518214911 192.168.25.203 0 typ host tcptype active generation 0\r\na=candidate:2072431048 2 tcp 1518285566 fde9:8246:57d3:df4f:8d39:2fa0:21a2:cc43 0 typ host tcptype active generation 0\r\na=candidate:3769654184 2 tcp 1518214910 192.168.25.203 0 typ host tcptype active generation 0\r\na=ice-ufrag:1tev3wX2bSa+rlLj\r\na=ice-pwd:a8xkPKpuvM99mnF4FriMxbkV\r\na=fingerprint:sha-256 F2:BC:FC:0A:9C:F5:7D:87:26:D1:8B:8F:7F:2E:AE:81:40:45:28:DD:51:2B:BD:08:44:EB:6B:69:3A:37:50:93\r\na=setup:actpass\r\na=mid:audio\r\na=extmap:1 urn:ietf:params:rtp-hdrext:ssrc-audio-level\r\na=extmap:3 http://www.webrtc.org/experiments/rtp-hdrext/abs-send-time\r\na=rtcp-mux\r\na=maxptime:60\r\na=ssrc:376706195 cname:5rLP7HpPfGQdTezq\r\na=ssrc:376706195 msid:CS9KeiNShVWxihCq6BmFZ7r1FrWsts6UfkcQ 0ae35b40-7027-412e-91a9-563e853b53b5\r\na=ssrc:376706195 mslabel:CS9KeiNShVWxihCq6BmFZ7r1FrWsts6UfkcQ\r\na=ssrc:376706195 label:0ae35b40-7027-412e-91a9-563e853b53b5\r\na=oldmediaip:127.0.0.1\r\n",
        "Request": "*97@wefwefwefwef.2600hz.com",
        "Ringing-Seconds": "0",
        "Switch-URI": "sip:192.168.25.216:11000",
        "Switch-URL": "sip:mod_sofia@192.168.25.216:11000",
        "Timestamp": 63610891988,
        "To": "*97@wefwefwefwef.2600hz.com",
        "To-Tag": "9tr3FZcKDaNHK",
        "To-Uri": "*97@wefwefwefwef.2600hz.com",
        "User-Agent": "SIP.js/0.6.4"
    }
}
```

### Blackhole bindings

See [the section on Blackhole's bindgins](https://github.com/2600hz/kazoo/tree/master/applications/blackhole/doc/bindings.md).


## WSS considerations

In order you'd like to secure your websocket connection, you can use HAProxy SSL Termination.

Edit your HAProxy config `/etc/kazoo/haproxy/haproxy.cfg`:

~~~
global
        ....
        tune.ssl.default-dh-param 2048
        ....

defaults
        ....
        timeout tunnel 1h
        ....


(add the next sections at the end of the config file)

frontend secure_blackhole
        bind 0.0.0.0:7777 ssl crt /etc/kazoo/haproxy/cert_key.pem
        timeout client 1h
        default_backend www_blackhole
        acl is_websocket hdr(Upgrade) -i WebSocket
        use_backend websocket_blackhole if is_websocket

backend www_blackhole
        mode http
        stats enable
        stats uri /haproxy
        option forwardfor
        reqadd x-forwarded-proto:\ https
        server server1 127.0.0.1:5555 weight 1 maxconn 8192

backend websocket_blackhole
        mode http
        option forwardfor
        option http-server-close
        option forceclose
        no option httpclose
        server server1 127.0.0.1:5555 weight 1 maxconn 8192
~~~


Here is how `cert_key.pem` should look like:

~~~
[root@kz527 ~]# cat /etc/kazoo/haproxy/cert_key.pem
-----BEGIN CERTIFICATE-----
MIIF0jCCBLqgAwIBAgIRAOQQ6+NpkZwOENe2OQiJlW4wDQYJKoZIhvcNAQEFBQAw
..........
LE5OWycye7miZLmgtC6ZkI6HI7KJuIEcfeYaBSpENinOXs0OjvmGBYELgNymAw2L
FG3/ESMR
-----END CERTIFICATE-----
-----BEGIN RSA PRIVATE KEY-----
MIIEpQIBSKDCAQEA0roiYyzi4Auuu2qJ/2uWsmUnNHjKqvWXd6iMf2aNbOKcVVps
..........
V8MsGq2IA+2FmrRrd0jYfh8iu1VydbmySghjs69HtYNPndfhs37HtH0=
-----END RSA PRIVATE KEY-----
~~~


Now you can use 7777 port for your blackhole WSS connections.


Config was created to connect Kazoo-Popup secure and wasn't fully tested,
so treat it as a hint needed to be proved before putting into production.
