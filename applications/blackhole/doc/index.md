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
    1. Replace `192.168.56.111` with your Kazoo server's IP address
    1. Replace default `5555` with the port number you can configure in **sysconfig > blackhole > port** (integer)
    1. Replace the `account_id` and `auth_token` fields with your data
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
      var socket = io.connect('http://10.1.0.70:5555');
      socket.emit('subscribe', { account_id: '4b8c6fec4b2597882c0390202d195419', auth_token: '53d0af4ae87bee5e5896ca3c98fd2497', binding: 'call.CHANNEL_CREATE.*'});
      socket.emit('subscribe', { account_id: '4b8c6fec4b2597882c0390202d195419', auth_token: '53d0af4ae87bee5e5896ca3c98fd2497', binding: 'call.CHANNEL_ANSWER.*'});
      socket.emit('subscribe', { account_id: '4b8c6fec4b2597882c0390202d195419', auth_token: '53d0af4ae87bee5e5896ca3c98fd2497', binding: 'call.CHANNEL_DESTROY.*'});
      socket.emit('subscribe', { account_id: '4b8c6fec4b2597882c0390202d195419', auth_token: '53d0af4ae87bee5e5896ca3c98fd2497', binding: 'conference.event.*'});

      socket.on('participants_event', function (data) {
        console.log(data);
      });
      socket.on('CHANNEL_CREATE', function (EventJObj) {
        console.log(EventJObj);
      });
      socket.on('CHANNEL_ANSWER', function (EventJObj) {
        console.log(EventJObj);
      });
      socket.on('CHANNEL_DESTROY', function (EventJObj) {
        console.log(EventJObj);
      });


      var Events = ['connect', 'error', 'disconnect', 'reconnect', 'reconnect_attempt', 'reconnecting', 'reconnect_error', 'reconnect_failed'];
      for (idx in Events) {
        socket.on(Events[idx], function (_evt) { console.log('=ERROR REPORT==== ' + Events[idx]); });
      }
    </script>
  </body>
</html>
```

### The EventJObj data structure

The Blackhole application listens to events from AMQP.
It will send an event to you through Websockets if there is an active binding that matches this event.
To learn more about how they are routed from your Kazoo cluster to this app, [read on on `wh_hook`](https://github.com/2600hz/kazoo/tree/master/core/whistle_apps-1.0.0/src).

Events are plain AMQP event messages.
Here is a complete one:
```json

```

### Binding to events

The binding syntax is the same used throughout all Crossbar applications, since it is the same as AMQP's binding syntax.

Here are a non-exhaustive list of bindings provided per default callback module:
* `bh_call`:
    * `call.CHANNEL_CREATE.*`
    * `call.CHANNEL_ANSWER.*`
    * `call.CHANNEL_DESTROY.*`
    * `call.RECORD_STOP.*`
* `bh_conference`:
    * `conference.event.*`
* `bh_fax`:
    * `fax.status.*`

### Writing your own bindings

Blackhole [callback modules](https://github.com/2600hz/kazoo/tree/master/applications/blackhole/src/modules) provide bindings to Kazoo events.
If however you do not find a callback module that provides the bindings you are looking for, you can easily add your own!

1. Copy [bh_skel](https://github.com/2600hz/kazoo/blob/master/applications/blackhole/src/modules/bh_skel.erl) into `bh_mymodule.erl` and have it listen to the events you want
1. Make sure the module name is prefixed by `bh_`
1. Make sure that it resides in the `modules/` directory


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
