/*
Section: Blackhole
Title: Blackhole
Language: en-US
*/

# Blackhole *Realtime HTTP Websocket Events*

## Example Client
~~~
<html>
  <head>
    <script src="//cdnjs.cloudflare.com/ajax/libs/socket.io/0.9.6/socket.io.min.js"></script>
  </head>
  <body>
    <div id="content"></div>
    <script>
      var socket = io.connect('http://192.168.56.111:5555');
      socket.emit("subscribe", { account_id: "4b31dd1d32ce6d249897c06332375d65", auth_token: "7b70f69a2a4976d80bfa0382894d1553", binding: "call.CHANNEL_CREATE.*"});
      socket.emit("subscribe", { account_id: "4b31dd1d32ce6d249897c06332375d65", auth_token: "7b70f69a2a4976d80bfa0382894d1553", binding: "call.CHANNEL_ANSWER.*"});
      socket.emit("subscribe", { account_id: "4b31dd1d32ce6d249897c06332375d65", auth_token: "7b70f69a2a4976d80bfa0382894d1553", binding: "call.CHANNEL_DESTROY.*"});
      socket.emit("subscribe", { account_id: "4b31dd1d32ce6d249897c06332375d65", auth_token: "7b70f69a2a4976d80bfa0382894d1553", binding: "conference.event.*"});

      socket.on("participants_event", function (data) {
        console.log(data);
      });
      socket.on("CHANNEL_CREATE", function (data) {
        console.log(data); // data = EventJObj
      });
      socket.on("CHANNEL_ANSWER", function (data) {
        console.log(data);
      });
      socket.on("CHANNEL_DESTROY", function (data) {
        console.log(data);
      });
    </script>
  </body>
</html>
~~~



## WSS considerations

In order you'd like to secure your websocket connection, you can use HAProxy SSL Termination.

Edit your HAProxy config /etc/kazoo/haproxy/haproxy.cfg:

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


Here is how cert_key.pem should look like:

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
