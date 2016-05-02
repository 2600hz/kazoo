/*
Section: Blackhole
Title: Haproxy
Language: en-US
*/

# Blackhole

## configuring haproxy

combining a http listener for both api and websockets

```
listen kazoo-crossbar-https
        bind *:8443 ssl crt /etc/haproxy/certs/mycert.pem
        mode http
        option httplog
        option dontlognull
        acl is_blackhole path_beg /socket.io/
        use_backend kapps-blackhole if is_blackhole
        default_backend kapps-crossbar
```

```
backend kapps-blackhole
  balance source
    mode http
    option forwardfor
    option http-server-close
    option forceclose
    no option httpclose
    option httpchk HEAD  /

   server srv-01 srv-01.mydomain.com:5555 check
   server srv-02 srv-02.mydomain.com:5555 check
   server srv-03 srv-03.mydomain.com:5555 check
   server srv-04 srv-04.mydomain.com:5555 check

```

## configuring monster-ui

edit js/config.js

```
define(function(require){

    return {
        api: {
            // The default API URL defines what API is used to log in to your back-end
            default: 'https://api.mydomain.com:8443/v2/'

            // If you want to use WebSockets you need to turn blackhole on in the back-end and then put the URL in the 'socket' key below
           , socket: 'https://api.mydomain.com:8443'
        },
        ...
```
