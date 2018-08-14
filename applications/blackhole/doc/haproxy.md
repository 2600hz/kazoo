# Blackhole HAProxy Configuration

## Configuration for API and Websockets

Combining a HTTP listener for both API and Websockets:

```
listen kazoo-crossbar-https
        bind *:8443 ssl crt /etc/haproxy/certs/mycert.pem
        mode http
        option httplog
        option dontlognull
        acl is_blackhole path_beg /socket.io/
        use_backend kapps-blackhole if is_blackhole
        default_backend kapps-crossbar
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

## Configuring Monster UI

Edit `src/js/config.js` Monster UI file:


```javascript hl_lines="4 5"
define(function(require){
    return {
        api: {
            default: 'https://api.mydomain.com:8443/v2/',
            socket: 'https://api.mydomain.com:8443'
        },
        ...
```
