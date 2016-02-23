/*
Section: HAPROXY
Title: HAPROXY
Language: en-US
*/

# HAPROXY role in smtp to fax
if you have more than one whapps node running the fax application, you may want to distribute the load from smtp among the several nodes.

```
# smtp configuration simple configuration for haproxy
listen kazoo-fax-smtp
 bind *:25
 mode tcp
  no option http-server-close
  maxconn 50
  log global
  option tcplog
  timeout client 1m
  timeout server 1m
  timeout connect 5s
 balance roundrobin

 server whapps-01 whapps-01:19025 check check-send-proxy send-proxy
 server whapps-02 whapps-02:19025 check check-send-proxy send-proxy
 server whapps-03 whapps-03:19025 check check-send-proxy send-proxy
 server whapps-04 whapps-04:19025 check check-send-proxy send-proxy
```
# haproxy version
at least haproxy-1.5-dev23 (check-send-proxy send-proxy)
