
# HAPROXY role in smtp to fax
if you have more than one kapps node running the fax application, you may want to distribute the load from smtp among the several nodes.

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
  option tcp-check
  tcp-check send NOOP\r\n
  tcp-check expect string 250 Ok\r\n
 balance roundrobin

 server kapps-01 kapps-01:19025 check check-send-proxy send-proxy
 server kapps-02 kapps-02:19025 check check-send-proxy send-proxy
 server kapps-03 kapps-03:19025 check check-send-proxy send-proxy
 server kapps-04 kapps-04:19025 check check-send-proxy send-proxy
```
# haproxy version
at least haproxy-1.5-dev23 (check-send-proxy send-proxy)
