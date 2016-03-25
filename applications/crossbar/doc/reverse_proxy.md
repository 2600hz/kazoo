
### Working with reverse proxies (HAProxy, Nginx, etc)

#### Configuring list allowed proxies
Set list in `system_config/crossbar`, key `reverse_proxies`.
Values can be:
- single ip address ("192.168.0.1");
- single ip address in CIDR notation ("192.168.0.1/32")
- network in CIDR notation ("192.168.0.0/24")

#### Configuration reverse proxy
Reverse proxy must set "X-Forwarded-For" header.
- [HAProxy](https://cbonte.github.io/haproxy-dconv/configuration-1.6.html#4-option%20forwardfor)
- [Nginx](https://www.nginx.com/resources/admin-guide/reverse-proxy/) + add `proxy_set_header X-Forwarded-For $remote_addr;`
- [Apache 2.2](https://httpd.apache.org/docs/2.2/mod/mod_proxy.html)
