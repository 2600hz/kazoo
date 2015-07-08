# Configuration

* [Global configuration options](#global-configuration-options).
* [SipApp configuration options](#sipapp-configuration-options).
* [Reconfiguration](#reconfiguration)

Keep in mind that nstalled plugins can add specific configuration options. See the [plugins documentation](../plugins/README.md).


## Global configuration options

They are defined as standard Erlang environment variables for `nksip` application, and all of them has a default value. SipApps cannot override them.

Name|Type|Default|Comments
---|---|---|---
global_max_calls|`integer()`|100000|Maximum number of simultaneous calls (each different Call-ID counts as a call)
global_max_connections|`integer()`|1024|Maximum number of simultaneous connections NkSIP will accept
dns_cache_ttl|`integer()`|3600|DNS cache TTL (See `nksip_dns`) (secs) (SipApps cannot override)
local_data_path|`string()`|"log"|Path to store UUID files (SipApps cannot override)
sync_call_time|`integer()`|30|Time to wait in sync calls


## SipApp configuration options
See [nksip.erl](../../src/nksip.erl) for types information

### Default configuration options

Can also be defined as standard Erlang environment variables for `nksip` application, but SipApps can override most of them when starting.

Name|Type|Default|Comments
---|---|---|---
allow|`string()`&#124;`binary()`|"INVITE,ACK,CANCEL,BYE,OPTIONS,INFO,UPDATE,SUBSCRIBE,NOTIFY,REFER,MESSAGE"|Default _Allow_ header
supported|`string()`&#124;`binary()`|"path"|Default _Supported_ header
timer_t1|`integer()`|500|Standar SIP T1 timer (msecs)
timer_t2|`integer()`|4000|Standar SIP T2 timer (msecs)
timer_t4|`integer()`|5000|Standar SIP T4 timer (msecs)
timer_c|`integer()`|180|Standar SIP C timer (secs)
udp_timeout|`integer()`|30|Time to remove UDP association if no message has been received (secs)
tcp_timeout|`integer()`|180|Time to disconnect TCP/SSL connection if no message has been received (secs)
sctp_timeout|`integer()`|180|Time to disconnect SCTP associations if no message has been received (secs)
ws_timeout|`integer()`|180|Time to disconnect WS/WSS connections if no message has been received (secs)
dialog_timeout|`integer()`|1800|Time to timeout non-refreshed dialogs (secs)
event_expires|`integer()`|60|Default Expires for events (secs)
nonce_timeout|`integer()`|30|Time a new `nonce` in an authenticate header will be usable (secs)
max_calls|`integer()`|100000|Maximum number of simultaneous calls (each different Call-ID counts as a call)
max_connections|`integer()`|1024|Maximum number of simultaneous connections NkSIP will accept


### Specific configuration options

Key|Type|Default|Description
---|---|---|---
plugins|`[atom()]`|`[]`|List of [plugins](../plugins/README.md) to activate
transports|[Proto&#124;{Proto, Ip}&#124;{Proto, Ip, Port}&#124;{Proto, Ip, Port, Opts}], Proto::`nksip:protocol()`, Ip::`inet:ip_address()`&#124;`string()`&#124;`binary()`&#124;all&#124;all6, Port::`inet:port_number()`&#124;any|`[udp]`|The SipApp can start any number of transports. If an UDP transport is started, a TCP transport on the same IP and port will be started automatically. Use `all` to use _all_ available IPv4 addresses and `all6` for all IPv6 addresses, and `any` to use any available port
certfile|`string()`|"(privdir)/cert.pem"|Path to the certificate file for TLS
keyfile|`string()`|"(privdir)/key.pem"|Path to the key file for TLS
supported|`string()`&#124;`binary()`|(installed plugins)|If present, these tokens will be used in _Supported_ headers instead of the default supported list, for example `"my_token1, mytoken2, 100rel"`
accept|`string()`&#124;`binary()`|"*/*"|If defined, this value will be used instead of default when option `accept` is used
events|`string()`&#124;`binary()`|""|Lists the Event Packages this SipApp supports
from|`user_uri()`|"NkSIP App <sip:user@nksip>"|Default _From_ to use in the requests
route|`user_uri()`|[]|Route (outbound proxy) to use. Generates one or more `Route` headers in every request, for example `<sip:1.2.3.4;lr>, <sip:abcd;lr>` (you will usually append the `lr` option to use _loose routing_)
local_host|auto&#124;`string()`&#124;`binary()`&#124;|auto|Default host or IP to use in headers like _Via_, _Contact_ and _Record-Route_. If set to `auto` NkSIP will use the IP of the transport selected in every case. If that transport is listening on all addresses NkSIP will try to find the best IP using the first valid IP among the network interfaces `ethX` and `enX`, or localhost if none is found
local_host6|auto&#124;`string()`&#124;`binary()`|auto|Default host or IP to use in headers like _Via_, _Contact_ and _Record-Route_ for IPv6 transports. See `local_host` option.
no_100|||If present, forbids the generation of automatic `100-type` responses for INVITE requests
log_level|`debug`&#124;`info`&#124;`notice`&#124;`warning`&#124;`error`|`notice`|Current log level (the [global log level](../reference/log.md) should also be adjusted)


## Reconfiguration

Any SipApp can be reconfigured on the fly. 

Any of the previous parameters can be changed (currently, except for `transports`), and the new options will be used fot the next call.

You can even change the plugin list on the fly, but you must be sure of the effects of such a change.
