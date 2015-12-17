# Features

## NkSIP main features are:
* Full support for all curently defined SIP methods: INVITE, ACK, REGISTER, OPTIONS, INFO, UPDATE, PRACK, SUBSCRIBE, NOTIFY, REFER, PUBLISH and MESSAGE, as an UAC, an UAS or and Proxy.
* Can be used to very easily develop any possible SIP application: endpoints, stateful proxies with serial and parallel forking, stateless proxies, B2BUAs, application servers, registrars, SBCs, load generators, etc. 
* Transports UDP, TCP, TLS, SCTP, WS and WSS (websockets) are available, all of them capable of handling thousands of simultaneous sessions.
* Full SIP Event support.
* A written from scratch, fully typed 100% Erlang code with few external dependencies.
* Robust and highly scalable, using all available processor cores automatically.
* More than 150 tests covering nearly all of the functionality.
* Sophisticated plugin mechanism, that adds very low overhead to the core.
* Hot, on-the-fly core and application configuration and code upgrades.
* IPv6 support and IPv4 <-> IPv6 bridge.
* Full support for NAPTR and SRV location, including priority and weights.
* Dialog and SDP processing utility functions, including media start and stop detection.

## Included standard plugins:
* Digest Authentication.
* Registrar Server and Automatic Registrations.
* Event State Compositor.
* Reliable provisional responses.
* Session Timers
* SIP REFER.
* Outbound and GRUU.
* Trace and Statistics.

## Implemented RFCs
In the current version the following RFCs are fully implemented (see notes):

RFC|Description|Notes
---|---|---
[RFC2617](http://tools.ietf.org/html/rfc2617)|Digest authentication|
[RFC2782](http://tools.ietf.org/html/rfc2782)|DNS SRV|
[RFC2915](http://tools.ietf.org/html/rfc2915)|DNS NAPTR|
[RFC2976](http://tools.ietf.org/html/rfc2976)|INFO|
[RFC3261](http://tools.ietf.org/html/rfc3261)|SIP 2.0|
[RFC3262](http://tools.ietf.org/html/rfc3262)|Reliable provisional responses|
[RFC3263](http://tools.ietf.org/html/rfc3263)|Locating SIP Services|
[RFC3264](http://tools.ietf.org/html/rfc3264)|Offer/Answer Model|
[RFC3265](http://tools.ietf.org/html/rfc3265)|Event Notification|
[RFC3311](http://tools.ietf.org/html/rfc3311)|UPDATE|
[RFC3326](http://tools.ietf.org/html/rfc3326)|Reason|
[RFC3327](http://tools.ietf.org/html/rfc3327)|Registering Non-Adjacent Contacts|path
[RFC3428](http://tools.ietf.org/html/rfc3428)|MESSAGE|
[RFC3515](http://tools.ietf.org/html/rfc3515)|REFER|
[RFC3581](http://tools.ietf.org/html/rfc3581)|RPort|
[RFC3608](http://tools.ietf.org/html/rfc3608)|Service-Route|
[RFC3903](http://tools.ietf.org/html/rfc3903)|PUBLISH|
[RFC4168](http://tools.ietf.org/html/rfc4168)|SCTP Transport|No TLS-SCTP
[RFC4475](http://tools.ietf.org/html/rfc4475)|Torture Tests|Included in unit tests
[RFC4566](http://tools.ietf.org/html/rfc4566)|SDP|Only parser and generator
[RFC5057](http://tools.ietf.org/html/rfc5057)|Multiple Dialogs|
[RFC5118](http://tools.ietf.org/html/rfc5118)|IPv6 Torture Tests|Included in unit tests
[RFC5389](http://tools.ietf.org/html/rfc5389)|STUN|Basic STUN client and server (no IPv6)
[RFC5626](http://tools.ietf.org/html/rfc5626)|Outbound|
[RFC5627](http://tools.ietf.org/html/rfc5626)|GRUU|
[RFC6026](http://tools.ietf.org/html/rfc6026)|2xx responses|
[RFC6157](http://tools.ietf.org/html/rfc6157)|IPv6 Transition|
[RFC6665](http://tools.ietf.org/html/rfc6665)|Event Notification|Obsoletes 3265. GRUU support pending
[RFC7118](http://tools.ietf.org/html/rfc7118)|SIP over Websockets|Client and server support


