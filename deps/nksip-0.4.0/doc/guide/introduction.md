Introduction
============

NkSIP is an Erlang SIP framework or _application server_, which greatly facilitates the development of robust and scalable server-side SIP applications like proxy, registrar, redirect or outbound servers and [B2BUAs](http://en.wikipedia.org/wiki/Back-to-back_user_agent).

SIP is the standard protocol related to IP voice, video and remote sessions, supported by thousands of devices, softphones and network operators. It is the basic building block for most current voice or video enabled networks and it is the core protocol of the IP Multimedia Subsytem ([IMS](https://en.wikipedia.org/wiki/IP_Multimedia_Subsystem)). SIP is powerful and flexible, but also very complex to work with. SIP basic concepts are easy to understand, but developing robust, scalable, highly available applications is usually quite hard and time consuming, because of the many details you have to take into account.

NkSIP takes care of much of the SIP complexity, while allowing full access to requests and responses. 

NkSIP allows you to run any number of **SipApps**. To start a SipApp, you define a _name_, a set of _transports_ to start listening on and a **callback module**. Currently you must use [Erlang]("http://www.erlang.org") to develop NkSIP applications (in the near future, it will be possible to use Javascript via node.js, see the [roadmap](../roadmap.md)). 

Once started a SipApp, you can start sending SIP requests, and when your application starts receiving requests, specific functions in the callback module will be called. Each defined callback function has a _sane_ default functionality, so you only have to implement the functions you need to customize. You don't have to deal with transports, retransmissions, authentications or dialog management. All of those aspects are managed by NkSIP in a standard way. In case you need to, you can implement the related callback functions.

NkSIP includes now a poweful **plugin mecahnism**. New functionality (like new RFCs, event support packages, etc.) can be added securely and quickly, without having to modify the core. There is a large number of additional callback functions that plugins can implement to modify NkSIP behaviour beyond what a SipApp is allowed to do.

NkSIP is a pure SIP framework, so it _does not support any real RTP media processing_ it can't record a call, host an audio conference or transcode. These type of tasks should be done with a SIP media server, like [Freeswitch](http://www.freeswitch.org) or [Asterisk](http://www.asterisk.org). However NkSIP can act as a standard endpoint (or a B2BUA, actually), which is very useful in many scenarios: registering with an external server, answering and redirecting a call or recovering in real time from a failed media server.

