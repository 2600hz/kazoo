hep_erlang [![Build Status](https://travis-ci.org/fenollp/hep_erlang.svg?branch=master)](https://travis-ci.org/fenollp/hep_erlang)
==========

An Erlang implementation of HEP (Homer Encapsulation Protocol).

**WARNING**

This project is currently in the incubator of
[Pannonia Technologies](http://www.pannonia-technologies.com), so there maybe
a lot of changes in how this software works or it might not even work at all.
There might be terrible inconsistencies in or a complete lack of documentation.
So you are warned to use this software at your own risk!

If you'd like to contribute, don't ask, just do it. That's one reason we are
here on GitHub.

Goals
-----

HEP was designed by the developers of the
[Homer SIP Capture Server](http://www.sipcapture.org) solution. HEP is used to
transport metadata and SIP messages from a SIP proxy or SIP user agent to a
capture server. HEP is not limited to transport SIP messages, but before
version 3 of the protocol, the payload could not be distinguished by the
metadata, so transporting anything else than SIP before Version 3 is rather
impractical when conveyed through the same channel. A few major Open Source
SIP-related projects, like [FreeSWITCH](http://www.freeswitch.org),
[Kamailio](http://www.kamailio.org) and [OpenSIPS](http://www.opensips.org)
integrated HEP natively, so this is why this project exists and we also wanted
to be able to capture and/or send HEP messages from Erlang. HEP can use any
kind of transport channel, but most of the time UDP is used.


Usage
-----

Very basic example:

```erlang
-module(naive_udp_hep_listener).

-export([start/1, stop/1]).

start(Port) ->
    Listener = fun () ->
                       {ok, Socket} = gen_udp:open(Port, [binary, {active,true}]),
                       loop(Socket)
               end,
    Pid = spawn(Listener),
    {ok, Pid}.

loop (Socket) ->
    receive
        {udp, _, _, _, Message} ->
            {ok, Hep} = hep:decode(Message),
            HepMsg = hep:payload(Hep),
            error_logger:info_msg("~p~n", [HepMsg]),
            loop(Socket);
        stop ->
            gen_udp:close(Socket);
        _ ->
            loop(Socket)
    end.

stop (ListenerPid) ->
    ListenerPid ! stop.
```

Protocol Version 1
------------------

All integer fields are stored in network byte order.

| Name                | Length   | Description                                                                         |
|---------------------|----------|-------------------------------------------------------------------------------------|
| Version             |  8 bits  | HEP version, always 1 for this version.                                             |
| Length              |  8 bits  | Length of the HEP header, including Version and Length field and excluding payload. |
| Protocol Family     |  8 bits  | Protocol Family PF_INET, PF_INET6 (2 = IPv4, 10 = IPv6).                            |
| Protocol            |  8 bits  | UDP, TCP, TLS, SCTP, etc..                                                          |
| Source Port         | 16 bits  | Source Port of the captured packet in payload.                                      |
| Destination Port    | 16 bits  | Destination Port of the captured packet in payload.                                 |
| Source Address      | *1       | Source IP of the captured packet in payload.                                        |
| Destination Address | *1       | Destination IP of the captured packet in payload.                                   |
| Payload             | *2       | Payload (usually SIP message)                                                       |

*1) Length is 32 bits for IPv4 and 128 bits for IPv6.

*2) Variable length not defined by metadata, so this protocol is really only well suited for UDP.

Protocol Version 2
------------------

All integer fields are stored in network byte order.

| Name                | Length   | Description                                                                         |
|---------------------|----------|-------------------------------------------------------------------------------------|
| Version             |  8 bits  | HEP version, always 2 for this version.                                             |
| Length              |  8 bits  | Length of the HEP header, including Version and Length field and excluding payload. |
| Protocol Family     |  8 bits  | Protocol Family PF_INET, PF_INET6 (2 = IPv4, 10 = IPv6).                            |
| Protocol            |  8 bits  | UDP, TCP, TLS, SCTP, etc..                                                          |
| Source Port         | 16 bits  | Source Port of the captured packet in payload.                                      |
| Destination Port    | 16 bits  | Destination Port of the captured packet in payload.                                 |
| Source Address      | *1       | Source IP of the captured packet in payload.                                        |
| Destination Address | *1       | Destination IP of the captured packet in payload.                                   |
| Seconds             | 32 bits  | The timestamp in seconds since the Epoch when the included payload was captured.    |
| Microseconds        | 32 bits  | The microseconds part of the timestamp.                                             |
| Capture ID of node  | 16 bits  | A capture ID of the node. XXX: What does this exactly mean???                       |
| *unused*            | 16 bits  | *unused*                                                                            |
| Payload             | *2       | Payload (usually SIP message)                                                       |

*1) Length is 32 bits for IPv4 and 128 bits for IPv6.

*2) Variable length not defined by metadata, so this protocol is really only well suited for UDP.

Protocol Version 3
------------------

[HEP v3 rev11](http://hep.sipcapture.org/hepfiles/HEP3_rev11.pdf)

Other Software Supporting HEP
-----------------------------

This is third-party software also supporting HEP.

| Name                                            | HEP Versions Supported | Client | Server |
|-------------------------------------------------|------------------------|--------|--------|
| [captagent](http://code.google.com/p/captagent) | 1, 2, 3                | yes    | no     |
| [FreeSWITCH](http://www.freeswitch.org)         | 1                      | yes    | no     |
| [Kamailio](http://www.kamailio.org)             | 1, 2                   | yes    | yes    |
| [OpenSIPS](http://www.opensips.org)             | 1, 2                   | yes    | yes    |

Contributors
------------

- [Matthias Endler](https://github.com/matthias-endler)
- [Pierre Fenoll](https://github.com/fenollp)

License
-------

This project is licensed under the ISC License. See [LICENSE](LICENSE) for details.

---

Copyright &#169; 2013 Matthias Endler. All rights reserved.
