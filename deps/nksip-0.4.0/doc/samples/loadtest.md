# NkSIP Load Tester Sample Application

This Erlang application is a _SipApp_ implementing a very simple SIP load tester.

Its purpose is to test NkSIP under heavy load, using transports _udp_, _tcp_ and _tls_, and testing _OPTION_ requests, _REGISTER_ requests and _INVITE_ + _ACK_ + _BYE_ call setups. 

If offers two different test modes:

* In the _standard_ mode, it generates and sends the requests using NkSIP request generation functions.
* In _raw_ mode, it sends locally generated raw packets. This mode is useful to test a NkSIP in the same machine as an UAS server, without the UAC processing overhead.

Full test suites are available in `nksip_loadtest:full/0` and `nksip_loadtest:full/1`. More fine-grained tests can be started with `nksip_loadtest:launch/1`.

The test is directed to a started NkSIP in the same host by default, but you can start this application and a listening server (using `nksip_loadtest_lib:start_server/0`) on another node and use the option `host` to send the requests to the remote node.
