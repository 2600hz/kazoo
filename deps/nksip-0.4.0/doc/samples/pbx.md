# NkSIP PBX Sample Application

This Erlang application is a _SipApp_ implementing a basic PBX-like SIP proxy application with some extras. Its purpose is to demostrate the use of NkSIP SipApps. it starts a new SipApp named `pbx` listening on all of the host's interfaces, port 5060 for tcp and udp protocols and 5061 for tls. 

Any SIP endpoint can register with the server, using any user, domain "nksip", and password "1234" (for exmple "100@nksip").

From this moment on, any endpoint can _call_ any other using its registered name (or _address-of-record_) through the proxy. NkPBX starts to monitor in background each registered endpoint, measuring its response time with periodic _OPTION_ requests.

There are some special addresses or "extensions" available:

Extension|Description
---------|-----------
`200@nksip`|Send the call in parallel to all registered endpoints but me, including a _Record-Route_, so all dialog requests will go through this proxy
`201@nksip`|Send the call in parallel to each two random registered endpoints. Include a custom header but no _Record-Route_, so next dialog requests will go directly to the endpoint
`202@nksip`|Send the request to the _fastest_ registered endpoint
`203@nksip`|Send the request to the _slowest_ registered endpoint
`(any)@nksip`|If this contact is registered, send the call to it
`nksip`|Process the request in the proxy. Since we have not implemented callbacks `sip_invite/2`, `sip_options/2`, etc., all responses will be default responses as defined in `nksip_sipapp`). _REGISTER_ requests will be processed as configured when starting the SipApp

Feel free to use the functions `nksip_pbx:trace/1` to start or stop the trace of SIP messages to console or `nksip_pbx:loglevel/1` to change the console log level. 

Use the function `nksip_pbx_sipapp:check_speed/1` to stop the automatic generation of
_OPTION_ requests.
