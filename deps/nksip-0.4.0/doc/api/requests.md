# Requests API

This document describes the API NkSIP makes available to extract information from Requests.

Most functions in the API allows two ways to refer to the requests:
* From a full *request object* (`nksip:request()`). Most functions called in the SipApp's _callback module_ receive a full request object, and you can use these functions to get information from it.
* From a *request handle* (`nksip:handle()`). You can get a request handle from a request object using [get_handle/1](#get_handle1). You can then use the handle to call most functions in this API. 
    
    In this case, the API function must contact with the corresponding call process to get the actual request, so you cannot use this method _inside_ the same call process (like in the callback functions). This method is useful to refer to the request from a _spawned_ process (specially for [reply/2](#reply2)), avoiding the need to copy the full object. Please notice that the request object may not exists any longer at the moment that the handle is used. Most functions return `error` in this case.


<br/>


Function|Description
---|---
[get_handle/1](#get_handle1)|Grabs a request's handle
[app_id/1](#app_id1)|Gets then SipApp's _internal name_
[app_name/1](#app_name1)|Gets the SipApp's _user name_
[method/1](#method1)|Gets the method of the request
[body/1](#body1)|Gets the body of the request
[call_id/1](#call_id1)|Gets the Call-ID header of the request
[meta/2](#meta2)|Gets specific metadata from the request
[metas/2](#meta2)|Gets specific metadata from the request
[header/2](#header2)|Gets the values for a header or headers in a request
[reply/2](#reply2)|Sends a reply to a request using a handle
[is_local_ruri/1](#is_local_ruri1)|Checks if this request would be sent to a local address in case of beeing proxied


## Functions List

### get_handle/1
```erlang
nksip_request:get_handle(nksip:request()|nksip:handle()) ->
    {ok, nksip:handle()} | {error, term()}.
```
Grabs a request's handle.


### app_id/1
```erlang
nksip_request:app_id(nksip:request()|nksip:handle()) -> 
    {ok, nksip:app_id()} | {error, term()}.
```
Gets then SipApp's _internal name_.


### app_name/1
```erlang
nksip_request:app_name(nksip:request()|nksip:handle()) -> 
    {ok, nksip:app_name()} | {error, term()}.
```
Gets the SipApp's _user name_


### method/1
```erlang
nksip_request:method(nksip:request()|nksip:handle()) ->
    {ok, nksip:method()} | {error, term()}.
```
Gets the method of the request.


### body/1
```erlang
nksip_request:body(nksip:request()|nksip:handle()) ->
    {ok, nksip:body()} | {error, term()}.
```
Gets the body of the request.


### call_id/1
```erlang
nksip_request:call_id(nksip:request()|nksip:handle()) ->
    {ok, nksip:call_id()} | {error, term()}.
```
Gets the Call-ID header of the request.


### meta/2
```erlang
nksip_request:meta(Meta::nksip_sipmsg:field(), nksip:request()|nksip:handle()) ->
    {ok, term()} | {error, term()}.
```
Gets specific metadata from the request.

See [Metadata Fields](../reference/metadata.md) for a description of available fields.


### metas/2
```erlang
nksip_request:meta(Meta::[nksip_sipmsg:field()], nksip:request()|nksip:handle()) ->
    {ok, [{nksip_sipmsg:field(), term()}]} | {error, term()}.
```
Gets specific metadata from the request.

See [Metadata Fields](../reference/metadata.md) for a description of available fields.


### header/2
```erlang
nksip_request:header(Name::string()|binary(), nksip:request()|nksip:handle()) -> 
    {ok, [binary()]} | {error, term()}.
```
Gets the all the values for a header.

NkSIP uses only lowercase for header names.


### reply/2
```erlang
nksip_request:reply(nksip:sipreply(), nksip:handle()) -> 
    ok | {error, term()}.
```
Sends a reply to a request using a handle.

See [Receiving Requests](../guide/receiving_requests.md) for a overall description and [Reply Options](../reference/reply_options.md) for a description of available responses.


### is_local_ruri/1
```erlang
nksip_request:is_local_ruri(nksip:request()) -> 
    boolean().
```
Checks if the R-URI of this request points to a local address.

