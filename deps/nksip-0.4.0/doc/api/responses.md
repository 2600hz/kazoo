# Responses API

This document describes the API NkSIP makes available to extract information from Responses.

Most functions in the API allows two ways to refer to the responses:
* From a full *response object* (`nksip:response()`). When you use the option `callback` when using the any [request sending function](../reference/sending_functions.md), it is called as `{reply, Code, Response, Call}`. You can use these API functions inside the function call.
* From a *response handle* (`nksip:handle()`). You can get a response handle from a response object using [get_handle/1](#get_handle1). You can then use the handle to call most functions in this API. 
    
In this case, the API function must contact with the corresponding call process to get the actual response, so you cannot use this method _inside_ the same call process (like in the callback function). This method is useful to refer to the response from a _spawned_ process, avoiding the need to copy the full object. Please notice that the response object may not exists any longer at the moment that the handle is used. Most functions return `error` in this case.


<br/>


Function|Description
---|---
[get_handle/1](#get_handle1)|Grabs a response's handle
[app_id/1](#app_id1)|Gets then SipApp's _internal name_
[app_name/1](#app_name1)|Gets the SipApp's _user name_
[code/1](#method1)|Gets the SIP code of the response
[body/1](#body1)|Gets the body of the response
[call_id/1](#call_id1)|Gets the Call-ID header of the response
[meta/2](#meta2)|Gets specific metadata from the response
[metas/2](#meta2)|Gets specific metadata from the response
[header/2](#header2)|Gets the values for a header or headers in a response
[wait_491/0](#wait_4910)|Sleeps a random time between 2.1 and 4 secs


## Functions List

### get_handle/1
```erlang
nksip_response:get_handle(nksip:response()|nksip:handle()) ->
    {ok, nksip:handle()} | {error, term()}.
```
Grabs a response's handle.


### app_id/1
```erlang
nksip_response:app_id(nksip:response()|nksip:handle()) -> 
    {ok, nksip:app_id()} | {error, term()}.
```
Gets then SipApp's _internal name_.


### app_name/1
```erlang
nksip_response:app_name(nksip:response()|nksip:handle()) -> 
    {ok, nksip:app_name()}  | {error, term()}.
```
Gets the SipApp's _user name_


### code/1
```erlang
nksip_response:code(nksip:response()|nksip:handle()) ->
    {ok, nksip:sip_code()} | {error, term()}.
```
Gets the SIP code of the response.


### body/1
```erlang
nksip_response:body(nksip:response()|nksip:handle()) ->
    {ok, nksip:body()} | {error, term()}.
```
Gets the body of the response.


### call_id/1
```erlang
nksip_response:call_id(nksip:response()|nksip:handle()) ->
    {ok, nksip:call_id()} | {error, term()}.
```
Gets the Call-ID header of the response.


### meta/2
```erlang
nksip_response:meta(Meta::nksip_sipmsg:field(), nksip:response()|nksip:handle()) ->
    {ok, term()} | {error, term()}.
```
Gets specific metadata from the response.

See [Metadata Fields](../reference/metadata.md) for a description of available fields.


### metas/2
```erlang
nksip_response:meta(Meta::[nksip_sipmsg:field()], nksip:response()|nksip:handle()) ->
    {ok, [{nksip_sipmsg:field(), term()}]} | {error, term()}.
```
Gets specific metadata from the response.

See [Metadata Fields](../reference/metadata.md) for a description of available fields.


### header/2
```erlang
nksip_response:header(Name::string()|binary(), nksip:response()|nksip:handle()) -> 
    {ok, [binary()]} | {error, term()}.
```
Gets all the values for a header.

NkSIP uses only lowercase for header names.


### wait_491/0
```erlang
nksip_response:wait_491() -> 
    ok.
```
Sleeps a random time between 2.1 and 4 secs. It should be called after receiving a 491 response and before trying the response again.

