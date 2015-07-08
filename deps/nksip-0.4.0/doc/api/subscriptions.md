# Subscriptions API

This document describes the API NkSIP makes available to extract information from Subscriptions.

Most functions in the API allows two ways to refer to the subscription:
* From a full *subscription object* (`nksip:subscription()`). In some specific _callback functions_ like [sip_dialog_update/3](../reference/callback_functions.md#sip_dialog_update3) you receive a full subscription object. You can use these API functions inside the function call. You can also get a full subscription object calling [get_subscription/2](#get_subscription2) using a request and a call object (received in callback functions like [sip_invite/2](../reference/callback_functions.md#sip_invite2), [sip_options/2](../reference/callback_functions.md#sip_options2), etc.
* From a *subscription handle* (`nksip:handle()`). You can get a subscription handle from a subscription object, request or response objects or handles for subscription, request, responses or subscriptions, calling [get_handle/1](#get_handle/1). You can then use the handle to call most functions in this API. 
    
In this case, the API function must contact with the corresponding call process to get the actual subscription, so you cannot use this method _inside_ the same call process (like in the callback functions). This method is useful to refer to the subscription from a _spawned_ process, avoiding the need to copy the full object. Please notice that the subscription object may not exists any longer at the moment that the handle is used. Most functions return `error` in this case.


<br/>


Function|Description
---|---
[get_handle/1](#get_handle1)|Grabs a subscription's handle
[app_id/1](#app_id1)|Gets then SipApp's _internal name_
[app_name/1](#app_name1)|Gets the SipApp's _user name_
[call_id/1](#call_id1)|Gets the Call-ID header of the subscription
[meta/2](#meta2)|Gets specific metadata from the subscription
[metas/2](#meta2)|Gets specific metadata from the subscription
[get_subscription/2](#get_subscription2)|Gets a subscription object from a request and a call objects
[get_all/0](#get_all0)|Get the handles of all started subscription
[get_all/2](#get_all2)Gets all current started subscription handles belonging to App and having Call-ID

## Functions List

### get_handle/1
```erlang
nksip_subscription:get_handle(nksip:subscription()|nksip:request()|nksip:response()|nksip:handle()) ->
    {ok, nksip:handle()} | {error, term()}.
```
Grabs a subscription's handle.


### app_id/1
```erlang
nksip_subscription:app_id(nksip:subscription()|nksip:handle()) -> 
    {ok, nksip:app_id()}.
```
Gets then SipApp's _internal name_.


### app_name/1
```erlang
nksip_subscription:app_name(nksip:subscription()|nksip:handle()) -> 
    {ok, nksip:app_name()}.
```
Gets the SipApp's _user name_


### call_id/1
```erlang
nksip_subscription:call_id(nksip:subscription()|nksip:handle()) ->
    {ok, nksip:call_id()}.
```
Gets the Call-ID header of the subscription.


### meta/2
```erlang
nksip_subscription:meta(nksip_subscription:field(), nksip:subscription()|nksip:handle()) -> 
    {ok, term()} | {error, term()}.
```
Gets specific metadata from the subscription.

See [Metadata Fields](../reference/metadata.md) for a description of available fields.


### metas/2
```erlang
nksip_subscription:metas([nksip_subscription:field()], nksip:subscription()|nksip:handle()) -> 
    {ok, [{field(), term()}]} | {error, term()}.
```
Gets specific metadata from the subscription.

See [Metadata Fields](../reference/metadata.md) for a description of available fields.


### get_subscription/2
```erlang
nksip_subscription:get_subscription(nksip:request()|nksip:response(), nksip:call()) ->
    {ok, nksip:subscription()} | {error, term()}.
```
Gets a subscription object from a request and a call objects.


### get_all/0
```erlang
nksip_subscription:get_all() ->
    [nksip:handle()].
```
Get the handles of all started subscriptions handles.


### get_all/2
```erlang
nksip_subscription:get_all(App::nksip:app_id(), CallId::nksip:call_id()) ->
    [nksip:handle()].
```
Gets all current started subscription handles belonging to a SippApp and having a specific _Call-ID._


