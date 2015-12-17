# Dialogs API

This document describes the API NkSIP makes available to extract information from Dialogs.

Most functions in the API allows two ways to refer to the dialogs:
* From a full *dialog object* (`nksip:dialog()`). In some specific _callback functions_ like [sip_dialog_update/3](../reference/callback_functions.md#sip_dialog_update3) you receive a full dialog object. You can use these API functions inside the function call. 

    You can also get a full dialog object calling [get_dialog/2](#get_dialog2) using a request or subscription object and a call object (received in callback functions like [sip_invite/2](../reference/callback_functions.md#sip_invite2), [sip_options/2](../reference/callback_functions.md#sip_options2), etc.

* From a *dialog handle* (`nksip:handle()`). You can get a dialog handle from a dialog object, request or response objects or handles for dialogs, request, responses or subscriptions, calling [get_handle/1](#get_handle/1). You can then use the handle to call most functions in this API. 
    
    In this case, the API function must contact with the corresponding call process to get the actual dialog, so you cannot use this method _inside_ the same call process (like in the callback functions). This method is useful to refer to the dialog from a _spawned_ process, avoiding the need to copy the full object. Please notice that the dialog object may not exists any longer at the moment that the handle is used. Most functions return `error` in this case.


<br/>


Function|Description
---|---
[get_handle/1](#get_handle1)|Grabs a dialog's handle
[app_id/1](#app_id1)|Gets then SipApp's _internal name_
[app_name/1](#app_name1)|Gets the SipApp's _user name_
[call_id/1](#call_id1)|Gets the Call-ID header of the dialog
[meta/2](#meta2)|Gets specific metadata from the dialog
[metas/2](#meta2)|Gets specific metadata from the dialog
[get_dialog/2](#get_dialog2)|Gets a dialog object from a request and a call objects
[get_all/0](#get_all0)|Get the handles of all started dialogs
[get_all/2](#get_all2)|Gets all current started dialog handles belonging to App and having Call-ID
[bye_all/0](#bye_all0)|Sends an in-dialog BYE to all existing dialogs
[stop/1](#stop1)|Stops an existing dialog from its handle (remove it from memory)
[stop_all/0](#stop_all0)|Stops (removes from memory) all current dialogs
[get_authorized_list/0](#get_authorized_list1)|Gets the authorized list of transport, ip and ports for a dialog.
[clear_authorized_list/0](#clear_authorized_list1)|Clear the authorized list of transport, ip and ports for a dialog.


## Functions List

### get_handle/1
```erlang
nksip_dialog:get_handle(nksip:dialog()|nksip:request()|nksip:response()|nksip:handle()) ->
    {ok, nksip:handle()} | {error, term()}.
```
Grabs a dialog's handle.


### app_id/1
```erlang
nksip_dialog:app_id(nksip:dialog()|nksip:handle()) -> 
    {ok, nksip:app_id()}.
```
Gets then SipApp's _internal name_.


### app_name/1
```erlang
nksip_dialog:app_name(nksip:dialog()|nksip:handle()) -> 
    {ok, nksip:app_name()}.
```
Gets the SipApp's _user name_


### call_id/1
```erlang
nksip_dialog:call_id(nksip:dialog()|nksip:handle()) ->
    {ok, nksip:call_id()}.
```
Gets the Call-ID header of the dialog.


### meta/2
```erlang
nksip_dialog:meta(nksip_dialog:field(), nksip:dialog()|nksip:handle()) -> 
    {ok, term()} | {error, term()}.
```
Gets specific metadata from the dialog.

See [Metadata Fields](../reference/metadata.md) for a description of available fields.


### metas/2
```erlang
nksip_dialog:meta([nksip_dialog:field()], nksip:dialog()|nksip:handle()) -> 
    {ok, [{field(), term()}]} | {error, term()}.
```
Gets specific metadata from the dialog.

See [Metadata Fields](../reference/metadata.md) for a description of available fields.


### get_dialog/2
```erlang
nksip_dialog:get_dialog(nksip:request()|nksip:response()|nksip:subscription(), nksip:call()) ->
    {ok, nksip:dialog()} | {error, term()}.
```
Gets a dialog object from a _request_, _response_ or _subscription_ object and a _call object_.


### get_all/0
```erlang
nksip_dialog:get_all() ->
    [nksip:handle()].
```
Get the handles of all started dialogs.


### get_all/2
```erlang
nksip_dialog:get_all(App::nksip:app_id(), CallId::nksip:call_id()) ->
    [nksip:handle()].
```
Gets all current started dialog handles belonging to a SipApp and having a specific _Call-ID_.


### bye_all/0
```erlang
nksip_dialog:bye_all() ->
    ok.
```
Sends an in-dialog BYE to all existing dialogs.


### stop/1
```erlang
nksip_dialog:stop(nksip:handle()) ->
    ok | {error, term()}.
```
Destroys an existing dialog from its handle (remove it from memory).


### stop_all/0
```erlang
nksip_dialog:stop_all() ->
    ok.
```
Destroys all current dialogs.


### get_authorized_list/1
```erlang
get_authorized_list(nksip:handle()) ->
    [{nksip:protocol(), inet:ip_address(), inet:port_number()}].
```

Gets the authorized list of transport, ip and ports for a dialog.


### clear_authorized_list/1
```erlang
clear_authorized_list(nksip:handle()) ->
    ok | {error, term()}.
```

Clears the authorized list of transport, ip and ports for a dialog.

