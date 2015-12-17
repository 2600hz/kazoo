# NkSIP SipApps' API

This document describes the API that NkSIP makes available to SipApps.<br/>
These functions are available in the [nksip.erl](../../src/nksip.erl) module.<br/>
See [Starting a SipApp](../guide/start_a_sipapp.md) for a general overview.


Function|Description
---|---
[start/4](#start4)|Starts a new SipApp
[stop/1](#stop1)|Stops a started SipApp
[stop_all/0](#stop_all/0)|Stops all started SipApps
[update/2](#update/2)|Updates the configuration of a started SipApp
[get_all/0](#get_all0)|Gets all currenty started SipApps
[get/2](#get2)|Gets a value for a SipApp variable
[get/3](#get3)|Gets a value for a SipApp variable, with a default
[put/3](#put3)|Saves a vaule for a SipApp variable
[del/2](#del2)|Deletes a SipApp variable
[get_pid/1](#get_pid1)|Gets the pid of a SipApp's gen_server process
[find_app/1](#find_app1)|Finds the _internal name_ for a currently started SipApp
[call/2](#call2)|Synchronous call to the SipApp's gen_server process
[call/3](#call3)|Synchronous call to the SipApp's gen_server process with timeout
[cast/2](#call3)|Asynchronous call to the SipApp's gen_server process
[get_uuid/1](#get_uuid/1)|Get the current _UUID_ for a stared SipApp


## Functions list

### start/4
```erlang
nksip:start(UserName::nksip:app_name(), CallbackModule::atom(), Args::term(), Opts::nksip:optslist()) -> 
	{ok, nksip:app_id()} | {error, term()}.
```

Starts a new SipApp. 

See [Starting a SipApp](../guide/start_a_sipapp.md) and [Configuration](../reference/configuration.md) to see the list of available configuration options. 

NkSIP returns the _internal name_ of the application. In most API calls you can use the _user name_ or the _internal name_.


### stop/1
```erlang
nksip:stop(Name::nksip:app_name()|nksip:app_id()) -> 
    ok | {error,term()}.
```
Stops a currently started SipApp.


### stop_all/0
```erlang
nksip:stop_all() -> 
   	ok.
```
Stops all currently started SipApps.


### update/2
```erlang
nksip:update(nksip:app_name()|nksip:app_id(), nksip:optslist()) ->
    {ok, nksip:app_id()} | {error, term()}.
```
Updates the callback module or options of a running SipApp.

You can change any configuration parameter on the fly, except for transports. See [Configuration](../reference/configuration.md).


### get_all/0
```erlang
nksip:get_all() ->
    [{AppName::term(), AppId::nksip:app_id()}].
```
Gets the user and internal ids of all started SipApps.


### get/2
```erlang
nksip:get(nksip:app_name()|nksip:app_id(), term()) ->
    {ok, term()} | undefined | {error, term()}.
```
Gets a value from SipApp's store.
See [saving state information](../guide/start_a_sipapp.md#saving-state-information).


### get/3
```erlang
nksip:get(nksip:app_name()|nksip:app_id(), term(), term()) ->
    {ok, term()} | {error, term()}.
```
Gets a value from SipApp's store, using a default if not found.
See [saving state information](../guide/start_a_sipapp.md#saving-state-information).

### put/3
```erlang
nksip:put(nksip:app_name()|nksip:app_id(), term(), term()) ->
    ok | {error, term()}.
```
Inserts a value in SipApp's store.
See [saving state information](../guide/start_a_sipapp.md#saving-state-information).

### del/2
```erlang
nksip:del(nksip:app_name()|nksip:app_id(), term()) ->
    ok | {error, term()}.
```
Deletes a value from SipApp's store.
See [saving state information](../guide/start_a_sipapp.md#saving-state-information).

### get_pid/1
```erlang
nksip:get_pid(nksip:app_name()|app_id()) -> 
    pid() | undefined.
```
Gets the SipApp's _gen_server process_ `pid()`.
See [starting a SipApp](../guide/start_a_sipapp.md).


### find_app/1
```erlang
nksip:find_app(term()) ->
    {ok, app_id()} | not_found.
```
Finds the _internal name_ of an existing SipApp.


### call/2
```erlang
nksip:call(nksip:app_name()|nksip:app_id(), term()) ->
    term().
```
Synchronous call to the SipApp's gen_server process. It is a simple `gen_server:call/2` but allowing SipApp names.


### call/3
```erlang
nksip:call(nksip:app_name()|nksip:app_id(), term(), pos_integer()|infinity) ->
    term().
```
Synchronous call to the SipApp's gen_server process. It is a simple `gen_server:call/3` but allowing SipApp names.


### cast/2
```erlang
nksip:cast(nksip:app_name()|nksip:app_id(), term()) ->
    term().
```
Asynchronous call to the SipApp's gen_server process. It is a simple `gen_server:cast/2` but allowing SipApp names.


### get_uuid/1
```erlang
nksip:get_uuid(nksip:app_name()|nksip:nksip:app_id()) -> 
    {ok, binary()} | {error, term()}.
```
Gets the SipApp's _UUID_.

