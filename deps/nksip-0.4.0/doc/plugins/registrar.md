# Registrar Server Plugin

* [Name](#name)
* [Description](#description)
* [Dependant Plugins](#dependant-plugins)
* [Configuration Values](#configuration-values)
* [API Functions](#api-functions)
* [Callback Functions](#callback-functions)
* [Examples](#examples)

## Name
### `nksip_registrar`

## Description

This plugin provides a full registrar server implementation according to RFC3261. _Path_ is also supported, according to RFC3327. It uses by default the built-in, RAM-only store, but can be configured to use any other database implementing callback [sip_registrar_store/2](#sip_registrar_store2). Each started SipApp activating this plugin maintains a fully independent set of registrations.

Once activated, the following happens:
* When a new _REGISTER_request arrives, you have two options:
  * Not implementing `sip_register/2` in you SipApp callback function. In this case, the request will be processed automatically.
  * Implementing your own `sip_register/`. You must inspect the request, and, in case you want it to be process, call [request/1](#request1)
  * 


When a new _REGISTER_ request arrives at a SipApp, and if you order to `process` the request in [sip_route/6](../reference/callback_functions.md#sip_route5) callback, NkSIP will try to call [sip_register/2](../reference/callback_functions.md#sip_register2) callback if it is defined in yor SipApp's callback module. If it is not defined there, NkSIP will process the request automatically. If you implement `sip_register/3` to customize the registration process you should call [request/1](#request1) directly.

Use [find/4](#find4) or [qfind/4](qfind4) to search for a specific registration's contacts, and [is_registered/1](#is_registered1) to check if the _Request-URI_ of a specific request is registered.


_REGISTER_ will also be added to all generated _Allow_ headers.



## Dependant Plugins

None


## Configuration Values

### SipApp configuration values

Option|Default|Description
---|---|---
nksip_registrar_default_time|3600 (1h)|Default registration expiration
nksip_registrar_min_time|60 (1m)|Minimum registration expiration
nksip_registrar_max_time|86400 (24h)|Maximum registration expiration


## API functions

### find/2

```erlang
find(nksip:app_name()|nksip:app_id(), nksip:aor() | nksip:uri()) ->
    [nksip:uri()].
```

Finds the registered contacts for this SipApp and _AOR_ or _Uri_, for example
```nksip_registrar:find(my_app, "sip:user@domain")``` or 
```nksip_registrar:find("my_other_app", {sip, <<"user">>, <<"domain">>})```


### find/4

```erlang
find(nksip:app_name()|nksip:app_id(), nksip:scheme(), binary(), binary()) ->
    [nksip:uri()].
```

Similar to `find/2`.


### qfind/2

```erlang
qfind(nksip:app_name()|nksip:app_id(), AOR::nksip:aor()) ->
    nksip:uri_set().
```

Gets all current registered contacts for an _AOR_, aggregated on _Q_ values.
You can use this function to generate a parallel and/o serial proxy request. For example, you could implement the following [sip_route/6](../reference/callback_functions.md#sip_route5) callback function:

```erlang
sip_route(Scheme, User, Domain, Req, _Call) -> 
    case Domain of
    	<<"nksip">> when User == <<>> ->
            process;
        <<"nksip">> ->
            case nksip_registrar:qfind(my_registrar, Scheme, User, Domain) of
                [] -> {reply, temporarily_unavailable};
                UriList -> {proxy, UriList, Opts}
            end;
        false ->
            {reply, forbidden}
    end.
```

Using this example, when a new request arrives at our proxy for domain 'nksip' and having an user, will be forked to all registered contacts, launching in parallel contacts having the same 'q' value.


### qfind/4

```erlang
qfind(nksip:app_name()|nksip:app_id(), nksip:scheme(), binary(), binary()) ->
    nksip:uri_set().
```

Similar to `qfind/2`


### delete/4

```erlang
delete(nksip:app_name()|nksip:app_id(), nksip:scheme(), binary(), binary()) ->
    ok | not_found | callback_error.
```

Deletes all registered contacts for an _AOR_.


### is_registered/1

```erlang
is_registered(Req::nksip:request()) ->
    boolean().
```

Finds if a the request has a _From_ header that has been already registered using the same transport, ip and port, or have a registered _Contact_ header  having the same received transport, ip and port.


### process/1

```erlang
request(nksip:request()) ->
    nksip:sipreply().
```

Call this function to process and incoming _REGISTER_ request. It returns an appropiate response, depending on the registration result.
If the Expires_ header is 0, the indicated _Contact_ will be unregistered. If _Contact_ header is `*`, all previous contacts will be unregistered.
The requested _Contact_ will replace a previous registration if it has the same `reg-id` and `+sip_instance` values, or has the same transport scheme, protocol, user, domain and port.

If the request is successful, a 200-code `nksip:sipreply()` is returned, including one or more _Contact_ headers (for all of the current registered contacts), _Date_ and _Allow_ headers.

For example, you could implement the following [sip_register/2](../reference/callback_functions.md#sip_register2) callback function:

```erlang
sip_register(Req, _Call) ->
	case nksip_request:meta(domain, Req) of
		{ok, <<"nksip">>} -> {reply, nksip_registrar:process(Req)};
		_ -> {reply, forbidden}
	end.
```


## Callback functions

You can implement any of these callback functions in your SipApp callback module.



### sip_registrar_store/2

```erlang
sip_registrar_store(StoreOp, AppId) ->
    [RegContact] | ok | not_found when 
        StoreOp :: {get, AOR} | {put, AOR, [RegContact], TTL} | 
                   {del, AOR} | del_all,
        AppId :: nksip:app_id(),
        AOR :: nksip:aor(),
        RegContact :: nksip_registrar_lib:reg_contact(),
        TTL :: integer().
```

Called when a operation database must be done on the registrar database. By default the in-memory database is used, but you can impement it to use your own database.

Op|Response|Comments
---|---|---
{get, AOR}|[RegContact]|Retrieve all stored contacts for this `AOR` and `AppId`.
{put, AOR, [RegContact], TTL}|ok|Store the list of contacts for this `AOR` and `AppId`. The record must be automatically deleted after `TTL` seconds.
{del, AOR}|ok &#124; not_found|Delete all stored contacts for this `AOR` and `AppIdp`, returning `ok` or `not_found` if the `AOR` is not found.
del_all|ok|Delete all stored information for this `AppId`.

See the [default implementation](../../plugins/src/nksip_registrar_sipapp.erl) as a basis. 

## Examples

```erlang
-module(example).
-compile([export_all]).

-include_lib("nksip/include/nksip.hrl").


start() ->
    {ok, _} = nksip:start(server, ?MODULE, [], [
        {from, "sip:server@nksip"},
        {plugins, [nksip_registrar]},
        {transports, [{udp, all, 5060}, {tls, all, 5061}]},
        {nksip_registrar_min_time, 60}
    ]),
    {ok, _} = nksip:start(client, ?MODULE, [], [
        {from, "sip:client@nksip"},
        {local_host, "127.0.0.1"},
        {transports, [{udp, all, 5070}, {tls, all, 5071}]}
    ]).

stop() ->
    ok = nksip:stop(server),
    ok = nksip:stop(client).


test1() ->
    {ok, 200, []} = nksip_uac:register(client, "sip:127.0.0.1", [unregister_all]),
    [] = nksip_registrar:find(server, sip, <<"client">>, <<"nksip">>),
    
    {ok, 200, []} = nksip_uac:register(client, "sip:127.0.0.1", [contact]),
    [
        #uri{
            user = <<"client">>,
            domain = <<"127.0.0.1">>,
            port = 5070
        }
    ] = nksip_registrar:find(server, sip, "client", "nksip"),
    {ok, 200, []} = nksip_uac:register(client, "sip:127.0.0.1", [unregister_all]),
    [] = nksip_registrar:find(server, sip, <<"client">>, <<"nksip">>),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%  CallBacks (servers and clients) %%%%%%%%%%%%%%%%%%%%%


sip_route(Scheme, User, Domain, Req, _Call) ->
    case nksip_request:app_name(Req) of
        {ok, server} ->
            Opts = [record_route, {insert, "x-nk-server", "server"}],
            case lists:member(Domain, [<<"nksip">>, <<"127.0.0.1">>]) of
                true when User =:= <<>> ->
                    process;
                true when Domain =:= <<"nksip">> ->
                    case nksip_registrar:find(server, Scheme, User, Domain) of
                        [] -> {reply, temporarily_unavailable};
                        UriList -> {proxy, UriList, Opts}
                    end;
                _ ->
                    {proxy, ruri, Opts}
            end;
        {ok, client} ->
            process
    end.
```
