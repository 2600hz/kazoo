# UAC Auto Registration Plugin

* [Name](#name)
* [Description](#description)
* [Dependant Plugins](#dependant-plugins)
* [Configuration Values](#configuration-values)
* [API Functions](#api-functions)
* [Callback Functions](#callback-functions)
* [Examples](#examples)


## Name
### `nksip_uac_auto_register`


## Description

This plugin allows a SipApp to program a serie of automatic registrations (sending REGISTER request) to a remote registrar, or automatic pings (sending OPTIONS requests) to any other SIP entity.


## Dependant Plugins

None


## Configuration Values

### SipApp configuration values

Option|Default|Description
---|---|---
nksip_uac_auto_register_timer|5 (secs)|Interval to check for new expired timers


## API functions

### start_register/4 

```erlang
start_register(App::nksip:app_name()|nksip:app_id(), Id::term(), Uri::nksip:user_uri(), 
               Opts::nksip:optslist()) -> 
    {ok, boolean()} | {error, term()}.
```

Programs the SipApp to start a serie of automatic registrations to the registrar at `Uri`. `Id` indentifies this request to be be able to stop it later. Use [get_registers/1](#get_registers1) or the  callback function [sip_uac_auto_register_updated_register/3](#sip_uac_auto_register_updated_register3) to know about the registration status.

Opts are passed to the REGISTER sending functions. You can use the `expires` option to change the default re-register time from the default of 300 secs.


### stop_register/2

```erlang
stop_register(App::nksip:app_name()|nksip:app_id(), Id::term()) -> 
    ok | not_found.
```

Stops a previously started registration serie.


### get_registers/1

```erlang
get_registers(App::nksip:app_name()|nksip:app_id()) -> 
    [{Id::term(), OK::boolean(), Time::non_neg_integer()}].
```
Get current registration status, including if last registration was successful and time remaining to next one.
 

### start_ping/4

```erlang
start_ping(App::nksip:app_name()|nksip:app_id(), Id::term(), Uri::nksip:user_uri(), 
		   Opts::nksip:optslist()) -> 
    {ok, boolean()} | {error, term()}.
```

Programs the SipApp to start a serie of _pings_ (OPTION requests) to the SIP element at `Uri`. `Id` indentifies this request to be able to stop it later. Use [get_pings/1](#get_pings1) or the callback function [sip_uac_auto_register_updated_ping/3](#sip_uac_auto_register_updated_ping3) to know about the ping status.

You can use the `expires` option to change the default re-options time from the default of 300 secs.



### stop_ping/2

```erlang
stop_ping(App::nksip:app_name()|nksip:app_id(), Id::term()) ->
    ok | not_found.
```

Stops a previously started ping serie.


### get_pings/1

```erlang
get_pings(App::nksip:app_name()|nksip:app_id()) -> 
    [{Id::term(), OK::boolean(), Time::non_neg_integer()}].
```

Get current ping status, including if last ping was successful and time remaining to next one.
 


## Callback functions

You can implement any of these callback functions in your SipApp callback module.



### sip_uac_auto_register_updated_register/3

```erlang
sip_uac_auto_register_updated_register(Id::term(), OK::boolean(), 
                                             AppId::nksip:app_id()) ->
    ok.
```

If implemented, it will called each time a registration serie changes its state.


### sip_uac_auto_register_updated_ping/3

```erlang
sip_uac_auto_register_updated_ping(Id::term(), OK::boolean(), 
                                         AppId::nksip:app_id()) ->
    ok.
```

If implemented, it will called each time a ping serie changes its state.


## Examples

```erlang
-module(sample).
-include_lib("nksip/include/nksip.hrl").
-compile([export_all]).

start() ->
    {ok, _} = nksip:start(server, ?MODULE, [], [
        {plugins, [nksip_registrar]},
        {transports, [{udp, all, 5080}]},
        {nksip_registrar_min_time, 1},
        {nksip_uac_auto_register_timer, 1}
    ]),
    {ok, _} = nksip:start(client, ?MODULE, [], [
        {from, "\"NkSIP Basic SUITE Test Client\" <sip:client@nksip>"},
        {transports, [{udp, all, 5070}, {tls, all, 5071}]},
        {plugins, [nksip_uac_auto_register]},
        {nksip_uac_auto_register_timer, 1}
    ]).
            

stop() ->
    ok = nksip:stop(server1),
    ok = nksip:stop(client),


test() ->
    {ok, true} = nksip_uac_auto_register:start_ping(client, ping1, 
                                "<sip:127.0.0.1:5080;transport=tcp>", [{expires, 5}]),
    {ok, true} = nksip_uac_auto_register:start_register(client, reg1, 
                                "<sip:127.0.0.1:5080;transport=tcp>", [{expires, 1}]),

    [{ping1, true, _}] = nksip_uac_auto_register:get_pings(client),
    [{reg1, true, _}] = nksip_uac_auto_register:get_registers(client),
    ok = nksip_uac_auto_register:stop_ping(client, ping1),
    ok = nksip_uac_auto_register:stop_register(client, reg1),
    [] = nksip_uac_auto_register:get_pings(client),
    [] = nksip_uac_auto_register:get_registers(client),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%  CallBacks (servers and clients) %%%%%%%%%%%%%%%%%%%%%


sip_uac_auto_register_updated_ping(PingId, OK, _AppId) ->
    io:format("PING ~p new status: ~p", [PingId, Ok]),
    ok.

sip_uac_auto_register_updated_register(RegId, OK, AppId=State) ->
    io:format("REG ~p new status: ~p", [RegId, Ok]),
    ok.
```






