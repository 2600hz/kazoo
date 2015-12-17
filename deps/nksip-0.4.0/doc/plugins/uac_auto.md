# nksip_uac_auto plugin

## Description

This plugin allows a SipApp to program a serie of automatic registrations (sending REGISTER request) to a remote registrar, or automatic pings (sending OPTIONS requests) to any other SIP entity.


## Dependant plugins

None


## Configuration values

Option|Default|Description
---|---|---
{register, Uri::nksip:user_uri()}|-|If defined, it programs an automatic registration to this registrar on SipApp startup 
{register_expires, Expires::integer()}|300|Default registration interval
{nksip_uac_auto_timer, Secs::pos_integer()}|5|Interval to check for new expired timers



## API functions

### start_register/5 

```erlang
nksip_uac_auto:start_register(nksip:app_name()|nksip:app_id(), Id::term(), Uri::nksip:user_uri(), 
								    Time::pos_integer(), Opts::nksip:optslist()) -> 
    {ok, boolean()} | {error, invalid_uri|sipapp_not_found}.
```

Programs the SipApp to start a serie of automatic registrations to the registrar at `Uri`, at `Time` (in seconds) intervals. `RegId` indentifies this request to be be able to stop it later. Use [get_registers/1](#get_registers1) or the  callback function [sip_uac_auto_register_update/3](#sip_uac_auto_register_update3) to know about the registration status.

If the SipApp is configured to support outbound (RFC5626), there is a `reg_id` option in `Opts` with a numeric value, and also the remote party replies indicating it has outbound support, NkSIP will keep the _flow_ opened sending _keep-alive_ packets. If the flow goes down, NkSIP will try to re-send the registration at specific intervals.


### stop_register/2

```erlang
nksip_uac_auto:stop_register(nksip:app_name()|nksip:app_id(), Id::term()) -> 
    ok | not_found.
```

Stops a previously started registration serie.
For outbound-supported requests, it will also stop the keep-alive messages on the flow.


### get_registers/1

```erlang
nksip_uac_auto:get_registers(nksip:app_name()|nksip:app_id()) -> 
    [{RegId::term(), OK::boolean(), Time::non_neg_integer()}].
```
Get current registration status, including if last registration was successful and time remaining to next one.
 

### start_ping/5

```erlang
nksip_uac_auto:start_ping(nksip:app_name()|nksip:app_id(), Id::term(), Uri::nksip:user_uri(), 
						  Time::pos_integer(), Opts::nksip:optslist()) -> 
    {ok, boolean()} | {error, invalid_uri|sipapp_not_found}.


Programs the SipApp to start a serie of _pings_ (OPTION requests) to the SIP element at `Uri`, at `Time` (in seconds) intervals. `Id` indentifies this request to be able to stop it later. Use [get_pings1](#get_pings1) or the callback function [sip_uac_auto_ping_update/3](#sip_uac_auto_ping_update3) to know about the ping status.


### stop_ping/2

```erlang
nksip_uac_auto:stop_ping(nksip:app_name()|nksip:app_id(), PingId::term()) ->
    ok | not_found.

Gets the current ping status, including if last ping was successful and time remaining to next one.


## Callback functions

You can implement any of these callback functions in your SipApp callback module.



### sip_uac_auto_register_update/3

```erlang
sip_uac_auto_register_update(AppId::nksip:app_id(), 
                             RegId::term(), OK::boolean()) ->
    ok.
```

If implemented, it will called each time a registration serie changes its state.


### sip_uac_auto_ping_update/3

```erlang
sip_uac_auto_ping_update(AppId::nksip:app_id(), 
                         PingId::term(), OK::boolean()) ->
    ok.
```

If implemented, it will called each time a ping serie changes its state.
