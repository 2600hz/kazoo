# Event State Compositor Plugin 

* [Name](#name)
* [Description](#description)
* [Dependant Plugins](#dependant-plugins)
* [Configuration Values](#configuration-values)
* [API Functions](#api-functions)
* [Callback Functions](#callback-functions)
* [Examples](#examples)

## Name
### `nksip_event_compositor`

## Description

This plugin provides full support to implement an Event State Compositor, according to RFC3903. It uses by default the built-in RAM-only store, but can be configured to use any other database implementing callback [sip_publish_store/2](#sip_event_compositor_store2).

_PUBLISH_ will also be added to default generared _Allow_ headers.

## Dependant Plugins

None


## Configuration Values

### SipApp configuration values

Option|Default|Description
---|---|---
nksip_event_compositor_default_expires|60 (secs)|Default expiration for stored events

## API functions

### find/3

```erlang
find(App::nksip:app_id()|term(), AOR::nksip:aor(), Tag::binary()) ->
    {ok, #reg_publish{}} | not_found | {error, term()}.
```

Finds a stored published information.


### request/1
```erlang
request(nksip:request()) ->
    nksip:sipreply().
```

Call this function to process and incoming _PUBLISH_ request. It returns an appropiate response, depending on the registration result.

For example, you could implement the following [sip_publish/2](../reference/callback_functions.md#sip_publish2) callback function:

```erlang
sip_publish(Req, _Call) ->
	case nksip_request:meta(domain, Req) of
		{ok, <<"nksip">>} -> {reply, nksip_event_compositor:process(Req)};
		_ -> {reply, forbidden}
	end.
```

* If No _Sip-If-Match_ header is found:
  * If we have a body, the server will generate a Tag, and store the boy under this AOR and Tag.
  * If we have no body, the request fails
* If _Sip-If-Match_ header is found:
  * If we alredy had a body stored with this Tag, and Expires=0, it is deleted
  * If Expires>0, and we have no body, the content is refreshed
  * If Expires>0, and we have a body, the content is updated
  * If we had no body stored, the request fails


### clear/1

```erlang
clear(nksip:app_name()|nksip:app_id()) -> 
    ok | callback_error | sipapp_not_found.
```

Clear all stored records by a SipApp.




## Callback functions

You can implement any of these callback functions in your SipApp callback module.

### sip_event_compositor_store/2

```erlang
sip_event_compositor_store(StoreOp, AppId) ->
    [RegPublish] | ok | not_found when
        StoreOp :: {get, AOR, Tag} | {put, AOR, Tag, RegPublish, TTL} | 
                   {del, AOR, Tag} | del_all,
        AppId :: nksip:app_id(),
        AOR :: nksip:aor(),
        Tag :: binary(),
        RegPublish :: nksip_event_compositor:reg_publish(),
        TTL :: integer().
```

Called when a operation database must be done on the publisher database. By default the in-memory database is used, but you can impement it to use your own database.

The possible values for Op and their allowed reply are:

Op|Response|Comments
---|---|---
{get, AOR, Tag}|RegPublish &#124; not_found|Retrieve store information this `AOR`, `AppId` and `Tag`.
{put, AOR, Tag, RegPublish, TTL}|ok|Store this information this `AOR`, `AppId` and `Tag`. The record must be automatically deleted after `TTL` seconds.
{del, AOR, Tag}|ok &#124; not_found|Delete stored information for this `AOR`, `AppId` and `Tag`, returning `ok` or `not_found` if it is not found.
del_all|ok|Delete all stored information for this `AppId`.

See the [default implementation](../../plugins/src/nksip_event_compositor_sipapp.erl) as a basis. 


## Examples

```erlang
-module(example).

-include_lib("nksip/include/nksip.hrl").
-include_lib("nksip/plugins/include/nksip_event_compositor.hrl").
-compile([export_all]).

start() ->
    {ok, _} = nksip:start(client1, ?MODULE, [], [
        {from, "sip:client1@nksip"},
        {local_host, "localhost"},
        {transports, [{udp, all, 5060}, {tls, all, 5061}]}
    ]),
    {ok, _} = nksip:start(server, ?MODULE, [], [
        {from, "sip:server@nksip"},
        no_100,
        {plugins, [nksip_event_compositor]},
        {local_host, "127.0.0.1"},
        {transports, [{udp, all, 5070}, {tls, all, 5071}]},
        {events, "nkpublish"}
    ]).


stop() ->
    ok = nksip:stop(client1),
    ok = nksip:stop(server).


test() ->
    {ok, 200, [{sip_etag, ETag1}, {expires, 5}]} = 
        nksip_uac:publish(client1, "sip:user1@127.0.0.1:5070", 
            [{event, "nkpublish"}, {expires, 5}, {body, <<"data1">>}]),

    AOR = {sip, <<"user1">>, <<"127.0.0.1">>},
    {ok, #reg_publish{data = <<"data1">>}} = nksip_event_compositor:find(server, AOR, ETag1),

    % This ETag1 is not at the server
    {ok, 412, []} = nksip_uac:publish(client1, "sip:user1@127.0.0.1:5070", 
            [{event, "nkpublish"}, {sip_if_match, <<"other">>}]),

    {ok, 200, [{sip_etag, ETag1}, {expires, 0}]} = 
        nksip_uac:publish(client1, "sip:user1@127.0.0.1:5070", 
            [{event, "nkpublish"}, {expires, 0}, {sip_if_match, ETag1}]),

    not_found = nksip_event_compositor:find(server, AOR, ETag1),

    {ok, 200, [{sip_etag, ETag2}, {expires, 60}]} = 
        nksip_uac:publish(client1, "sip:user1@127.0.0.1:5070", 
            [{event, "nkpublish"}, {expires, 60}, {body, <<"data2">>}]),

    {ok, #reg_publish{data = <<"data2">>}} = nksip_event_compositor:find(server, AOR, ETag2),

    {ok, 200, [{sip_etag, ETag2}, {expires, 1}]} = 
        nksip_uac:publish(client1, "sip:user1@127.0.0.1:5070", 
            [{event, "nkpublish"}, {expires, 1}, {sip_if_match, ETag2}, {body, <<"data3">>}]),

    {ok, #reg_publish{data = <<"data3">>}} = nksip_event_compositor:find(server, AOR, ETag2),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%  CallBacks (we need no callback module) %%%%%%%%%%%%%%%%%%%%%
```


