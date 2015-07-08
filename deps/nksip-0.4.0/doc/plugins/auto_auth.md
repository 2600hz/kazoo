# UAC Auto Authentication Plugin

* [Name](#name)
* [Description](#description)
* [Dependant Plugins](#dependant-plugins)
* [Configuration Values](#configuration-values)
* [API Functions](#api-functions)
* [Callback Functions](#callback-functions)
* [Examples](#examples)


## Name
### `nksip_uac_auto_auth`


## Description

This plugin provides the capability of, after receiving a 401 or 407 response, automatically retry the request using digest authentication. If, after a successful response, the next proxy or element sends another 401 or 407, a new authentication is added, up to the value configured in `nksip_uac_auto_auth_max_tries`.

You can configure the password to use with the [options](#configuration-values) `pass` or `passes`. When a 401 or 407 response is received, NkSIP finds a password for the _realm_ in the response. If none is found, the password with realm `<<>>` is used.



## Dependant Plugins

None


## Configuration Values

### SipApp configuration values

Option|Default|Description
---|---|---
nksip_uac_auto_auth_max_tries|5|Number of times to attemp the request
pass|-|Pass to use for digest authentication (see bellow)
passes|-|List of passes to use for digest authentication (see bellow)

You can use only one of `pass` or `passes` configuration option. In the first case, it can have the form `Pass::binary()` or `{Realm::binary(), Pass::binary()}`. In the second form, must be a list of any of the previous types.

In case you don't want to use a clear-text function, you can use the function [nksip_auth:make_ha1/3](../../src/nksip_auth.erl) to get a hash of the password that can be used instead of the real password.



### Request sending values

The previous configuration options can also be used for a specific request when [sending a request](../reference/sending_functions.md).

If there is a `pass` or `passes` option in the global configuration values, the new values are added to the global ones.




## API functions

None


## Callback functions

None


## Examples

```erlang
-module(sample).
-include_lib("eunit/include/eunit.hrl").
-include_lib("nksip/include/nksip.hrl").
-compile([export_all]).

start() ->
    {ok, _} = nksip:start(client1, ?MODULE, [], [
        {from, "sip:client1@nksip"},
        {plugins, [nksip_uac_auto_auth]},
        {local_host, "127.0.0.1"},
        {transports, [{udp, all, 5070}]}
    ]),
    {ok, _} = nksip:start(client2, ?MODULE, [], [
        {from, "sip:client2@nksip"},
        {plugins, [nksip_uac_auto_auth]},
        {passes, ["jj", {"client1", "4321"}]},
        {local_host, "127.0.0.1"},
        {transports, [{udp, all, 5071}]}
    ]).


stop() ->
    ok = nksip:stop(client1),
    ok = nksip:stop(client2).


test() ->
    {ok, 401, []} = nksip_uac:options(client1, "sip:127.0.0.1:5071", []),
    {ok, 200, []} = nksip_uac:options(client1, "sip:127.0.0.1:5071", [{pass, "1234"}]),
    {ok, 403, []} = nksip_uac:options(client1, "sip:127.0.0.1:5071", [{pass, "12345"}]),
    {ok, 200, []} = nksip_uac:options(client1, "sip:127.0.0.1:5071", [{pass, {"client2", "1234"}}]),
    {ok, 403, []} = nksip_uac:options(client1, "sip:127.0.0.1:5071", [{pass, {"other", "1234"}}]),

    HA1 = nksip_auth:make_ha1("client1", "1234", "client2"),
    {ok, 200, []} = nksip_uac:options(client1, "sip:127.0.0.1:5071", [{pass, HA1}]),
    
    % Pass is invalid, but there is a valid one in SipApp's options
    {ok, 200, []} = nksip_uac:options(client2, "sip:127.0.0.1:5070", []),
    {ok, 200, []} = nksip_uac:options(client2, "sip:127.0.0.1:5070", [{pass, "kk"}]),
    {ok, 403, []} = nksip_uac:options(client2, "sip:127.0.0.1:5070", [{pass, {"client1", "kk"}}]),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%  CallBacks (servers and clients) %%%%%%%%%%%%%%%%%%%%%


sip_get_user_pass(User, Realm, _Req, _Call) ->
    % Password for any user in realm "client1" is "4321",
    % for any user in realm "client2" is "1234", and for "client3" is "abcd"
    case Realm of 
        <<"client1">> ->
            % A hash can be used instead of the plain password
            nksip_auth:make_ha1(User, "4321", "client1");
        <<"client2">> ->
            "1234";
        <<"client3">> ->
            "abcd";
        _ ->
            false
    end.


% Authorization is only used for "auth" suite
sip_authorize(Auth, Req, _Call) ->
    {ok, App} = nksip_request:app_name(Req),
    BinId = nksip_lib:to_binary(App) ,
    case nksip_lib:get_value({digest, BinId}, Auth) of
        true -> ok;                         % At least one user is authenticated
        false -> forbidden;                 % Failed authentication
        undefined -> {authenticate, BinId}  % No auth header
    end.

```

