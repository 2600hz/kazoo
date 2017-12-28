%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(registrar_maintenance).

-export([device_by_ip/1]).
-export([set_listeners/1]).
-export([refresh_views/0]).

-include("reg.hrl").

-spec device_by_ip(text()) -> 'ok'.
device_by_ip(IP) when not is_binary(IP) ->
    device_by_ip(kz_term:to_binary(IP));
device_by_ip(IP) ->
    io:format("Looking up IP: ~s~n", [IP]),
    case reg_route_req:lookup_account_by_ip(IP) of
        {'ok', AccountProps} ->
            pretty_print_device_by_ip(AccountProps);
        {'error', _E} ->
            io:format("Not found: ~p~n", [_E])
    end.

-spec pretty_print_device_by_ip(kz_proplist()) -> 'ok'.
pretty_print_device_by_ip([]) -> 'ok';
pretty_print_device_by_ip([{Key, Value}|Props]) ->
    io:format("~-39s: ~s~n", [Key, kz_term:to_binary(Value)]),
    pretty_print_device_by_ip(Props).

-spec set_listeners(integer() | binary()) -> 'ok'.
set_listeners(Count) when is_binary(Count) ->
    set_listeners(kz_term:to_integer(Count));
set_listeners(Count) ->
    kapps_config:set(?CONFIG_CAT, <<"listeners">>, Count),
    registrar_shared_listener_sup:set_listeners(Count).

-spec refresh_views() -> 'ok'.
refresh_views() ->
    View = kapps_util:get_view_json('registrar', <<"credentials.json">>),
    _ = kapps_util:update_views(?KZ_SIP_DB, [View], 'false'),
    'ok'.
