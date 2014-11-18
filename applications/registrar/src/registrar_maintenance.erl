%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(registrar_maintenance).

-export([device_by_ip/1]).

-include("reg.hrl").

-spec device_by_ip(text()) -> 'ok'.
device_by_ip(IP) when not is_binary(IP) ->
    device_by_ip(wh_util:to_binary(IP));
device_by_ip(IP) ->
    io:format("Looking up IP: ~s~n", [IP]),
    case reg_authn_req:lookup_account_by_ip(IP) of
	{'ok', AccountProps} ->
	    pretty_print_device_by_ip(AccountProps);
	{'error', _E} ->
	    io:format("Not found: ~p~n", [_E])
    end.

-spec pretty_print_device_by_ip(wh_proplist()) -> 'ok'.
pretty_print_device_by_ip([]) -> 'ok';
pretty_print_device_by_ip([{Key, Value}|Props]) ->
    io:format("~-39s: ~s~n", [Key, wh_util:to_binary(Value)]),
    pretty_print_device_by_ip(Props).
