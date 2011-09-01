%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Provides utility methods for monitoring
%%% @end
%%% Created : 17 Nov 2010 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(monitor_util).

-export([to_binary/1]).
-export([prop_update/3, prop_updates/2]).
-export([to_list/1]).
-export([uuid/0, uuid_v4/0, uuid_to_string/1, uuid_get_parts/1]).

-import(random).

to_binary(Term) ->
    wh_util:to_binary(Term).

to_list(Term) ->
    wh_util:to_list(Term).

prop_update(Key, Value, Prop) ->
    [{Key, Value} | proplists:delete(Key, Prop)].

prop_updates([], Prop) ->
    Prop;

prop_updates([{Key, Value}|T], Prop) ->
    prop_updates(T, prop_update(Key, Value, Prop)).

uuid() ->
    uuid_to_string(uuid_v4()).

uuid_v4() ->
    uuid_v4(random:uniform(round(math:pow(2, 48))) - 1, random:uniform(round(math:pow(2, 12))) - 1, random:uniform(round(math:pow(2, 32))) - 1, random:uniform(round(math:pow(2, 30))) - 1).

uuid_v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

uuid_to_string(U) ->
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", uuid_get_parts(U))).

uuid_get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    [TL, TM, THV, CSR, CSL, N].
