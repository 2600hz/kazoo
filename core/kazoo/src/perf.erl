%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(perf).

-export([test/1]).

-define(DEFAULT_HEADERS, [<<"Server-ID">>, <<"Event-Category">>, <<"Event-Name">>
                         , <<"App-Name">>, <<"App-Version">>, <<"Raw-Headers">>, <<"Destination-Server">>
                         , <<"Geo-Location">>, <<"Access-Group">>
                         , <<"Tenant-ID">>]).

-spec test(pos_integer()) -> 'ok'.
test(Times) ->
    Props = [get_prop() || _ <- lists:seq(1, Times)],
    {ElapsedOne, _} = timer:tc(fun() -> [ try_one(Prop) || Prop <- Props] end, []),
    {ElapsedTwo, _} = timer:tc(fun() -> [ try_two(Prop) || Prop <- Props] end, []),
    io:format("Took One: ~b ms and Two: ~b ms~n", [ElapsedOne div Times, ElapsedTwo div Times]).

try_one(Prop) ->
    [ {H, V} || H <- ?DEFAULT_HEADERS,
                (V = props:get_value(H, Prop)) =/= undefined
    ].

try_two(Prop) ->
    lists:foldl(fun(H, Acc) ->
                        case props:get_value(H, Prop) of
                            undefined -> Acc;
                            V -> [{H, V} | Acc]
                        end
                end, [], ?DEFAULT_HEADERS).

get_prop() ->
    get_vals(?DEFAULT_HEADERS).

get_vals(L) ->
    get_vals(L, []).
get_vals([], Acc) ->
    Acc;
get_vals([H|T], Acc) ->
    case rand:uniform(20) of
        X when X < 5 -> get_vals(T, [{H, undefined}|Acc]);
        _ -> get_vals(T, [{H,crypto:strong_rand_bytes(16)}|Acc])
    end.
