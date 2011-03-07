%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 2 Feb 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(whapps_json).

-export([get_value/2, get_value/3]).
-export([set_value/3]).

get_value(Key, Doc) ->
    get_value(Key, Doc, undefined).

get_value([], Doc, _Default) -> Doc;
get_value(Key, Doc, Default) when not is_list(Key)->
    get_value([Key], Doc, Default);
get_value([K|Ks], {struct, Props}, Default) ->
    get_value(Ks, props:get_value(whistle_util:to_binary(K), Props, Default), Default);
get_value([K|Ks], Doc, Default) when is_list(Doc) ->
    case try lists:nth(whistle_util:to_integer(K), Doc) catch _:_ -> undefined end of
	undefined -> Default;
	Doc1 -> get_value(Ks, Doc1, Default)
    end;
get_value(_, _, Default) -> Default.


%% Figure out how to set the current key among a list of objects
set_value(Key, Value, Doc) when not is_list(Key) ->
    set_value([Key], Value, Doc);
set_value([Key|T], Value, [{struct, _}|_]=Doc) ->
    Key1 = whistle_util:to_integer(Key),
    case Key1 > length(Doc) of
        %% The object index does not exist so try to add a new one to the list
        true ->
            try
                %% Create a new object with the next key as a property
                NxtKey = whistle_util:to_binary(hd(T)),
                Doc ++ [set_value(T, Value, {struct, [{NxtKey, []}]})]
            catch
                %% There are no more keys in the list, add it unless not an object
                error:badarg ->
                    _ = try {struct, _} = Value catch _:_ -> error(badarg) end,
                    Doc ++ [Value]
            end;
        %% The object index exists so iterate into the object and updat it
        false ->
            element(1, lists:mapfoldl(fun(E, {Pos, Pos}) ->
                                             {set_value(T, Value, E), {Pos + 1, Pos}};
                                         (E, {Pos, Idx}) ->
                                             {E, {Pos + 1, Idx}}
                                      end, {1, Key1}, Doc))
    end;
%% Figure out how to set the current key in an existing object
set_value([Key|T], Value, {struct, Props}) ->
    Key1 = whistle_util:to_binary(Key),
    case lists:keyfind(Key1, 1, Props) of
        {Key1, {struct, _}=V1} ->
            %% Replace or add a property in an object in the object at this key
            {struct, lists:keyreplace(Key1, 1, Props, {Key1, set_value(T, Value, V1)})};
        {Key1, V1} when is_list(V1) ->
            %% Replace or add a member in an array in the object at this key
            {struct, lists:keyreplace(Key1, 1, Props, {Key1, set_value(T, Value, V1)})};
        {Key1, _} when T == [] ->
            %% This is the final key and the objects property should just be
            %% replaced
            {struct, lists:keyreplace(Key1, 1, Props, {Key1, Value})};
        {Key1, _} ->
            %% This is not the final key and the objects property should just be
            %% replaced so continue looping the keys creating the necessary json as we go
            {struct, lists:keyreplace(Key1, 1, Props, {Key1, set_value(T, Value, {struct, []})})};
        false when T == [] ->
            %% This is the final key and doesnt already exist, just add it to this
            %% objects existing properties
            {struct, Props ++ [{Key1, Value}]};
        false ->
            %% This is not the final key and this object does not have this key
            %% so continue looping the keys creating the necessary json as we go
            {struct, Props ++ [{Key1, set_value(T, Value, {struct, []})}]}
    end;
%% There are no more keys to iterate through! Override the value here...
set_value([], Value, _Doc) -> Value.


%% EUNIT TESTING
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

-define(D1, {struct, [{<<"d1k1">>, "d1v1"}, {<<"d1k2">>, d1v2}, {<<"d1k3">>, ["d1v3.1", "d1v3.2", "d1v3.3"]}]}).
-define(D2, {struct, [{<<"d2k1">>, 1}, {<<"d2k2">>, 3.14}, {<<"sub_d1">>, ?D1}]}).
-define(D3, {struct, [{<<"d3k1">>, <<"d3v1">>}, {<<"d3k2">>, []}, {<<"sub_docs">>, [?D1, ?D2]}]}).
-define(D4, [?D1, ?D2, ?D3]).

get_value_test() ->
    %% Basic first level key
    ?assertEqual(undefined, get_value(["d1k1"], [])),
    ?assertEqual("d1v1",    get_value(["d1k1"], ?D1)),
    ?assertEqual(undefined, get_value(["d1k1"], ?D2)),
    ?assertEqual(undefined, get_value(["d1k1"], ?D3)),
    ?assertEqual(undefined, get_value(["d1k1"], ?D4)),
    %% Basic nested key
    ?assertEqual(undefined, get_value(["sub_d1", "d1k2"], [])),
    ?assertEqual(undefined, get_value(["sub_d1", "d1k2"], ?D1)),
    ?assertEqual(d1v2,      get_value(["sub_d1", "d1k2"], ?D2)),
    ?assertEqual(undefined, get_value(["sub_d1", "d1k2"], ?D3)),
    ?assertEqual(undefined, get_value(["sub_d1", "d1k2"], ?D4)),
    %% Get the value in an object in an array in another object that is part of
    %% an array of objects
    ?assertEqual(undefined, get_value([3, "sub_docs", 2, "d2k2"], [])),
    ?assertEqual(undefined, get_value([3, "sub_docs", 2, "d2k2"], ?D1)),
    ?assertEqual(undefined, get_value([3, "sub_docs", 2, "d2k2"], ?D2)),
    ?assertEqual(undefined, get_value([3, "sub_docs", 2, "d2k2"], ?D3)),
    ?assertEqual(3.14,      get_value([3, "sub_docs", 2, "d2k2"], ?D4)),
    %% Get the value in an object in an array in another object that is part of
    %% an array of objects, but change the default return if it is not present.
    %% Also tests the ability to have indexs represented as strings
    ?assertEqual(<<"not">>, get_value([3, "sub_docs", "2", "d2k2"], [], <<"not">>)),
    ?assertEqual(<<"not">>, get_value([3, "sub_docs", "2", "d2k2"], ?D1, <<"not">>)),
    ?assertEqual(<<"not">>, get_value([3, "sub_docs", "2", "d2k2"], ?D2, <<"not">>)),
    ?assertEqual(<<"not">>, get_value([3, "sub_docs", "2", "d2k2"], ?D3, <<"not">>)),
    ?assertEqual(3.14,      get_value([3, "sub_docs", "2", "d2k2"], ?D4, <<"not">>)).

-define(T2R1, {struct, [{<<"d1k1">>, "d1v1"}, {<<"d1k2">>, <<"update">>}, {<<"d1k3">>, ["d1v3.1", "d1v3.2", "d1v3.3"]}]}).
-define(T2R2, {struct, [{<<"d1k1">>, "d1v1"}, {<<"d1k2">>, d1v2}, {<<"d1k3">>, ["d1v3.1", "d1v3.2", "d1v3.3"]}, {<<"d1k4">>, new_value}]}).
-define(T2R3, {struct, [{<<"d1k1">>, "d1v1"}, {<<"d1k2">>, {struct, [{<<"new_key">>, added_value}]}}, {<<"d1k3">>, ["d1v3.1", "d1v3.2", "d1v3.3"]}]}).
-define(T2R4, {struct, [{<<"d1k1">>, "d1v1"}, {<<"d1k2">>, d1v2}, {<<"d1k3">>, ["d1v3.1", "d1v3.2", "d1v3.3"]}, {<<"d1k4">>, {struct, [{<<"new_key">>, added_value}]}}]}).

set_value_object_test() ->
    %% Test setting an existing key
    ?assertEqual(?T2R1, set_value(["d1k2"], <<"update">>, ?D1)),
    %% Test setting a non-existing key
    ?assertEqual(?T2R2, set_value(["d1k4"], new_value, ?D1)),
    %% Test setting an existing key followed by a non-existant key
    ?assertEqual(?T2R3, set_value(["d1k2", "new_key"], added_value, ?D1)),
    %% Test setting a non-existing key followed by another non-existant key
    ?assertEqual(?T2R4, set_value(["d1k4", "new_key"], added_value, ?D1)).

-define(D5,   [{struct,[{<<"k1">>, v1}]}, {struct, [{<<"k2">>, v2}]}]).
-define(T3R1, [{struct,[{<<"k1">>,test}]},{struct,[{<<"k2">>,v2}]}]).
-define(T3R2, [{struct,[{<<"k1">>,v1},{<<"pi">>, 3.14}]},{struct,[{<<"k2">>,v2}]}]).
-define(T3R3, [{struct,[{<<"k1">>,v1},{<<"callerid">>,{struct,[{<<"name">>,"2600hz"}]}}]},{struct,[{<<"k2">>,v2}]}]).
-define(T3R4, [{struct,[{<<"k1">>,v1}]},{struct,[{<<"k2">>,"updated"}]}]).
-define(T3R5, [{struct,[{<<"k1">>,v1}]},{struct,[{<<"k2">>,v2}]},{struct,[{<<"new_key">>,"added"}]}]).

set_value_multiple_object_test() ->
    %% Set an existing key in the first json_object()
    ?assertEqual(?T3R1, set_value([1, "k1"], test, ?D5)),
    %% Set a non-existing key in the first json_object()
    ?assertEqual(?T3R2, set_value([1, "pi"], 3.14, ?D5)),
    %% Set a non-existing key followed by another non-existant key in the first json_object()
    ?assertEqual(?T3R3, set_value([1, "callerid", "name"], "2600hz", ?D5)),
    %% Set an existing key in the second json_object()
    ?assertEqual(?T3R4, set_value([2, "k2"], "updated", ?D5)),
    %% Set a non-existing key in a non-existing json_object()
    ?assertEqual(?T3R5, set_value([3, "new_key"], "added", ?D5)).

-endif.
