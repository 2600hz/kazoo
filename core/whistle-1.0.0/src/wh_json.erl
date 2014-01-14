%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz
%%% @doc
%%% proplists-like interface to json objects
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_json).

%% don't import the get_keys/1 that fetches keys from the process dictionary
-compile({'no_auto_import', [get_keys/1]}).

-export([to_proplist/1, to_proplist/2]).
-export([to_querystring/1]).
-export([recursive_to_proplist/1]).

-export([get_first_defined/2, get_first_defined/3]).
-export([get_binary_boolean/2, get_binary_boolean/3]).
-export([get_integer_value/2, get_integer_value/3]).
-export([get_number_value/2, get_number_value/3]).
-export([get_float_value/2, get_float_value/3]).
-export([get_binary_value/2, get_binary_value/3]).
-export([get_lower_binary/2, get_lower_binary/3]).
-export([get_atom_value/2, get_atom_value/3]).
-export([get_string_value/2, get_string_value/3]).
-export([get_json_value/2, get_json_value/3]).
-export([is_true/2, is_true/3, is_false/2, is_false/3, is_empty/1]).

-export([filter/2, filter/3
         ,map/2
         ,foldl/3
         ,find/2, find/3
         ,foreach/2
        ]).
-export([get_ne_value/2, get_ne_value/3]).
-export([get_value/2, get_value/3, get_values/1]).
-export([get_keys/1, get_keys/2]).
-export([set_value/3, set_values/2, new/0]).
-export([delete_key/2, delete_key/3, delete_keys/2]).
-export([merge_recursive/1
         ,merge_recursive/2
         ,merge_recursive/3
        ]).

-export([from_list/1, merge_jobjs/2]).

-export([load_fixture_from_file/2]).

-export([normalize_jobj/1
         ,normalize/1
         ,normalize_key/1
         ,is_json_object/1, is_json_object/2
         ,is_valid_json_object/1
         ,is_json_term/1
        ]).
-export([public_fields/1
         ,private_fields/1
         ,is_private_key/1
        ]).

-export([encode/1]).
-export([decode/1, decode/2]).

%% not for public use
-export([prune/2, no_prune/2]).

-export([flatten/3]).

-include_lib("whistle/include/wh_log.hrl").

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("wh_json.hrl").

-export_type([json_string/0, json_strings/0
              ,json_term/0, json_terms/0
              ,json_proplist/0, json_proplist_k/1, json_proplist_kv/2
              ,json_key/0
              ,object/0, objects/0
              ,key/0, keys/0
             ]).

-spec new() -> object().
new() -> ?JSON_WRAPPER([]).

-spec encode(json_term()) -> text().
encode(JObj) -> ejson:encode(JObj).

-spec decode(iolist() | ne_binary()) -> object().
-spec decode(iolist() | ne_binary(), ne_binary()) -> object().

decode(Thing) when is_list(Thing) orelse is_binary(Thing) ->
    decode(Thing, ?DEFAULT_CONTENT_TYPE).

decode(JSON, <<"application/json">>) ->
    try ejson:decode(JSON) of
        JObj -> JObj
    catch
        'throw':{'error',{_Loc, 'invalid_string'}}=E ->
            lager:debug("invalid string(near char ~p) in input, checking for unicode", [_Loc]),
            try_converting(JSON, E);
        'throw':{'invalid_json',{{'error',{1, Err}}, _Text}} ->
            lager:debug("~s: ~s", [Err, _Text]),
            throw({'error', Err});
        'throw':{'invalid_json',{{'error',{Loc, Err}}, Text}} ->
            Size = erlang:byte_size(Text),
            Part = binary:part(Text, Loc, Size-Loc),
            lager:debug("~s near char # ~b of ~b ('~s'): ~s", [Err, Loc, Size, Part, Text]),
            try_converting(JSON, {'error', Err});
        'throw':E ->
            lager:debug("thrown decoder error: ~p", [E]),
            throw(E)
    end.
try_converting(JSON, E) ->
    case unicode:bom_to_encoding(JSON) of
        {'latin1', 0} ->
            lager:debug("json is latin1, trying as utf8"),
            case unicode:characters_to_binary(JSON, 'latin1', 'utf8') of
                Converted when is_binary(Converted) ->
                    case unicode:bom_to_encoding(Converted) of
                        {'latin1', 0} -> throw(E);
                        _ -> decode(Converted)
                    end;
                _O -> lager:debug("failed to char_to_bin: ~p", [_O]), throw(E)
            end;
        _Enc ->
            lager:debug("unknown encoding: ~p", [_Enc]),
            throw(E)
    end.

-spec is_empty(term()) -> boolean().
is_empty(MaybeJObj) ->
    MaybeJObj =:= ?EMPTY_JSON_OBJECT.

-spec is_json_object(term()) -> boolean().
-spec is_json_object(key(), term()) -> boolean().
is_json_object(?JSON_WRAPPER(P)) when is_list(P) -> 'true';
is_json_object(_) -> 'false'.

is_json_object(Key, JObj) ->
    is_json_object(get_value(Key, JObj)).

-spec is_valid_json_object(term()) -> boolean().
is_valid_json_object(MaybeJObj) ->
    try
        lists:all(fun(K) ->
                          is_json_term(get_value([K], MaybeJObj))
                  end, ?MODULE:get_keys(MaybeJObj))
    catch
        'throw':_ -> 'false';
        'error':_ -> 'false'
    end.

-spec is_json_term(json_term()) -> boolean().
is_json_term('undefined') -> throw({'error', 'no_undefined_atom_in_jobj_please'});
is_json_term(V) when is_atom(V) -> 'true';
is_json_term(V) when is_binary(V) -> 'true';
is_json_term(V) when is_bitstring(V) -> 'true';
is_json_term(V) when is_integer(V) -> 'true';
is_json_term(V) when is_float(V) -> 'true';
is_json_term(Vs) when is_list(Vs) ->
    lists:all(fun is_json_term/1, Vs);
is_json_term({'json', IOList}) when is_list(IOList) -> 'true';
is_json_term(MaybeJObj) ->
    is_json_object(MaybeJObj).

%% converts top-level proplist to json object, but only if sub-proplists have been converted
%% first.
%% For example:
%% [{a, b}, {c, [{d, e}]}]
%% would be converted to json by
%% wh_json:from_list([{a,b}, {c, wh_json:from_list([{d, e}])}]).
%% the sub-proplist [{d,e}] needs converting before being passed to the next level
-spec from_list(json_proplist()) -> object().
from_list([]) -> new();
from_list(L) when is_list(L) -> ?JSON_WRAPPER(L).

%% only a top-level merge
%% merges JObj1 into JObj2
-spec merge_jobjs(object(), object()) -> object().
merge_jobjs(?JSON_WRAPPER(Props1)=_JObj1, ?JSON_WRAPPER(_)=JObj2) ->
    lists:foldr(fun({K, V}, JObj2Acc) ->
                        set_value(K, V, JObj2Acc)
                end, JObj2, Props1).

-type merge_pred() :: fun(({json_term(), json_term()}) -> boolean()).

-spec merge_recursive(objects()) -> object().
merge_recursive(JObjs) ->
    merge_recursive(JObjs, fun(_, _) -> 'true' end).

-spec merge_recursive(objects(), merge_pred()) -> object();
                     (object(), object()) -> object().
merge_recursive([J|JObjs], Pred) when is_function(Pred, 2) ->
    lists:foldl(fun(JObj2, JObj1) ->
                        merge_recursive(JObj1, JObj2, Pred)
                end, J, JObjs);
merge_recursive(JObj1, JObj2) ->
    merge_recursive(JObj1, JObj2, fun(_, _) -> 'true' end).

-spec merge_recursive(object(), object(), merge_pred()) -> object().
merge_recursive(JObj1, JObj2, Pred) ->
    merge_recursive(JObj1, JObj2, Pred, []).

%% inserts values from JObj2 into JObj1
-spec merge_recursive(object(), object() | json_term(), merge_pred(), json_strings()) -> object().
merge_recursive(JObj1, ?JSON_WRAPPER(Prop2), Pred, Keys) ->
    lists:foldr(fun(Key, J) ->
                        merge_recursive(J, props:get_value(Key, Prop2), Pred, [Key|Keys])
                end, JObj1, props:get_keys(Prop2));
merge_recursive(JObj1, Value, Pred, Keys) ->
    Syek = lists:reverse(Keys),
    case Pred(get_value(Syek, JObj1), Value) of
        'false' -> JObj1;
        'true' -> set_value(Syek, Value, JObj1)
    end.

-spec to_proplist(object() | objects()) -> json_proplist() | json_proplists().
-spec to_proplist(key(), object() | objects()) -> json_proplist() | json_proplists().
%% Convert a json object to a proplist
%% only top-level conversion is supported
to_proplist(JObjs) when is_list(JObjs) -> [to_proplist(JObj) || JObj <- JObjs];
to_proplist(?JSON_WRAPPER(Prop)) -> Prop.

%% convert everything starting at a specific key
to_proplist(Key, JObj) -> to_proplist(get_json_value(Key, JObj, new())).

-spec recursive_to_proplist(object() | wh_proplist()) -> wh_proplist().
recursive_to_proplist(?JSON_WRAPPER(Props)) ->
    [{K, recursive_to_proplist(V)} || {K, V} <- Props];
recursive_to_proplist(Props) when is_list(Props) ->
    [recursive_to_proplist(V) || V <- Props];
recursive_to_proplist(Else) -> Else.

%% Convert {key1:val1,key2:[v2_1, v2_2],key3:{k3_1:v3_1}} =>
%%   key=val&key2[]=v2_1&key2[]=v2_2&key3[key3_1]=v3_1
-spec to_querystring(object()) -> iolist().
to_querystring(JObj) -> to_querystring(JObj, <<>>).

%% if Prefix is empty, don't wrap keys in array tags, otherwise Prefix[key]=value
-spec to_querystring(object(), iolist() | binary()) -> iolist().
to_querystring(JObj, Prefix) ->
    {Vs, Ks} = get_values(JObj),
    fold_kvs(Ks, Vs, Prefix, []).

%% foreach key/value pair, encode the key/value with the prefix and prepend the &
%% if the last key/value pair, encode the key/value with the prefix, prepend to accumulator
%% and reverse the list (putting the key/value at the end of the list)
-spec fold_kvs(json_strings(), json_terms(), binary() | iolist(), iolist()) -> iolist().
fold_kvs([], [], _, Acc) -> Acc;
fold_kvs([K], [V], Prefix, Acc) -> lists:reverse([encode_kv(Prefix, K, V) | Acc]);
fold_kvs([K|Ks], [V|Vs], Prefix, Acc) ->
    fold_kvs(Ks, Vs, Prefix, [<<"&">>, encode_kv(Prefix, K, V) | Acc]).

-spec encode_kv(iolist() | binary(), ne_binary(), json_term() | json_terms()) -> iolist().
%% If a list of values, use the []= as a separator between the key and each value
encode_kv(Prefix, K, Vs) when is_list(Vs) ->
    encode_kv(Prefix, wh_util:to_binary(K), Vs, <<"[]=">>, []);
%% if the value is a "simple" value, just encode it (url-encoded)
encode_kv(Prefix, K, V) when is_binary(V) orelse is_number(V) ->
    encode_kv(Prefix, K, <<"=">>, mochiweb_util:quote_plus(V));

% key:{k1:v1, k2:v2} => key[k1]=v1&key[k2]=v2
%% if no prefix is present, use just key to prefix the key/value pairs in the jobj
encode_kv(<<>>, K, ?JSON_WRAPPER(_)=JObj) -> to_querystring(JObj, [K]);
%% if a prefix is defined, nest the key in square brackets
encode_kv(Prefix, K, ?JSON_WRAPPER(_)=JObj) -> to_querystring(JObj, [Prefix, <<"[">>, K, <<"]">>]).

-spec encode_kv(iolist() | binary(), ne_binary(), ne_binary(), string() | binary()) -> iolist().
encode_kv(<<>>, K, Sep, V) -> [wh_util:to_binary(K), Sep, wh_util:to_binary(V)];
encode_kv(Prefix, K, Sep, V) -> [Prefix, <<"[">>, wh_util:to_binary(K), <<"]">>, Sep, wh_util:to_binary(V)].

-spec encode_kv(iolist() | binary(), ne_binary(), [string(),...] | [], ne_binary(), iolist()) -> iolist().
encode_kv(Prefix, K, [V], Sep, Acc) ->
    lists:reverse([ encode_kv(Prefix, K, Sep, mochiweb_util:quote_plus(V)) | Acc]);
encode_kv(Prefix, K, [V|Vs], Sep, Acc) ->
    encode_kv(Prefix, K, Vs, Sep, [ <<"&">>, encode_kv(Prefix, K, Sep, mochiweb_util:quote_plus(V)) | Acc]);
encode_kv(_, _, [], _, Acc) -> lists:reverse(Acc).

-spec get_json_value(key(), object()) -> api_object().
-spec get_json_value(key(), object(), Default) -> Default | object().
get_json_value(Key, JObj) -> get_json_value(Key, JObj, 'undefined').
get_json_value(Key, ?JSON_WRAPPER(_)=JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        ?JSON_WRAPPER(_)=V -> V;
        _ -> Default
    end.

-spec filter(fun(({json_string(), json_term()}) -> boolean()), object()) ->
                          object().
-spec filter(fun(({json_string(), json_term()}) -> boolean()), object(), key()) ->
                          object() | objects().
filter(Pred, ?JSON_WRAPPER(Prop)) when is_function(Pred, 1) ->
    from_list([E || {_,_}=E <- Prop, Pred(E)]).

filter(Pred, ?JSON_WRAPPER(_)=JObj, Keys) when is_list(Keys),
                                               is_function(Pred, 1) ->
    set_value(Keys, filter(Pred, get_json_value(Keys, JObj)), JObj);
filter(Pred, JObj, Key) -> filter(Pred, JObj, [Key]).

-spec map(fun((json_string(), json_term()) -> {json_string(), json_term()}), object()) -> object().
map(F, ?JSON_WRAPPER(Prop)) -> from_list([ F(K, V) || {K,V} <- Prop]).

-spec foreach(fun(({json_key(), json_term()}) -> any()), object()) -> 'ok'.
foreach(F, ?JSON_WRAPPER(Prop)) when is_function(F, 1) -> lists:foreach(F, Prop).

-spec foldl(fun((key(), json_term(), any()) -> any()), any(), object()) -> any().
foldl(F, Acc0, ?JSON_WRAPPER([])) when is_function(F, 3) -> Acc0;
foldl(F, Acc0, ?JSON_WRAPPER(Prop)) when is_function(F, 3) ->
    lists:foldl(fun({Key, Value}, Acc1) -> F(Key, Value, Acc1) end, Acc0, Prop).

-spec get_string_value(key(), object() | objects()) -> 'undefined' | list().
-spec get_string_value(key(), object(), Default) -> list() | Default.
get_string_value(Key, JObj) -> get_string_value(Key, JObj, 'undefined').

get_string_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> wh_util:to_list(Value)
    end.

-spec get_binary_value(key(), object() | objects()) -> 'undefined' | binary().
-spec get_binary_value(key(), object() | objects(), Default) -> binary() | Default.
get_binary_value(Key, JObj) -> get_binary_value(Key, JObj, 'undefined').
get_binary_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> wh_util:to_binary(Value)
    end.

-spec get_lower_binary(key(), object() | objects()) -> 'undefined' | binary().
-spec get_lower_binary(key(), object() | objects(), Default) -> binary() | Default.
get_lower_binary(Key, JObj) -> get_lower_binary(Key, JObj, 'undefined').
get_lower_binary(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> wh_util:to_lower_binary(Value)
    end.

%% must be an existing atom
-spec get_atom_value(key(), object() | objects()) -> 'undefined' | atom().
-spec get_atom_value(key(), object() | objects(), Default) -> atom() | Default.
get_atom_value(Key, JObj) -> get_atom_value(Key, JObj, 'undefined').
get_atom_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> wh_util:to_atom(Value)
    end.

-spec get_integer_value(key(), object() | objects()) -> 'undefined' | integer().
get_integer_value(Key, JObj) ->
    case get_value(Key, JObj) of
        'undefined' -> 'undefined';
        Value -> wh_util:to_integer(Value)
    end.

-spec get_integer_value(key(), object() | objects(), Default) -> integer() | Default.
get_integer_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> wh_util:to_integer(Value)
    end.

-spec get_number_value(key(), object() | objects()) -> 'undefined' | number().
get_number_value(Key, JObj) ->
    case get_value(Key, JObj) of
        'undefined' -> 'undefined';
        Value -> wh_util:to_number(Value)
    end.

-spec get_number_value(key(), object() | objects(), Default) -> number() | Default.
get_number_value(Key, JObj, Default) when is_number(Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> wh_util:to_number(Value)
    end.

-spec get_float_value(key(), object() | objects()) -> 'undefined' | float().
get_float_value(Key, JObj) ->
    case get_value(Key, JObj) of
        'undefined' -> 'undefined';
        Value -> wh_util:to_float(Value)
    end.

-spec get_float_value(key(), object() | objects(), Default) -> float() | Default.
get_float_value(Key, JObj, Default) when is_float(Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> wh_util:to_float(Value)
    end.

-spec is_false(key(), object() | objects()) -> boolean().
-spec is_false(key(), object() | objects(), Default) -> boolean() | Default.
is_false(Key, JObj) ->
    wh_util:is_false(get_value(Key, JObj)).

is_false(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        V -> wh_util:is_false(V)
    end.

-spec is_true(key(), object() | objects()) -> boolean().
-spec is_true(key(), object() | objects(), Default) -> boolean() | Default.
is_true(Key, JObj) ->
    is_true(Key, JObj, 'false').
is_true(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        V -> wh_util:is_true(V)
    end.

-spec get_binary_boolean(key(), wh_json:object() | objects()) -> api_binary().
-spec get_binary_boolean(key(), wh_json:object() | objects(), Default) -> Default | ne_binary().
get_binary_boolean(Key, JObj) ->
    get_binary_boolean(Key, JObj, 'undefined').

get_binary_boolean(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> wh_util:to_binary(wh_util:is_true(Value))
    end.

-spec get_keys(object()) -> json_strings().
-spec get_keys(key(), object()) -> [pos_integer(),...] | json_strings().
get_keys(JObj) -> get_keys1(JObj).

get_keys([], JObj) -> get_keys1(JObj);
get_keys(Keys, JObj) -> get_keys1(get_value(Keys, JObj, new())).

-spec get_keys1(list() | object()) -> [pos_integer(),...] | json_strings().
get_keys1(KVs) when is_list(KVs) -> lists:seq(1,length(KVs));
get_keys1(JObj) -> props:get_keys(to_proplist(JObj)).

-spec get_ne_value(key(), object() | objects()) -> json_term() | 'undefined'.
-spec get_ne_value(key(), object() | objects(), Default) -> json_term() | Default.
get_ne_value(Key, JObj) -> get_ne_value(Key, JObj, 'undefined').

get_ne_value(Key, JObj, Default) ->
    Value = get_value(Key, JObj),
    case wh_util:is_empty(Value) of
        'true' -> Default;
        'false' -> Value
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find first json object that has a non_empty value for Key.
%% Returns the value at Key
%% @end
%%--------------------------------------------------------------------
-spec find(key(), objects()) -> json_term() | 'undefined'.
-spec find(key(), objects(), Default) -> json_term() | Default.

find(Key, Docs) -> find(Key, Docs, 'undefined').

find(_, [], Default) -> Default;
find(Key, [JObj|JObjs], Default) when is_list(JObjs) ->
    case get_value(Key, JObj) of
        'undefined' -> find(Key, JObjs, Default);
        V -> V
    end.

-spec get_first_defined(keys(), object()) -> 'undefined' | json_term().
-spec get_first_defined(keys(), object(), Default) -> Default | json_term().
get_first_defined(Keys, JObj) -> get_first_defined(Keys, JObj, 'undefined').

get_first_defined([], _JObj, Default) -> Default;
get_first_defined([H|T], JObj, Default) ->
    case ?MODULE:get_value(H, JObj) of
        'undefined' -> get_first_defined(T, JObj, Default);
        V -> V
    end.

-spec get_value(key(), object() | objects()) ->
                       json_term() | 'undefined'.
-spec get_value(key(), object() | objects(), Default) ->
                       json_term() | Default.
get_value(Key, JObj) ->
    get_value(Key, JObj, 'undefined').

get_value([Key|Ks], L, Default) when is_list(L) ->
    try
        get_value1(Ks, lists:nth(wh_util:to_integer(Key), L), Default)
    catch
        'error':'badarg' -> Default;
        'error':'badarith' -> Default;
        'error':'function_clause' -> Default
    end;
get_value(K, Doc, Default) -> get_value1(K, Doc, Default).

-spec get_value1(key(), object() | objects(), Default) ->
                        json_term() | Default.
get_value1([], JObj, _Default) ->
    JObj;
get_value1(Key, JObj, Default) when not is_list(Key)->
    get_value1([Key], JObj, Default);
get_value1([K|Ks], JObjs, Default) when is_list(JObjs) ->
    try lists:nth(wh_util:to_integer(K), JObjs) of
        'undefined' -> Default;
        JObj1 -> get_value1(Ks, JObj1, Default)
    catch
        _:_ -> Default
    end;
get_value1([K|Ks], ?JSON_WRAPPER(Props)=_JObj, Default) ->
    get_value1(Ks, props:get_value(K, Props, Default), Default);
get_value1(_, _, Default) -> Default.

%% split the json object into values and the corresponding keys
-spec get_values(object()) -> {json_terms(), json_strings()}.
get_values(JObj) ->
    lists:foldr(fun(Key, {Vs, Ks}) ->
                        {[get_value(Key, JObj)|Vs], [Key|Ks]}
                end, {[], []}, ?MODULE:get_keys(JObj)).

%% Figure out how to set the current key among a list of objects
-spec set_values(json_proplist(), object()) -> object().
set_values(KVs, JObj) when is_list(KVs) ->
    lists:foldr(fun({K,V}, JObj0) -> set_value(K, V, JObj0) end, JObj, KVs).

-spec set_value(key(), json_term(), object() | objects()) -> object() | objects().
set_value(Keys, Value, JObj) when is_list(Keys) -> set_value1(Keys, Value, JObj);
set_value(Key, Value, JObj) -> set_value1([Key], Value, JObj).

-spec set_value1(json_strings() | [integer(),...], json_term(), object() | objects()) -> object() | objects().
set_value1([Key|_]=Keys, Value, []) when not is_integer(Key) -> set_value1(Keys, Value, new());
set_value1([Key|T], Value, JObjs) when is_list(JObjs) ->
    Key1 = wh_util:to_integer(Key),
    case Key1 > length(JObjs) of
        %% The object index does not exist so try to add a new one to the list
        'true' ->
            try
                %% Create a new object with the next key as a property
                JObjs ++ [ set_value1(T, Value, set_value1([hd(T)], [], new())) ]
            catch
                %% There are no more keys in the list, add it unless not an object
                _:_ ->
                    try
                        JObjs ++ [Value]
                    catch _:_ -> erlang:error('badarg')
                    end
            end;
        %% The object index exists so iterate into the object and update it
        'false' ->
            element(1, lists:mapfoldl(fun(E, {Pos, Pos}) ->
                                              {set_value1(T, Value, E), {Pos + 1, Pos}};
                                         (E, {Pos, Idx}) ->
                                              {E, {Pos + 1, Idx}}
                                      end, {1, Key1}, JObjs))
    end;
%% Figure out how to set the current key in an existing object
set_value1([Key1|T], Value, ?JSON_WRAPPER(Props)) ->
    case lists:keyfind(Key1, 1, Props) of
        {Key1, ?JSON_WRAPPER(_)=V1} ->
            %% Replace or add a property in an object in the object at this key
            ?JSON_WRAPPER(lists:keyreplace(Key1, 1, Props, {Key1, set_value1(T, Value, V1)}));
        {Key1, V1} when is_list(V1) ->
            %% Replace or add a member in an array in the object at this key
            ?JSON_WRAPPER(lists:keyreplace(Key1, 1, Props, {Key1, set_value1(T, Value, V1)}));
        {Key1, _} when T == [] ->
            %% This is the final key and the objects property should just be replaced
            ?JSON_WRAPPER(lists:keyreplace(Key1, 1, Props, {Key1, Value}));
        {Key1, _} ->
            %% This is not the final key and the objects property should just be
            %% replaced so continue looping the keys creating the necessary json as we go
            ?JSON_WRAPPER(lists:keyreplace(Key1, 1, Props, {Key1, set_value1(T, Value, new())}));
        'false' when T == [] ->
            %% This is the final key and doesnt already exist, just add it to this
            %% objects existing properties
            ?JSON_WRAPPER(Props ++ [{Key1, Value}]);
        'false' ->
            %% This is not the final key and this object does not have this key
            %% so continue looping the keys creating the necessary json as we go
            ?JSON_WRAPPER(Props ++ [{Key1, set_value1(T, Value, new())}])
    end;
%% There are no more keys to iterate through! Override the value here...
set_value1([], Value, _JObj) -> Value.

%% delete_key(foo, {struct, [{foo, bar}, {baz, biz}]}) -> {struct, [{baz, biz}]}
%% delete_key([foo, far], {struct, [{foo, {struct, [{far, away}]}}, {baz, biz}]}) -> {struct, [{foo, {struct, []}}, {baz, biz}]}

-spec delete_key(key(), object() | objects()) -> object() | objects().
-spec delete_key(key(), object() | objects(), 'prune' | 'no_prune') -> object() | objects().
delete_key(Key, JObj) when not is_list(Key) -> delete_key([Key], JObj, 'no_prune');
delete_key(Keys, JObj) -> delete_key(Keys, JObj, 'no_prune').

%% 'prune' removes the parent key if the result of the delete is an empty list; no 'prune' leaves the parent intact
%% so, delete_key([<<"k1">>, <<"k1.1">>], {struct, [{<<"k1">>, {struct, [{<<"k1.1">>, <<"v1.1">>}]}}]}) would result in
%%   'no_prune' -> {struct, [{<<"k1">>, []}]}
%%   'prune' -> {struct, []}
delete_key(Key, JObj, PruneOpt) when not is_list(Key) ->
    ?MODULE:PruneOpt([Key], JObj);
delete_key(Keys, JObj, PruneOpt) ->
    ?MODULE:PruneOpt(Keys, JObj).

%% Figure out how to set the current key among a list of objects
-spec delete_keys([list() | binary(),...], object()) -> object().
delete_keys(Keys, JObj) when is_list(Keys) ->
    lists:foldr(fun(K, JObj0) -> delete_key(K, JObj0) end, JObj, Keys).

prune([], JObj) ->
    JObj;
prune([K], JObj) when not is_list(JObj) ->
    case lists:keydelete(K, 1, to_proplist(JObj)) of
        [] -> new();
        L -> from_list(L)
    end;
prune([K|T], JObj) when not is_list(JObj) ->
    case get_value(K, JObj) of
        'undefined' -> JObj;
        V ->
            case prune(T, V) of
                ?EMPTY_JSON_OBJECT -> from_list(lists:keydelete(K, 1, to_proplist(JObj)));
                [] -> from_list(lists:keydelete(K, 1, to_proplist(JObj)));
                V1 -> from_list([{K, V1} | lists:keydelete(K, 1, to_proplist(JObj))])
            end
    end;
prune(_, []) -> [];
prune([K|T], [_|_]=JObjs) ->
    V = lists:nth(wh_util:to_integer(K), JObjs),
    case prune(T, V) of
        ?EMPTY_JSON_OBJECT -> replace_in_list(K, 'undefined', JObjs, []);
        V -> replace_in_list(K, 'undefined', JObjs, []);
        V1 -> replace_in_list(K, V1, JObjs, [])
    end.

no_prune([], JObj) ->
    JObj;
no_prune([K], JObj) when not is_list(JObj) ->
    case lists:keydelete(K, 1, to_proplist(JObj)) of
        [] -> new();
        L -> from_list(L)
    end;
no_prune([K|T], Array) when is_list(Array) ->
    {Less, [V|More]} = lists:split(wh_util:to_integer(K)-1, Array),
    case {is_json_object(V), T, V} of
        {'true', [_|_]=Keys, JObj} ->
            Less ++ [no_prune(Keys, JObj)] ++ More;
        {'false', [_|_]=Keys, Arr} when is_list(Arr) ->
            Less ++ 'no_prune'(Keys, Arr) ++ More;
        {_,_,_} -> Less ++ More
    end;
no_prune([K|T], JObj) ->
    case get_value(K, JObj) of
        'undefined' -> JObj;
        V ->
            from_list([{K, no_prune(T, V)} | lists:keydelete(K, 1, to_proplist(JObj))])
    end;
no_prune(_, []) -> [];
no_prune([K|T], [_|_]=JObjs) when is_integer(K) ->
    V = lists:nth(wh_util:to_integer(K), JObjs),
    V1 = no_prune(T, V),
    case V1 =:= V of
        'true' ->
            replace_in_list(K, 'undefined', JObjs, []);
        'false' ->
            replace_in_list(K, V1, JObjs, [])
    end.

replace_in_list(N, _, _, _) when N < 1 ->
    exit('badarg');
replace_in_list(1, 'undefined', [_OldV | Vs], Acc) ->
    lists:reverse(Acc) ++ Vs;
replace_in_list(1, V1, [_OldV | Vs], Acc) ->
    lists:reverse([V1 | Acc]) ++ Vs;
replace_in_list(N, V1, [V | Vs], Acc) ->
    replace_in_list(N-1, V1, Vs, [V | Acc]).

%%--------------------------------------------------------------------
%% @doc
%% Read a json fixture file from the filesystem into memory
%%
%% @end
%%--------------------------------------------------------------------
-spec load_fixture_from_file(atom(), nonempty_string() | ne_binary()) ->
                                {'ok', wh_json:object()} |
                                {'error', atom()}.
load_fixture_from_file(App, File) ->
    Path = list_to_binary([code:priv_dir(App), "/couchdb/", wh_util:to_list(File)]),
    lager:debug("read fixture from filesystem whapp ~s from CouchDB JSON file: ~s", [App, Path]),
    try
        {'ok', Bin} = file:read_file(Path),
        decode(Bin)
    catch
        _Type:{'badmatch',{'error',Reason}} ->
            lager:debug("badmatch error: ~p", [Reason]),
            {'error', Reason};
        _Type:Reason ->
            lager:debug("exception: ~p", [Reason]),
            {'error', Reason}
    end.



%%--------------------------------------------------------------------
%% @doc
%% Normalize a JSON object for storage as a Document
%% All dashes are replaced by underscores, all upper case character are
%% converted to lower case
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_jobj(object()) -> object().
normalize_jobj(JObj) -> normalize(JObj).

-spec normalize(object()) -> object().
normalize(JObj) -> foldl(fun normalize_foldl/3, new(), JObj).

-spec normalize_foldl(key(), json_term(), object()) -> object().
normalize_foldl(_K, 'undefined', JObj) -> JObj;
normalize_foldl(K, V, JObj) -> set_value(normalize_key(K), normalize_value(V), JObj).

-spec normalize_value(json_term()) -> json_term().
normalize_value([_|_]=As) -> [normalize_value(A) || A <- As];
normalize_value(Obj) ->
    case is_json_object(Obj) of
        'true' -> normalize(Obj);
        'false' -> Obj
    end.

-spec normalize_key(ne_binary()) -> ne_binary().
normalize_key(Key) when is_binary(Key) ->
    << <<(normalize_key_char(B))>> || <<B>> <= Key>>.

-spec normalize_key_char(char()) -> char().
normalize_key_char($-) -> $_;
normalize_key_char(C) when is_integer(C), $A =< C, C =< $Z -> C + 32;
%% Converts latin capital letters to lowercase, skipping 16#D7 (extended ascii 215) "multiplication sign: x"
normalize_key_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 -> C + 32; % from string:to_lower
normalize_key_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE -> C + 32; % so we only loop once
normalize_key_char(C) -> C.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will filter any private fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec public_fields(object() | objects()) -> object() | objects().
public_fields(JObjs) when is_list(JObjs) -> [public_fields(JObj) || JObj <- JObjs];
public_fields(JObj) ->
    PubJObj = filter(fun({K, _}) -> (not is_private_key(K)) end, JObj),

    case get_binary_value(<<"_id">>, JObj) of
        'undefined' -> PubJObj;
        Id -> set_value(<<"id">>, Id, PubJObj)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will filter any public fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec private_fields(object() | objects()) -> object() | objects().
private_fields(JObjs) when is_list(JObjs) -> [private_fields(JObj) || JObj <- JObjs];
private_fields(JObj) -> filter(fun({K, _}) -> is_private_key(K) end, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will return a boolean, 'true' if the provided key is
%% considered private; otherwise 'false'
%% @end
%%--------------------------------------------------------------------
-spec is_private_key(json_string()) -> boolean().
is_private_key(<<"_", _/binary>>) -> 'true';
is_private_key(<<"pvt_", _/binary>>) -> 'true';
is_private_key(_) -> 'false'.

-spec flatten(object() | objects(), integer(), list()) -> objects().
-spec flatten(any(), list(), list(), integer()) -> objects().
flatten([], _, _) -> [];
flatten(JObj, Depth, Ids) when is_list(Ids) ->
    lists:foldl(
      fun(Id, Acc) ->
              Acc ++ flatten(JObj, Depth, Id)
      end, [], Ids);
flatten(JObj, Depth, Id) ->
    lists:foldl(
      fun(Obj, Acc) ->
              case Obj of
                  {[Id|_], Data} ->
                      [{Data}|Acc];
                  _ ->
                      Acc
              end
      end
      ,[]
      ,flatten(JObj, [], [], Depth)
     ).

flatten({[_ | _] = Elems}, Acc, Keys, Depth) ->
    flatten(Elems, Acc, Keys, Depth);
flatten([_ | _] = Elems, Acc, Keys, Depth) ->
    lists:foldl(fun (Value, A) ->
                        flatten(Value, A, Keys, Depth)
                end,
                Acc, Elems);
flatten({Key, Value}, Acc, Keys, Depth) ->
    case length(Keys) + 2 =:=  Depth of
        'false' ->
            flatten(Value, Acc, [Key | Keys], Depth);
        'true' ->
            case flatten(Value, [], [Key | Keys], Depth) of
                [] -> Acc;
                Group ->
                    Pos = lists:reverse([Key | Keys]),
                    [{Pos, Group} | Acc]
            end
    end;
flatten(Value, Acc, [K | Keys], Depth) ->
    case length(Keys) + 1 =:= Depth of
        'false' -> Acc;
        'true' ->
            [{K, Value} | Acc]
    end.


%% EUNIT TESTING
-ifdef(TEST).

%% PropEr Testing
prop_is_object() ->
    ?FORALL(JObj, object(),
            ?WHENFAIL(io:format("Failed prop_is_json_object ~p~n", [JObj]),
                      is_json_object(JObj))
           ).

prop_from_list() ->
    ?FORALL(Prop, json_proplist(),
            ?WHENFAIL(io:format("Failed prop_from_list with ~p~n", [Prop]),
                      is_json_object(from_list(Prop)))
           ).

prop_get_value() ->
    ?FORALL(Prop, json_proplist(),
            ?WHENFAIL(io:format("Failed prop_get_value with ~p~n", [Prop]),
                      begin
                          JObj = from_list(Prop),
                          case length(Prop) > 0 andalso hd(Prop) of
                              {K,V} ->
                                  V =:= get_value([K], JObj);
                              'false' -> new() =:= JObj
                          end
                      end)).

prop_set_value() ->
    ?FORALL({JObj, Key, Value}, {object(), json_strings(), json_term()},
            ?WHENFAIL(io:format("Failed prop_set_value with ~p:~p -> ~p~n", [Key, Value, JObj]),
                      begin
                          JObj1 = set_value(Key, Value, JObj),
                          Value =:= get_value(Key, JObj1)
                      end)).

prop_to_proplist() ->
    ?FORALL(Prop, json_proplist(),
      ?WHENFAIL(io:format("Failed prop_to_proplist ~p~n", [Prop]),
                begin
                    JObj = from_list(Prop),
                    lists:all(fun(K) -> props:get_value(K, Prop) =/= 'undefined' end, ?MODULE:get_keys(JObj))
                end)
           ).

-define(D1, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}
                           ,{<<"d1k2">>, 'd1v2'}
                           ,{<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}
                          ])).
-define(D2, ?JSON_WRAPPER([{<<"d2k1">>, 1}, {<<"d2k2">>, 3.14}, {<<"sub_d1">>, ?D1}])).
-define(D3, ?JSON_WRAPPER([{<<"d3k1">>, <<"d3v1">>}, {<<"d3k2">>, []}, {<<"sub_docs">>, [?D1, ?D2]}])).
-define(D4, [?D1, ?D2, ?D3]).

-define(D6, ?JSON_WRAPPER([{<<"d2k1">>, 1}
                           ,{<<"d2k2">>, 3.14}
                           ,{<<"sub_d1">>, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}])}
                          ]
                         )).
-define(D7, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}])).

is_json_object_proper_test_() ->
    {"Runs wh_json PropEr tests for is_json_object/1",
     {'timeout', 10000, [?_assertEqual([], proper:module(?MODULE))]}}.

is_empty_test() ->
    ?assertEqual('true', is_empty(new())),
    ?assertEqual('false', is_empty(?D1)),
    ?assertEqual('false', is_empty(?D6)),
    ?assertEqual('false', is_empty(123)),
    ?assertEqual('false', is_empty(<<"foobar">>)),
    ?assertEqual('false', is_empty([{'bar', 'bas'}])).

merge_jobjs_test() ->
    JObj = merge_jobjs(?D1, ?D2),
    ?assertEqual('true', 'undefined' =/= get_value(<<"d1k1">>, JObj)),
    ?assertEqual('true', 'undefined' =/= get_value(<<"d2k1">>, JObj)),
    ?assertEqual('true', 'undefined' =/= get_value(<<"sub_d1">>, JObj)),
    ?assertEqual('true', 'undefined' =:= get_value(<<"missing_k">>, JObj)).

merge_recursive_test() ->
    JObj = merge_recursive(?D1, set_value(<<"d1k2">>, 'd2k2', ?D2)),
    ?assertEqual('true', is_json_object(JObj)),
    ?assertEqual('true', 'undefined' =/= get_value(<<"d1k1">>, JObj)),
    ?assertEqual('true', 'undefined' =/= get_value(<<"d2k1">>, JObj)),

    ?assertEqual('true', 'undefined' =/= get_value([<<"d1k3">>, 2], JObj)),

    ?assertEqual('true', 'undefined' =/= get_value(<<"sub_d1">>, JObj)),

    ?assertEqual('true', 'undefined' =/= get_value([<<"sub_d1">>, <<"d1k1">>], JObj)),
    ?assertEqual('true', 'd2k2' =:= get_value(<<"d1k2">>, JObj)), %% second JObj takes precedence
    ?assertEqual('true', 'undefined' =:= get_value(<<"missing_k">>, JObj)).

get_binary_value_test() ->
    ?assertEqual('true', is_binary(get_binary_value(<<"d1k1">>, ?D1))),
    ?assertEqual('undefined', get_binary_value(<<"d2k1">>, ?D1)),
    ?assertEqual('true', is_binary(get_binary_value(<<"d1k1">>, ?D1, <<"something">>))),
    ?assertEqual(<<"something">>, get_binary_value(<<"d2k1">>, ?D1, <<"something">>)).

get_integer_value_test() ->
    ?assertEqual(1, get_integer_value(<<"d2k1">>, ?D2)),
    ?assertEqual('undefined', get_integer_value(<<"d1k1">>, ?D2)),
    ?assertEqual(1, get_integer_value(<<"d2k1">>, ?D2, 0)),
    ?assertEqual(0, get_integer_value(<<"d1k1">>, ?D2, 0)).

get_float_value_test() ->
    ?assertEqual('true', is_float(get_float_value(<<"d2k2">>, ?D2))),
    ?assertEqual('undefined', get_float_value(<<"d1k1">>, ?D2)),
    ?assertEqual(3.14, get_float_value(<<"d2k2">>, ?D2, 0.0)),
    ?assertEqual(0.0, get_float_value(<<"d1k1">>, ?D2, 0.0)).

get_binary_boolean_test() ->
    ?assertEqual('undefined', get_binary_boolean(<<"d1k1">>, ?D2)),
    ?assertEqual(<<"false">>, get_binary_boolean(<<"a_key">>, ?JSON_WRAPPER([{<<"a_key">>, 'false'}]))),
    ?assertEqual(<<"true">>, get_binary_boolean(<<"a_key">>, ?JSON_WRAPPER([{<<"a_key">>, 'true'}]))).

is_false_test() ->
    ?assertEqual('false', is_false(<<"d1k1">>, ?D1)),
    ?assertEqual('true', is_false(<<"a_key">>, ?JSON_WRAPPER([{<<"a_key">>, 'false'}]))).

is_true_test() ->
    ?assertEqual('false', is_true(<<"d1k1">>, ?D1)),
    ?assertEqual('true', is_true(<<"a_key">>, ?JSON_WRAPPER([{<<"a_key">>, 'true'}]))).

-define(D1_FILTERED, ?JSON_WRAPPER([{<<"d1k2">>, d1v2}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}])).
-define(D2_FILTERED, ?JSON_WRAPPER([{<<"sub_d1">>, ?D1}])).
-define(D3_FILTERED, ?JSON_WRAPPER([{<<"d3k1">>, <<"d3v1">>}, {<<"d3k2">>, []}, {<<"sub_docs">>, [?D1, ?D2_FILTERED]}])).
filter_test() ->
    ?assertEqual(?D1_FILTERED, filter(fun({<<"d1k1">>, _}) -> 'false'; (_) -> 'true' end, ?D1)),
    ?assertEqual(?D2_FILTERED, filter(fun({_, V}) when is_number(V) -> 'false'; (_) -> 'true' end, ?D2)),
    ?assertEqual(?D3_FILTERED, filter(fun({_, V}) when is_number(V) -> 'false'; (_) -> 'true' end, ?D3, [<<"sub_docs">>, 2])).

new_test() ->
    ?EMPTY_JSON_OBJECT =:= new().

-spec is_json_object_test() -> any().
is_json_object_test() ->
    ?assertEqual('false', is_json_object(foo)),
    ?assertEqual('false', is_json_object(123)),
    ?assertEqual('false', is_json_object([boo, yah])),
    ?assertEqual('false', is_json_object(<<"bin">>)),

    ?assertEqual('true', is_json_object(?D1)),
    ?assertEqual('true', is_json_object(?D2)),
    ?assertEqual('true', is_json_object(?D3)),
    ?assertEqual('true', lists:all(fun is_json_object/1, ?D4)),
    ?assertEqual('true', is_json_object(?D6)),
    ?assertEqual('true', is_json_object(?D7)).

%% delete results
-define(D1_AFTER_K1, ?JSON_WRAPPER([{<<"d1k2">>, d1v2}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}])).
-define(D1_AFTER_K3_V2, ?JSON_WRAPPER([{<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.3">>]}, {<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, d1v2}])).

-define(D6_AFTER_SUB, ?JSON_WRAPPER([{<<"sub_d1">>, ?EMPTY_JSON_OBJECT}
                                     ,{<<"d2k1">>, 1}
                                     ,{<<"d2k2">>, 3.14}
                                    ]
                                   )).
-define(D6_AFTER_SUB_PRUNE, ?JSON_WRAPPER([{<<"d2k1">>, 1}
                                           ,{<<"d2k2">>, 3.14}
                                          ]
                                         )).

-define(P1, [{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, d1v2}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}]).
-define(P2, [{<<"d2k1">>, 1}, {<<"d2k2">>, 3.14}, {<<"sub_d1">>, ?JSON_WRAPPER(?P1)}]).
-define(P3, [{<<"d3k1">>, <<"d3v1">>}, {<<"d3k2">>, []}, {<<"sub_docs">>, [?JSON_WRAPPER(?P1), ?JSON_WRAPPER(?P2)]}]).
-define(P4, [?P1, ?P2, ?P3]).
-define(P6, [{<<"d2k1">>, 1},{<<"d2k2">>, 3.14},{<<"sub_d1">>, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}])}]).
-define(P7, [{<<"d1k1">>, <<"d1v1">>}]).

-define(P8, [{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, d1v2}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}]).
-define(P9, [{<<"d2k1">>, 1}, {<<"d2k2">>, 3.14}, {<<"sub_d1">>, ?P1}]).
-define(P10, [{<<"d3k1">>, <<"d3v1">>}, {<<"d3k2">>, []}, {<<"sub_docs">>, [?P8, ?P9]}]).
-define(P11, [?P8, ?P9, ?P10]).
-define(P12, [{<<"d2k1">>, 1}, {<<"d2k2">>, 3.14},{<<"sub_d1">>, [{<<"d1k1">>, <<"d1v1">>}]}]).
-define(P13, [{<<"d1k1">>, <<"d1v1">>}]).

%% deleting [k1, 1] should return empty json object
-define(D_ARR, ?JSON_WRAPPER([{<<"k1">>, [1]}])).
-define(P_ARR, ?JSON_WRAPPER([{<<"k1">>, []}])).

-spec get_keys_test() -> any().
get_keys_test() ->
    Keys = [<<"d1k1">>, <<"d1k2">>, <<"d1k3">>],
    ?assertEqual('true', lists:all(fun(K) -> lists:member(K, Keys) end, ?MODULE:get_keys([], ?D1))),
    ?assertEqual('true', lists:all(fun(K) -> lists:member(K, Keys) end, ?MODULE:get_keys([<<"sub_docs">>, 1], ?D3))),
    ?assertEqual('true', lists:all(fun(K) -> lists:member(K, [1,2,3]) end, ?MODULE:get_keys([<<"sub_docs">>], ?D3))).

-spec to_proplist_test() -> any().
to_proplist_test() ->
    ?assertEqual(?P1, to_proplist(?D1)),
    ?assertEqual(?P2, to_proplist(?D2)),
    ?assertEqual(?P3, to_proplist(?D3)),
    ?assertEqual(?P4, lists:map(fun to_proplist/1, ?D4)),
    ?assertEqual(?P6, to_proplist(?D6)),
    ?assertEqual(?P7, to_proplist(?D7)).

-spec recursive_to_proplist_test() -> any().
recursive_to_proplist_test() ->
    ?assertEqual(?P8, recursive_to_proplist(?D1)),
    ?assertEqual(?P9, recursive_to_proplist(?D2)),
    ?assertEqual(?P10, recursive_to_proplist(?D3)),
    ?assertEqual(?P11, lists:map(fun recursive_to_proplist/1, ?D4)),
    ?assertEqual(?P12, recursive_to_proplist(?D6)),
    ?assertEqual(?P13, recursive_to_proplist(?D7)).

-spec delete_key_test() -> any().
delete_key_test() ->
    ?assertEqual(?EMPTY_JSON_OBJECT, delete_key(<<"foo">>, ?EMPTY_JSON_OBJECT)),
    ?assertEqual(?EMPTY_JSON_OBJECT, delete_key(<<"foo">>, ?EMPTY_JSON_OBJECT, 'prune')),
    ?assertEqual(?EMPTY_JSON_OBJECT, delete_key([<<"foo">>], ?EMPTY_JSON_OBJECT)),
    ?assertEqual(?EMPTY_JSON_OBJECT, delete_key([<<"foo">>], ?EMPTY_JSON_OBJECT, 'prune')),
    ?assertEqual(?EMPTY_JSON_OBJECT, delete_key([<<"foo">>, <<"bar">>], ?EMPTY_JSON_OBJECT)),
    ?assertEqual(?EMPTY_JSON_OBJECT, delete_key([<<"foo">>, <<"bar">>], ?EMPTY_JSON_OBJECT, 'prune')),
    ?assertEqual(?EMPTY_JSON_OBJECT, delete_key([<<"d1k1">>], ?D7)),
    ?assertEqual(?EMPTY_JSON_OBJECT, delete_key([<<"d1k1">>], ?D7, 'prune')),
    ?assertEqual(?D1_AFTER_K1, delete_key([<<"d1k1">>], ?D1)),
    ?assertEqual(?D1_AFTER_K1, delete_key([<<"d1k1">>], ?D1, 'prune')),
    ?assertEqual(?D1_AFTER_K3_V2, delete_key([<<"d1k3">>, 2], ?D1)),
    ?assertEqual(?D1_AFTER_K3_V2, delete_key([<<"d1k3">>, 2], ?D1, 'prune')),
    ?assertEqual(?D6_AFTER_SUB, delete_key([<<"sub_d1">>, <<"d1k1">>], ?D6)),
    ?assertEqual(?D6_AFTER_SUB_PRUNE, delete_key([<<"sub_d1">>, <<"d1k1">>], ?D6, 'prune')),
    ?assertEqual(?P_ARR, delete_key([<<"k1">>, 1], ?D_ARR)),
    ?assertEqual(?EMPTY_JSON_OBJECT, delete_key([<<"k1">>, 1], ?D_ARR, 'prune')).

-spec get_value_test() -> any().
get_value_test() ->
    %% Basic first level key
    ?assertEqual('undefined', get_value([<<"d1k1">>], ?EMPTY_JSON_OBJECT)),
    ?assertEqual(<<"d1v1">>, get_value([<<"d1k1">>], ?D1)),
    ?assertEqual('undefined', get_value([<<"d1k1">>], ?D2)),
    ?assertEqual('undefined', get_value([<<"d1k1">>], ?D3)),
    ?assertEqual('undefined', get_value([<<"d1k1">>], ?D4)),
    %% Basic nested key
    ?assertEqual('undefined', get_value([<<"sub_d1">>, <<"d1k2">>], ?EMPTY_JSON_OBJECT)),
    ?assertEqual('undefined', get_value([<<"sub_d1">>, <<"d1k2">>], ?D1)),
    ?assertEqual(d1v2,      get_value([<<"sub_d1">>, <<"d1k2">>], ?D2)),
    ?assertEqual('undefined', get_value([<<"sub_d1">>, <<"d1k2">>], ?D3)),
    ?assertEqual('undefined', get_value([<<"sub_d1">>, <<"d1k2">>], ?D4)),
    %% Get the value in an object in an array in another object that is part of
    %% an array of objects
    ?assertEqual('undefined', get_value([3, <<"sub_docs">>, 2, <<"d2k2">>], ?EMPTY_JSON_OBJECT)),
    ?assertEqual('undefined', get_value([3, <<"sub_docs">>, 2, <<"d2k2">>], ?D1)),
    ?assertEqual('undefined', get_value([3, <<"sub_docs">>, 2, <<"d2k2">>], ?D2)),
    ?assertEqual('undefined', get_value([3, <<"sub_docs">>, 2, <<"d2k2">>], ?D3)),
    ?assertEqual(3.14,      get_value([3, <<"sub_docs">>, 2, <<"d2k2">>], ?D4)),
    %% Get the value in an object in an array in another object that is part of
    %% an array of objects, but change the default return if it is not present.
    %% Also tests the ability to have indexs represented as strings
    ?assertEqual(<<"not">>, get_value([3, <<"sub_docs">>, <<"2">>, <<"d2k2">>], [], <<"not">>)),
    ?assertEqual(<<"not">>, get_value([3, <<"sub_docs">>, <<"2">>, <<"d2k2">>], ?D1, <<"not">>)),
    ?assertEqual(<<"not">>, get_value([3, <<"sub_docs">>, <<"2">>, <<"d2k2">>], ?D2, <<"not">>)),
    ?assertEqual(<<"not">>, get_value([3, <<"sub_docs">>, <<"2">>, <<"d2k2">>], ?D3, <<"not">>)),
    ?assertEqual(3.14,      get_value([3, <<"sub_docs">>, 2, <<"d2k2">>], ?D4, <<"not">>)).

-define(T2R1, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, <<"update">>}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}])).
-define(T2R2, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, d1v2}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}, {<<"d1k4">>, new_value}])).
-define(T2R3, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, ?JSON_WRAPPER([{<<"new_key">>, added_value}])}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}])).
-define(T2R4, ?JSON_WRAPPER([{<<"d1k1">>, <<"d1v1">>}, {<<"d1k2">>, d1v2}, {<<"d1k3">>, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]}, {<<"d1k4">>, ?JSON_WRAPPER([{<<"new_key">>, added_value}])}])).

set_value_object_test() ->
    %% Test setting an existing key
    ?assertEqual(?T2R1, set_value([<<"d1k2">>], <<"update">>, ?D1)),
    %% Test setting a non-existing key
    ?assertEqual(?T2R2, set_value([<<"d1k4">>], new_value, ?D1)),
    %% Test setting an existing key followed by a non-existant key
    ?assertEqual(?T2R3, set_value([<<"d1k2">>, <<"new_key">>], added_value, ?D1)),
    %% Test setting a non-existing key followed by another non-existant key
    ?assertEqual(?T2R4, set_value([<<"d1k4">>, <<"new_key">>], added_value, ?D1)).

-define(D5,   [?JSON_WRAPPER([{<<"k1">>, v1}]), ?JSON_WRAPPER([{<<"k2">>, v2}])]).
-define(T3R1, [?JSON_WRAPPER([{<<"k1">>,test}]),?JSON_WRAPPER([{<<"k2">>,v2}])]).
-define(T3R2, [?JSON_WRAPPER([{<<"k1">>,v1},{<<"pi">>, 3.14}]),?JSON_WRAPPER([{<<"k2">>,v2}])]).
-define(T3R3, [?JSON_WRAPPER([{<<"k1">>,v1},{<<"callerid">>,?JSON_WRAPPER([{<<"name">>,<<"2600hz">>}])}]),?JSON_WRAPPER([{<<"k2">>,v2}])]).
-define(T3R4, [?JSON_WRAPPER([{<<"k1">>,v1}]),?JSON_WRAPPER([{<<"k2">>,<<"updated">>}])]).
-define(T3R5, [?JSON_WRAPPER([{<<"k1">>,v1}]),?JSON_WRAPPER([{<<"k2">>,v2}]),?JSON_WRAPPER([{<<"new_key">>,<<"added">>}])]).

set_value_multiple_object_test() ->
    %% Set an existing key in the first object()
    ?assertEqual(?T3R1, set_value([1, <<"k1">>], test, ?D5)),
    %% Set a non-existing key in the first object()
    ?assertEqual(?T3R2, set_value([1, <<"pi">>], 3.14, ?D5)),
    %% Set a non-existing key followed by another non-existant key in the first object()
    ?assertEqual(?T3R3, set_value([1, <<"callerid">>, <<"name">>], <<"2600hz">>, ?D5)),
    %% Set an existing key in the second object()
    ?assertEqual(?T3R4, set_value([2, <<"k2">>], <<"updated">>, ?D5)),
    %% Set a non-existing key in a non-existing object()
    ?assertEqual(?T3R5, set_value([3, <<"new_key">>], <<"added">>, ?D5)).

%% <<"ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ"
-define(T4R1,  ?JSON_WRAPPER([{<<"Caller-ID">>, 1234},{list_to_binary(lists:seq(16#C0, 16#D6)), <<"Smith">>} ])).
%% <<"àáâãäåæçèéêëìíîïðñòóôõö"
-define(T4R1V, ?JSON_WRAPPER([{<<"caller_id">>, 1234},{list_to_binary(lists:seq(16#E0, 16#F6)), <<"Smith">>} ])).
%% <<"ØÙÚÛÜÝÞ"
-define(T5R1,  ?JSON_WRAPPER([{<<"Caller-ID">>, 1234},{list_to_binary(lists:seq(16#D8, 16#DE)), <<"Smith">>} ])).
%% <<"øùúûüýþ"
-define(T5R1V, ?JSON_WRAPPER([{<<"caller_id">>, 1234},{list_to_binary(lists:seq(16#F8, 16#FE)), <<"Smith">>} ])).

-define(T4R2,  ?JSON_WRAPPER([{<<"Account-ID">>, <<"45AHGJDF8DFDS2130S">>}, {<<"TRUNK">>, 'false'}, {<<"Node1">>, ?T4R1 }, {<<"Node2">>, ?T4R1 }])).
-define(T4R2V, ?JSON_WRAPPER([{<<"account_id">>, <<"45AHGJDF8DFDS2130S">>}, {<<"trunk">>, 'false'}, {<<"node1">>, ?T4R1V}, {<<"node2">>, ?T4R1V}])).
-define(T4R3,  ?JSON_WRAPPER([{<<"Node-1">>, ?JSON_WRAPPER([{<<"Node-2">>, ?T4R2  }])}, {<<"Another-Node">>, ?T4R1 }] )).
-define(T4R3V, ?JSON_WRAPPER([{<<"node_1">>, ?JSON_WRAPPER([{<<"node_2">>, ?T4R2V }])}, {<<"another_node">>, ?T4R1V}] )).

set_value_normalizer_test() ->
    %% Normalize a flat JSON object
    ?assertEqual(normalize_jobj(?T4R1), ?T4R1V),
    %% Normalize a single nested JSON object
    ?assertEqual(normalize_jobj(?T4R2), ?T4R2V),
    %% Normalize multiple nested JSON object
    ?assertEqual(normalize_jobj(?T4R3), ?T4R3V),

    ?assertEqual(normalize_jobj(?T5R1), ?T5R1V).

to_querystring_test() ->
    Tests = [{<<"{}">>, <<>>}
             ,{<<"{\"foo\":\"bar\"}">>, <<"foo=bar">>}
             ,{<<"{\"foo\":\"bar\",\"fizz\":\"buzz\"}">>, <<"foo=bar&fizz=buzz">>}
             ,{<<"{\"foo\":\"bar\",\"fizz\":\"buzz\",\"arr\":[1,3,5]}">>, <<"foo=bar&fizz=buzz&arr[]=1&arr[]=3&arr[]=5">>}
             ,{<<"{\"Msg-ID\":\"123-abc\"}">>, <<"Msg-ID=123-abc">>}
             ,{<<"{\"url\":\"http://user:pass@host:port/\"}">>, <<"url=http%3A%2F%2Fuser%3Apass%40host%3Aport%2F">>}
             ,{<<"{\"topkey\":{\"subkey1\":\"v1\",\"subkey2\":\"v2\",\"subkey3\":[\"v31\",\"v32\"]}}">>
                   ,<<"topkey[subkey1]=v1&topkey[subkey2]=v2&topkey[subkey3][]=v31&topkey[subkey3][]=v32">>}
             ,{<<"{\"topkey\":{\"subkey1\":\"v1\",\"subkey2\":{\"k3\":\"v3\"}}}">>
                   ,<<"topkey[subkey1]=v1&topkey[subkey2][k3]=v3">>}
            ],
    lists:foreach(fun({JSON, QS}) ->
                          QS1 = wh_util:to_binary(to_querystring(decode(JSON))),
                          ?assertEqual(QS, QS1)
                  end, Tests).

get_values_test() ->
    ?assertEqual('true', are_all_there(?D1, [<<"d1v1">>, d1v2, [<<"d1v3.1">>, <<"d1v3.2">>, <<"d1v3.3">>]], [<<"d1k1">>, <<"d1k2">>, <<"d1k3">>])).

-define(CODEC_JOBJ, ?JSON_WRAPPER([{<<"k1">>, <<"v1">>}
                                   ,{<<"k2">>, ?EMPTY_JSON_OBJECT}
                                   ,{<<"k3">>, ?JSON_WRAPPER([{<<"k3.1">>, <<"v3.1">>}])}
                                   ,{<<"k4">>, [1,2,3]}
                                  ])).
codec_test() ->
    ?assertEqual(?CODEC_JOBJ, decode(encode(?CODEC_JOBJ))).

are_all_there(JObj, Vs, Ks) ->
    {Values, Keys} = get_values(JObj),
    lists:all(fun(K) -> lists:member(K, Keys) end, Ks) andalso
        lists:all(fun(V) -> lists:member(V, Values) end, Vs).

-endif.
