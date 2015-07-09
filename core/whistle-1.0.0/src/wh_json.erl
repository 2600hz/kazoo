%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
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
-export([get_ne_binary_value/2, get_ne_binary_value/3]).
-export([get_lower_binary/2, get_lower_binary/3]).
-export([get_atom_value/2, get_atom_value/3]).
-export([get_string_value/2, get_string_value/3
         ,get_list_value/2, get_list_value/3
        ]).
-export([get_json_value/2, get_json_value/3]).
-export([is_true/2, is_true/3, is_false/2, is_false/3, is_empty/1]).

-export([filter/2, filter/3
         ,map/2
         ,foldl/3
         ,find/2, find/3
         ,find_first_defined/2, find_first_defined/3
         ,find_value/3, find_value/4
         ,foreach/2
         ,all/2, any/2
        ]).

-export([get_ne_value/2, get_ne_value/3]).
-export([get_value/2, get_value/3
         ,get_values/1, get_values/2
        ]).
-export([get_keys/1, get_keys/2]).
-export([set_value/3, set_values/2
         ,insert_value/3
         ,new/0
        ]).
-export([delete_key/2, delete_key/3, delete_keys/2]).
-export([merge_recursive/1
         ,merge_recursive/2
         ,merge_recursive/3
        ]).

-export([from_list/1, merge_jobjs/2]).

-export([load_fixture_from_file/2]).

-export([normalize_jobj/1
         ,normalize_jobj/3
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

-include("wh_json.hrl").

-export_type([json_term/0, json_terms/0
              ,json_proplist/0, json_proplist_k/1, json_proplist_kv/2
              ,object/0, objects/0
              ,key/0, keys/0
             ]).

-spec new() -> object().
new() -> ?JSON_WRAPPER([]).

-spec encode(json_term()) -> text().
encode(JObj) -> ejson:encode(JObj).

-spec decode(iolist() | ne_binary()) -> json_term().
-spec decode(iolist() | ne_binary(), ne_binary()) -> json_term().

decode(Thing) when is_list(Thing) orelse is_binary(Thing) ->
    decode(Thing, <<"application/json">>).

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

-type merge_pred() :: fun((json_term(), json_term()) -> boolean()).

-spec merge_true(any(), any()) -> 'true'.
merge_true(_, _) -> 'true'.

-spec merge_recursive(objects()) -> object().
merge_recursive(JObjs) when is_list(JObjs) ->
    merge_recursive(JObjs, fun merge_true/2).

-spec merge_recursive(objects() | object(), merge_pred() | object()) -> object().
merge_recursive([], Pred) when is_function(Pred, 2) -> new();
merge_recursive([?JSON_WRAPPER(_)=J|JObjs], Pred) when is_function(Pred, 2) ->
    lists:foldl(fun(?JSON_WRAPPER(_)=JObj2, ?JSON_WRAPPER(_)=JObjAcc) ->
                        merge_recursive(JObjAcc, JObj2, Pred)
                end, J, JObjs);
merge_recursive(?JSON_WRAPPER(_)=JObj1, ?JSON_WRAPPER(_)=JObj2) ->
    merge_recursive(JObj1, JObj2, fun merge_true/2).

-spec merge_recursive(object(), object() | json_term(), merge_pred()) -> object().
merge_recursive(JObj1, JObj2, Pred) when is_function(Pred, 2) ->
    merge_recursive(JObj1, JObj2, Pred, []).

%% inserts values from JObj2 into JObj1
-spec merge_recursive(object(), object() | json_term(), merge_pred(), keys()) -> object().
merge_recursive(?JSON_WRAPPER(_)=JObj1, ?JSON_WRAPPER(_)=JObj2, Pred, Keys) when is_function(Pred, 2) ->
    foldl(fun(Key2, Value2, JObj1Acc) ->
                  merge_recursive(JObj1Acc, Value2, Pred, [Key2|Keys])
          end
          ,JObj1
          ,JObj2
         );
merge_recursive(?JSON_WRAPPER(_)=JObj1, Value, Pred, Keys) when is_function(Pred, 2) ->
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

-spec recursive_to_proplist(object() | objects() | wh_proplist()) -> wh_proplist().
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
-spec fold_kvs(keys(), json_terms(), binary() | iolist(), iolist()) -> iolist().
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
encode_kv(Prefix, K, 'true') ->
    encode_kv(Prefix, K, <<"=">>, <<"true">>);
encode_kv(Prefix, K, 'false') ->
    encode_kv(Prefix, K, <<"=">>, <<"false">>);

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

-spec filter(fun(({key(), json_term()}) -> boolean()), object()) ->
                          object().
-spec filter(fun(({key(), json_term()}) -> boolean()), object(), key()) ->
                          object() | objects().
filter(Pred, ?JSON_WRAPPER(Prop)) when is_function(Pred, 1) ->
    from_list([E || {_,_}=E <- Prop, Pred(E)]).

filter(Pred, ?JSON_WRAPPER(_)=JObj, Keys) when is_list(Keys),
                                               is_function(Pred, 1) ->
    set_value(Keys, filter(Pred, get_json_value(Keys, JObj)), JObj);
filter(Pred, JObj, Key) -> filter(Pred, JObj, [Key]).

-spec map(fun((key(), json_term()) -> {key(), json_term()}), object()) -> object().
map(F, ?JSON_WRAPPER(Prop)) -> from_list([ F(K, V) || {K,V} <- Prop]).

-spec foreach(fun(({json_key(), json_term()}) -> any()), object()) -> 'ok'.
foreach(F, ?JSON_WRAPPER(Prop)) when is_function(F, 1) -> lists:foreach(F, Prop).

-spec all(fun(({json_key(), json_term()}) -> boolean()), object()) -> boolean().
all(Pred, ?JSON_WRAPPER(Prop)) when is_function(Pred, 1) ->
    lists:all(Pred, Prop).

-spec any(fun(({json_key(), json_term()}) -> boolean()), object()) -> boolean().
any(Pred, ?JSON_WRAPPER(Prop)) when is_function(Pred, 1) ->
    lists:any(Pred, Prop).

-spec foldl(fun((key(), json_term(), _) -> _), _, object()) -> _.
foldl(F, Acc0, ?JSON_WRAPPER([])) when is_function(F, 3) -> Acc0;
foldl(F, Acc0, ?JSON_WRAPPER(Prop)) when is_function(F, 3) ->
    lists:foldl(fun({Key, Value}, Acc1) -> F(Key, Value, Acc1) end, Acc0, Prop).

-spec get_string_value(key(), object() | objects()) -> 'undefined' | list().
-spec get_string_value(key(), object(), Default) -> list() | Default.
get_string_value(Key, JObj) ->
    get_string_value(Key, JObj, 'undefined').
get_string_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> safe_cast(Value, Default, fun wh_util:to_list/1)
    end.

-spec get_list_value(key(), object() | objects()) -> 'undefined' | list().
-spec get_list_value(key(), object() | objects(), Default) -> Default | list().
get_list_value(Key, JObj) ->
    get_list_value(Key, JObj, 'undefined').
get_list_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        List when is_list(List) -> List;
        _Else -> Default
    end.

-spec get_binary_value(key(), object() | objects()) -> 'undefined' | binary().
-spec get_binary_value(key(), object() | objects(), Default) -> binary() | Default.
get_binary_value(Key, JObj) ->
    get_binary_value(Key, JObj, 'undefined').
get_binary_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> safe_cast(Value, Default, fun wh_util:to_binary/1)
    end.

-spec get_ne_binary_value(key(), object() | objects()) -> api_binary().
-spec get_ne_binary_value(key(), object() | objects(), Default) -> ne_binary() | Default.
get_ne_binary_value(Key, JObj) ->
    get_ne_binary_value(Key, JObj, 'undefined').
get_ne_binary_value(Key, JObj, Default) ->
    case get_binary_value(Key, JObj, Default) of
        Default -> Default;
        <<>> -> Default;
        Value -> Value
    end.

-spec get_lower_binary(key(), object() | objects()) -> 'undefined' | binary().
-spec get_lower_binary(key(), object() | objects(), Default) -> binary() | Default.
get_lower_binary(Key, JObj) ->
    get_lower_binary(Key, JObj, 'undefined').

get_lower_binary(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> safe_cast(Value, Default, fun wh_util:to_lower_binary/1)
    end.

%% must be an existing atom
-spec get_atom_value(key(), object() | objects()) -> 'undefined' | atom().
-spec get_atom_value(key(), object() | objects(), Default) -> atom() | Default.
get_atom_value(Key, JObj) ->
    get_atom_value(Key, JObj, 'undefined').

get_atom_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> safe_cast(Value, Default, fun wh_util:to_atom/1)
    end.

-spec get_integer_value(key(), object() | objects()) -> api_integer().
get_integer_value(Key, JObj) ->
    get_integer_value(Key, JObj, 'undefined').

-spec get_integer_value(key(), object() | objects(), Default) -> integer() | Default.
get_integer_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> safe_cast(Value, Default, fun wh_util:to_integer/1)
    end.

-spec safe_cast(json_term(), json_term(), fun()) -> json_term().
safe_cast(Value, Default, CastFun) ->
    try CastFun(Value) of
        Casted -> Casted
    catch
        _:_ -> Default
    end.

-spec get_number_value(key(), object() | objects()) -> 'undefined' | number().
-spec get_number_value(key(), object() | objects(), Default) -> number() | Default.
get_number_value(Key, JObj) ->
    get_number_value(Key, JObj, 'undefined').

get_number_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> safe_cast(Value, Default, fun wh_util:to_number/1)
    end.

-spec get_float_value(key(), object() | objects()) -> 'undefined' | float().
-spec get_float_value(key(), object() | objects(), Default) -> float() | Default.
get_float_value(Key, JObj) ->
    get_float_value(Key, JObj, 'undefined').

get_float_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> safe_cast(Value, Default, fun wh_util:to_float/1)
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

-spec get_keys(object()) -> keys().
-spec get_keys(key(), object()) -> [pos_integer(),...] | keys().
get_keys(JObj) -> get_keys1(JObj).

get_keys([], JObj) -> get_keys1(JObj);
get_keys(Keys, JObj) -> get_keys1(get_value(Keys, JObj, new())).

-spec get_keys1(list() | object()) -> [pos_integer(),...] | keys().
get_keys1(KVs) when is_list(KVs) -> lists:seq(1,length(KVs));
get_keys1(JObj) -> props:get_keys(to_proplist(JObj)).

-spec get_ne_value(key(), object() | objects()) ->
                          json_term() | 'undefined'.
-spec get_ne_value(key(), object() | objects(), Default) ->
                          json_term() | Default.
get_ne_value(Key, JObj) ->
    get_ne_value(Key, JObj, 'undefined').

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

find(Key, Docs) ->
    find(Key, Docs, 'undefined').

find(_, [], Default) -> Default;
find(Key, [JObj|JObjs], Default) when is_list(JObjs) ->
    case get_value(Key, JObj) of
        'undefined' -> find(Key, JObjs, Default);
        V -> V
    end.

-spec find_first_defined(key(), objects()) -> json_term() | 'undefined'.
-spec find_first_defined(key(), objects(), Default) -> json_term() | Default.
find_first_defined(Keys, Docs) ->
    find_first_defined(Keys, Docs, 'undefined').

find_first_defined([], _Docs, Default) -> Default;
find_first_defined([Key|Keys], Docs, Default) ->
    case find(Key, Docs) of
        'undefined' -> find_first_defined(Keys, Docs, Default);
        V -> V
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find first json object that has a Value for Key.
%% Returns the json object or 'undefined'
%% @end
%%--------------------------------------------------------------------
-spec find_value(key(), json_term(), objects()) -> api_object().
-spec find_value(key(), json_term(), objects(), Default) -> object() | Default.
find_value(Key, Value, JObjs) ->
    find_value(Key, Value, JObjs, 'undefined').

find_value(_Key, _Value, [], Default) -> Default;
find_value(Key, Value, [JObj|JObjs], Default) ->
    case get_value(Key, JObj) of
        Value -> JObj;
        _Value -> find_value(Key, Value, JObjs, Default)
    end.

-spec get_first_defined(keys(), object()) -> 'undefined' | json_term().
-spec get_first_defined(keys(), object(), Default) -> Default | json_term().
get_first_defined(Keys, JObj) ->
    get_first_defined(Keys, JObj, 'undefined').

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
-spec get_values(object()) -> {json_terms(), keys()}.
get_values(JObj) ->
    lists:foldr(fun(Key, {Vs, Ks}) ->
                        {[get_value(Key, JObj)|Vs], [Key|Ks]}
                end, {[], []}, ?MODULE:get_keys(JObj)).

-spec get_values(key(), object()) -> {json_terms(), keys()}.
get_values(Key, JObj) ->
    get_values(get_value(Key, JObj, new())).

%% Figure out how to set the current key among a list of objects
-type set_value_fun() :: {fun((object(), json_term()) -> object()), json_term()}.
-type set_value_funs() :: [set_value_fun(),...].

-spec set_values(json_proplist() | set_value_funs(), object()) -> object().
set_values(KVs, JObj) when is_list(KVs) ->
    lists:foldr(fun set_value_fold/2, JObj, KVs).

-spec set_value_fold(set_value_fun() | {key(), json_term()}, object()) -> object().
set_value_fold({F, V}, JObj) when is_function(F, 2) ->
    F(JObj, V);
set_value_fold({K, V}, JObj) ->
    set_value(K, V, JObj).

-spec insert_value(key(), json_term(), object()) -> object().
insert_value(Key, Value, JObj) ->
    case get_value(Key, JObj) of
        'undefined' -> set_value(Key, Value, JObj);
        _V -> JObj
    end.

-spec set_value(key(), json_term(), object() | objects()) -> object() | objects().
set_value(Keys, Value, JObj) when is_list(Keys) -> set_value1(Keys, Value, JObj);
set_value(Key, Value, JObj) -> set_value1([Key], Value, JObj).

-spec set_value1(keys() | integers(), json_term(), object() | objects()) -> object() | objects().
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

-type search_replace_format() :: {ne_binary(), ne_binary()} |
                                  {ne_binary(), ne_binary(), fun((term()) -> term())}.
-type search_replace_formatters() :: [search_replace_format(),...] | [].
-spec normalize_jobj(object(), ne_binaries(), search_replace_formatters()) -> object().
normalize_jobj(?JSON_WRAPPER(_)=JObj, RemoveKeys, SearchReplaceFormatters) ->
    normalize_jobj(
      lists:foldl(fun search_replace_format/2
                  ,delete_keys(RemoveKeys, JObj)
                  ,SearchReplaceFormatters
                 )).

-spec search_replace_format(search_replace_format(), object()) -> object().
search_replace_format({Old, New}, JObj) ->
    V = get_value(Old, JObj),
    set_value(New, V, delete_key(Old, JObj));
search_replace_format({Old, New, Formatter}, JObj) when is_function(Formatter, 1) ->
    V = get_value(Old, JObj),
    set_value(New, Formatter(V), delete_key(Old, JObj)).

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
    case wh_doc:id(JObj) of
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
-spec is_private_key(key()) -> boolean().
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
