%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
%%% @doc
%%% proplists-like interface to json objects
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_json).

%% don't import the get_keys/1 that fetches keys from the process dictionary
-compile({'no_auto_import', [get_keys/1]}).

-export([to_proplist/1, to_proplist/2]).
-export([to_map/1, to_map/2, from_map/1]).
-export([recursive_to_proplist/1]).

-export([get_first_defined/2, get_first_defined/3]).
-export([get_binary_boolean/2, get_binary_boolean/3]).
-export([get_boolean_value/2, get_boolean_value/3]).
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
-export([get_json_value/2, get_json_value/3
        ,get_ne_json_value/2, get_ne_json_value/3
        ]).

-export([is_true/2, is_true/3, is_false/2, is_false/3]).
-export([is_empty/1]).
-export([is_json_object/1, is_json_object/2
        ,are_json_objects/1
        ,is_valid_json_object/1
        ,is_json_term/1
        ]).
-export([is_defined/2]).

-export([filter/2, filter/3
        ,filtermap/2
        ,map/2
        ,foldl/3, foldr/3
        ,find/2, find/3
        ,find_first_defined/2, find_first_defined/3
        ,find_value/3, find_value/4
        ,foreach/2
        ,all/2, any/2
        ,exec/2
        ]).

-export([get_ne_value/2, get_ne_value/3]).
-export([get_value/2, get_value/3
        ,get_values/1, get_values/2
        ,values/1, values/2
        ]).
-export([get_keys/1, get_keys/2]).

-export([set_value/3, set_values/2
        ,insert_value/3, insert_values/2
        ,new/0
        ]).
-export([delete_key/2, delete_key/3
        ,delete_keys/2, prune_keys/2
        ]).
-export([merge_recursive/1
        ,merge_recursive/2
        ,merge_recursive/3
        ,merge/1, merge/2, merge/3
        ,merge_left/2, merge_right/2
        ]).

-export([from_list/1, from_list_recursive/1, merge_jobjs/2]).

-export([load_fixture_from_file/2, load_fixture_from_file/3]).
-ifdef(TEST).
-export([fixture/2]).
-endif.

-export([normalize_jobj/1
        ,normalize_jobj/3
        ,normalize/1
        ,normalize_key/1
        ,are_equal/2
        ]).

-export([encode/1, encode/2]).
-export([decode/1, decode/2]).
-export([unsafe_decode/1, unsafe_decode/2]).

-export([flatten/1, flatten/2
        ,expand/1
        ,diff/2
        ]).

-export([sum/2, sum/3]).
-export([sum_jobjs/1, sum_jobjs/2]).
-export_type([sumer/0]).

-export([order_by/3]).

-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-export_type([json_proplist/0
             ,object/0, objects/0
             ,flat_object/0, flat_objects/0
             ,path/0, paths/0
             ,key/0, keys/0
             ,json_term/0, api_json_term/0, json_terms/0
             ,encode_options/0
             ]).

-spec new() -> object().
new() -> ?JSON_WRAPPER([]).

-spec encode(json_term()) -> text().
-spec encode(json_term(), encode_options()) -> text().
encode(JObj) -> encode(JObj, []).

encode(JObj, Options) -> jiffy:encode(JObj, Options).

-spec unsafe_decode(iolist() | ne_binary()) -> json_term().
-spec unsafe_decode(iolist() | ne_binary(), ne_binary()) -> json_term().

unsafe_decode(Thing) when is_list(Thing);
                          is_binary(Thing) ->
    unsafe_decode(Thing, <<"application/json">>).

unsafe_decode(JSON, <<"application/json">>) ->
    try jiffy:decode(JSON)
    catch
        'throw':{'error',{_Loc, 'invalid_string'}}=Error ->
            lager:debug("invalid string(near char ~p) in input, checking for unicode", [_Loc]),
            case try_converting(JSON) of
                JSON -> throw({'invalid_json', Error, JSON});
                Converted -> unsafe_decode(Converted)
            end;
        'throw':Error ->
            throw({'invalid_json', Error, JSON});
        _Error:_Reason ->
            throw({'invalid_json', {'error', {0, 'decoder_exit'}}, JSON})
    end.

-spec decode(iolist() | ne_binary()) -> json_term().
-spec decode(iolist() | ne_binary(), ne_binary()) -> json_term().

decode(Thing) when is_list(Thing)
                   orelse is_binary(Thing) ->
    decode(Thing, <<"application/json">>).

decode(JSON, <<"application/json">>) ->
    try unsafe_decode(JSON)
    catch
        _:{'invalid_json', {'error', {_Loc, _Msg}}, _JSON} ->
            lager:debug("decode error ~s near char # ~b", [_Msg, _Loc]),
            log_big_binary(JSON),
            new()
    end.

try_converting(JSON) ->
    case unicode:bom_to_encoding(JSON) of
        {'latin1', 0} ->
            lager:debug("json is latin1, trying as utf8"),
            case unicode:characters_to_binary(JSON, 'latin1', 'utf8') of
                Converted when is_binary(Converted) ->
                    case unicode:bom_to_encoding(Converted) of
                        {'latin1', 0} -> JSON;
                        _ -> Converted
                    end;
                _O ->
                    lager:debug("failed to char_to_bin: ~p", [_O]),
                    JSON
            end;
        _Enc ->
            lager:debug("unknown encoding: ~p", [_Enc]),
            JSON
    end.

-spec log_big_binary(binary()) -> 'ok'.
log_big_binary(<<Bin:500/binary, Rest/binary>>) ->
    lager:debug("bin: ~w", [Bin]),
    log_big_binary(Rest);
log_big_binary(Bin) ->
    lager:debug("bin: ~w", [Bin]).

-spec is_defined(path(), object()) -> boolean().
is_defined(Path, JObj) ->
    'undefined' =/= get_value(Path, JObj).

-spec is_empty(any()) -> boolean().
is_empty(MaybeJObj) ->
    MaybeJObj =:= ?EMPTY_JSON_OBJECT.

-spec is_json_object(any()) -> boolean().
-spec is_json_object(path(), any()) -> boolean().
is_json_object(?JSON_WRAPPER(P)) when is_list(P) -> 'true';
is_json_object(_) -> 'false'.

is_json_object(Key, JObj) ->
    is_json_object(get_value(Key, JObj)).

-spec are_json_objects(list()) -> boolean().
are_json_objects(JObjs) when is_list(JObjs) ->
    lists:all(fun is_json_object/1, JObjs).

-spec is_valid_json_object(any()) -> boolean().
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
is_json_term('undefined') -> throw({'error', 'no_atom_undefined_in_jobj_please'});
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Finds out wether 2 JSON objects are recursively identical.
%% @end
%%--------------------------------------------------------------------
-spec are_equal(api_object(), api_object()) -> boolean().
are_equal('undefined', 'undefined') -> 'true';
are_equal('undefined', _) -> 'false';
are_equal(_, 'undefined') -> 'false';
are_equal(JObj1, JObj2) ->
    to_map(JObj1) =:= to_map(JObj2).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Converts top-level proplist to json object, but only if sub-proplists have been converted
%% first.
%% For example:
%% [{a, b}, {c, [{d, e}]}]
%% would be converted to json by
%% kz_json:from_list([{a,b}, {c, kz_json:from_list([{d, e}])}]).
%% the sub-proplist [{d,e}] needs converting before being passed to the next level
%% @end
%%--------------------------------------------------------------------
-spec from_list(json_proplist()) -> object().
from_list(L) when is_list(L) ->
    ?JSON_WRAPPER(props:filter_undefined(L)).

-spec from_list_recursive(json_proplist()) -> object().
from_list_recursive([]) -> new();
from_list_recursive(L)
  when is_list(L) ->
    recursive_from_list(L).

-spec recursive_from_list(list()) -> object().
recursive_from_list([First | _]=List)
  when is_list(List), is_tuple(First) ->
    from_list([{kz_term:to_binary(K), recursive_from_list(V)}
               || {K,V} <- List
              ]);
recursive_from_list(X) when is_float(X) -> X;
recursive_from_list(X) when is_integer(X) -> X;
recursive_from_list(X) when is_atom(X) -> X;
recursive_from_list(X) when is_list(X) ->
    case io_lib:printable_unicode_list(X) of
        'true' -> kz_term:to_binary(X);
        'false' -> [recursive_from_list(Xn) || Xn <- X]
    end;
recursive_from_list(X) when is_binary(X) -> X;
recursive_from_list({_Y, _M, _D}=Date) -> kz_date:to_iso8601_extended(Date);
recursive_from_list({{_, _, _}, {_, _, _}}=DateTime) -> kz_time:iso8601(DateTime);
recursive_from_list(_Else) -> null.

%% Lifted from Jesper's post on the ML (Nov 2016) on merging maps
-spec merge(objects()) -> object().
-spec merge(object(), object()) -> object().
merge([JObjInit | JObjs]) ->
    lists:foldl(fun(JObj, Acc) -> merge(Acc, JObj) end
               ,JObjInit
               ,JObjs
               ).
merge(JObj1, JObj2) ->
    merge(fun merge_right/2, JObj1, JObj2).

-type merge_arg_2() :: {'left' | 'right', json_term()} | {'both', json_term(), json_term()}.
-type merge_fun_result() :: 'undefined' | {'ok', json_term()}.
-type merge_fun() :: fun((key(), merge_arg_2()) -> merge_fun_result()).
-spec merge(merge_fun(), object(), object()) -> object().
merge(MergeFun, ?JSON_WRAPPER(PropsA), ?JSON_WRAPPER(PropsB)) ->
    ListA = lists:sort(PropsA),
    ListB = lists:sort(PropsB),
    merge(MergeFun, ListA, ListB, []).

merge(_MergeFun, [], [], Acc) ->
    from_list(Acc);
merge(MergeFun, [{KX, VX}|Xs], [], Acc) ->
    merge(MergeFun, Xs, [], f(KX, MergeFun(KX, {'left', VX}), Acc));
merge(MergeFun, [], [{KY, VY}|Ys], Acc) ->
    merge(MergeFun, Ys, [], f(KY, MergeFun(KY, {'right', VY}), Acc));
merge(MergeFun, [{KX, VX}|Xs]=Left, [{KY, VY}|Ys]=Right, Acc) ->
    if
        KX < KY -> merge(MergeFun, Xs, Right, f(KX, MergeFun(KX, {'left', VX}), Acc));
        KX > KY -> merge(MergeFun, Left, Ys, f(KY, MergeFun(KY, {'right', VY}), Acc));
        KX =:= KY -> merge(MergeFun, Xs, Ys, f(KX, MergeFun(KX, {'both', VX, VY}), Acc))
    end.

-spec f(key(), merge_fun_result(), list()) -> list().
f(_K, 'undefined', Acc) -> Acc;
f(K, {'ok', R}, Acc) -> [{K, R} | Acc].

-spec merge_left(key(), merge_arg_2()) -> merge_fun_result().
merge_left(_K, {_Dir, 'null'}) -> 'undefined';
merge_left(_K, {_Dir, 'undefined'}) -> 'undefined';
merge_left(_K, {'both', 'null', _Right}) -> 'undefined';
merge_left(_K, {'both', 'undefined', _Right}) -> 'undefined';

merge_left(_K, {'left', ?JSON_WRAPPER(_)=Left}) ->
    {'ok', merge(fun merge_left/2, Left, new())};
merge_left(_K, {'left', V}) -> {'ok', V};

merge_left(_K, {'right', ?JSON_WRAPPER(_)=Right}) ->
    {'ok', merge(fun merge_left/2, Right, new())};
merge_left(_K, {'right', V}) -> {'ok', V};

merge_left(_K, {'both', ?EMPTY_JSON_OBJECT=Left, ?JSON_WRAPPER(_)=_Right}) ->
    {'ok', Left};
merge_left(_K, {'both', ?JSON_WRAPPER(_)=Left, ?JSON_WRAPPER(_)=Right}) ->
    {'ok', merge(fun merge_left/2, Left, Right)};
merge_left(_K, {'both', Left, _Right}) -> {'ok', Left}.

-spec merge_right(key(), merge_arg_2()) -> merge_fun_result().
merge_right(_K, {_Dir, 'null'}) -> 'undefined';
merge_right(_K, {_Dir, 'undefined'}) -> 'undefined';
merge_right(_K, {'both', _Left, 'null'}) -> 'undefined';
merge_right(_K, {'both', _Left, 'undefined'}) -> 'undefined';

merge_right(_K, {'left', ?JSON_WRAPPER(_)=Left}) ->
    {'ok', merge(fun merge_right/2, new(), Left)};
merge_right(_K, {'left', V}) -> {'ok', V};

merge_right(_K, {'right', ?JSON_WRAPPER(_)=Right}) ->
    {'ok', merge(fun merge_right/2, new(), Right)};
merge_right(_K, {'right', V}) -> {'ok', V};

merge_right(_K, {'both', ?JSON_WRAPPER(_)=_Left, ?EMPTY_JSON_OBJECT=Right}) ->
    {'ok', Right};
merge_right(_K, {'both', ?JSON_WRAPPER(_)=Left, ?JSON_WRAPPER(_)=Right}) ->
    {'ok', merge(fun merge_right/2, Left, Right)};
merge_right(_K, {'both', _Left, Right}) -> {'ok', Right}.

%% @public
%% @doc
%% Only a top-level merge.
%% Merges JObj1 into JObj2
%% @end
-spec merge_jobjs(object(), object()) -> object().
merge_jobjs(?JSON_WRAPPER(Props1), ?JSON_WRAPPER(_)=JObj2) ->
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
                end
               ,J
               ,JObjs
               );
merge_recursive(?JSON_WRAPPER(_)=JObj1, ?JSON_WRAPPER(_)=JObj2) ->
    merge_recursive(JObj1, JObj2, fun merge_true/2).

-spec merge_recursive(object(), object() | json_term(), merge_pred()) -> object().
merge_recursive(JObj1, JObj2, Pred) when is_function(Pred, 2) ->
    merge_recursive(JObj1, JObj2, Pred, []).

%% inserts values from JObj2 into JObj1
-spec merge_recursive(object(), object() | json_term(), merge_pred(), keys() | []) -> object().
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sum two (deep) JSON objects.
%% Default sumer function only sums numbers. For other kinds of values,
%% the value from JObj1 is kept untouched. If it is undefined it's the one from JObj2.
%% @end
%%--------------------------------------------------------------------
-spec sum(object(), object()) -> object().
sum(?JSON_WRAPPER(_)=JObj1, ?JSON_WRAPPER(_)=JObj2) ->
    sum(JObj1, JObj2, fun default_sumer/2).

-type sumer() :: fun((json_term(), json_term()) -> json_term()).
-spec default_sumer(json_term(), json_term()) -> json_term().
default_sumer(Value1, undefined) -> Value1;
default_sumer(undefined, Value2) -> Value2;
default_sumer(Value1, Value2) when is_number(Value1),
                                   is_number(Value2) ->
    Value1 + Value2;
default_sumer(Value1, _) ->
    Value1.

-spec sum(object(), object(), sumer()) -> object().
sum(?JSON_WRAPPER(_)=JObj1, ?JSON_WRAPPER(_)=JObj2, Sumer)
  when is_function(Sumer, 2) ->
    sum(JObj1, JObj2, Sumer, []).

-spec sum(object(), object(), sumer(), keys() | []) -> object().
sum(?JSON_WRAPPER(_)=JObj1, ?JSON_WRAPPER(_)=JObj2, Sumer, Keys)
  when is_function(Sumer, 2) ->
    F = fun(Key2, Value2, JObj1Acc) -> sum(JObj1Acc, Value2, Sumer, [Key2|Keys]) end,
    foldl(F, JObj1, JObj2);
sum(?JSON_WRAPPER(_)=JObj1, Value, Sumer, Keys)
  when is_function(Sumer, 2) ->
    Syek = lists:reverse(Keys),
    V = get_value(Syek, JObj1),
    set_value(Syek, Sumer(V, Value), JObj1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sum (deep) JSON objects.
%% Default sumer function only sums numbers. For other kinds of values,
%% the value from JObj1 is kept untouched. If it is undefined it takes the value from the other JObjs.
%% @end
%%--------------------------------------------------------------------
-spec sum_jobjs(objects()) -> object().
sum_jobjs(JObjs) -> sum_jobjs(JObjs, fun default_sumer/2).

-spec sum_jobjs(objects(), sumer()) -> object().
sum_jobjs([], Sumer)
  when is_function(Sumer, 2) -> new();
sum_jobjs([?JSON_WRAPPER(_)=JObj], Sumer)
  when is_function(Sumer, 2) -> JObj;
sum_jobjs([FirstJObj|JObjs], Sumer)
  when is_function(Sumer, 2) ->
    F = fun (JObj, Carry) -> sum(Carry, JObj, fun default_sumer/2) end,
    lists:foldl(F, FirstJObj, JObjs).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Reorder JSON objects according to the given list of binaries.
%% Given Path MUST resolve to all distinct values in the given objects.
%% These resolved values MUST all be in the list of binaries too.
%% List of binaries MUST NOT contain duplicates.
%% Both lists MUST be of same size.
%% @end
%%--------------------------------------------------------------------
-spec order_by(path(), ne_binaries(), [objects()]) -> objects().
order_by(Path, Ids, ListOfJObjs)
  when is_list(Ids), is_list(ListOfJObjs) ->
    _ = [[put(get_value(Path, JObj), JObj) || JObj <- JObjs]
         || JObjs <- ListOfJObjs
        ],
    [erase(Id) || Id <- Ids].

-spec to_proplist(object() | objects()) ->
                         json_proplist() | json_proplists() | flat_proplist().
-spec to_proplist(path(), object() | objects()) ->
                         json_proplist() | json_proplists() | flat_proplist().
%% Convert a json object to a proplist
%% only top-level conversion is supported
to_proplist(JObjs) when is_list(JObjs) -> [to_proplist(JObj) || JObj <- JObjs];
to_proplist(?JSON_WRAPPER(Prop)) -> Prop.

%% convert everything starting at a specific key
to_proplist(Key, JObj) -> to_proplist(get_json_value(Key, JObj, new())).

-spec recursive_to_proplist(object() | objects() | kz_proplist()) -> kz_proplist().
recursive_to_proplist(?JSON_WRAPPER(Props)) ->
    [{K, recursive_to_proplist(V)} || {K, V} <- Props];
recursive_to_proplist(Props) when is_list(Props) ->
    [recursive_to_proplist(V) || V <- Props];
recursive_to_proplist(Else) -> Else.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a json object to a map
%% @end
%%--------------------------------------------------------------------
-spec to_map(object() | objects()) -> map().
-spec to_map(path(), object() | objects()) -> map().
to_map(JObjs) when is_list(JObjs) ->
    lists:foldl(fun to_map_fold/2, #{}, JObjs);
to_map(JObj) ->
    recursive_to_map(JObj).

%% convert everything starting at a specific key
to_map(Key, JObj) ->
    recursive_to_map(get_json_value(Key, JObj, new())).

to_map_fold(JObj, #{}=Map) ->
    maps:merge(Map, recursive_to_map(JObj)).

-spec recursive_to_map(object() | objects() | kz_proplist()) -> map().
recursive_to_map(?JSON_WRAPPER(Props)) ->
    maps:from_list([{K, recursive_to_map(V)} || {K, V} <- Props]);
recursive_to_map(List) when is_list(List) ->
    [recursive_to_map(Item) || Item <- List];
recursive_to_map(Else) -> Else.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a map to a json object
%% @end
%%--------------------------------------------------------------------
-spec from_map(map()) -> object().
from_map(Map) when is_map(Map) ->
    recursive_from_map(Map).

-spec recursive_from_map(map()) -> object().
recursive_from_map(Map) when is_map(Map) ->
    from_list([{K, recursive_from_map(V)} || {K, V} <- maps:to_list(Map)]);
recursive_from_map(List) when is_list(List) ->
    [recursive_from_map(Item) || Item <- List];
recursive_from_map(Else) -> Else.

-spec get_json_value(path(), object()) -> api_object().
-spec get_json_value(path(), object(), Default) -> Default | object().
get_json_value(Key, JObj) -> get_json_value(Key, JObj, 'undefined').
get_json_value(Key, ?JSON_WRAPPER(_)=JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        ?JSON_WRAPPER(_)=V -> V;
        _ -> Default
    end.

-spec get_ne_json_value(path(), object()) -> api_object().
-spec get_ne_json_value(path(), object(), Default) -> Default | object().
get_ne_json_value(Key, JObj) ->
    get_ne_json_value(Key, JObj, 'undefined').
get_ne_json_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        ?EMPTY_JSON_OBJECT -> Default;
        ?JSON_WRAPPER(_)=V -> V;
        _ -> Default
    end.

-type filter_pred() :: fun(({key(), json_term()}) -> boolean()).
-spec filter(filter_pred(), object()) -> object().
-spec filter(filter_pred(), object(), path()) -> object() | objects().

filter(Pred, ?JSON_WRAPPER(Prop)) when is_function(Pred, 1) ->
    from_list([E || {_,_}=E <- Prop, Pred(E)]).

filter(Pred, ?JSON_WRAPPER(_)=JObj, Path) when is_list(Path),
                                               is_function(Pred, 1) ->
    Filtered = filter(Pred, get_json_value(Path, JObj)),
    set_value(Path, Filtered, JObj);
filter(Pred, JObj, Key) ->
    filter(Pred, JObj, [Key]).

-type mapper() :: fun((key(), json_term()) -> {key(), json_term()}).
-spec map(mapper(), object() | flat_object()) -> object() | flat_object().
map(F, ?JSON_WRAPPER(Prop)) when is_function(F, 2) ->
    from_list([F(K, V) || {K,V} <- Prop]).

-type filtermapper() :: fun((key(), json_term()) -> boolean() | {'true', json_term()}).
-spec filtermap(filtermapper(), object() | flat_object()) -> object() | flat_object().
filtermap(F, ?JSON_WRAPPER(Prop)) when is_function(F, 2) ->
    ?JSON_WRAPPER(lists:filtermap(fun({K, V}) -> F(K, V) end, Prop)).

-type foreach_fun() :: fun(({key(), json_term()}) -> any()).
-spec foreach(foreach_fun(), object()) -> 'ok'.
foreach(F, ?JSON_WRAPPER(Prop)) when is_function(F, 1) ->
    lists:foreach(F, Prop).

-type kv_boolean_pred() :: fun(({key(), json_term()}) -> boolean()).
-spec all(kv_boolean_pred(), object()) -> boolean().
all(Pred, ?JSON_WRAPPER(Prop)) when is_function(Pred, 1) ->
    lists:all(Pred, Prop).

-spec any(kv_boolean_pred(), object()) -> boolean().
any(Pred, ?JSON_WRAPPER(Prop)) when is_function(Pred, 1) ->
    lists:any(Pred, Prop).

-type folder() :: fun((key(), json_term(), any()) -> any()).
-spec foldl(folder(), any(), object()) -> any().
foldl(F, Acc0, ?JSON_WRAPPER([])) when is_function(F, 3) -> Acc0;
foldl(F, Acc0, ?JSON_WRAPPER(Prop)) when is_function(F, 3) ->
    lists:foldl(fun({Key, Value}, Acc1) -> F(Key, Value, Acc1) end, Acc0, Prop).

-spec foldr(folder(), any(), object()) -> any().
foldr(F, Acc0, ?JSON_WRAPPER([])) when is_function(F, 3) -> Acc0;
foldr(F, Acc0, ?JSON_WRAPPER(Prop)) when is_function(F, 3) ->
    lists:foldr(fun({Key, Value}, Acc1) -> F(Key, Value, Acc1) end, Acc0, Prop).

-spec get_string_value(path(), object() | objects()) -> api_list().
-spec get_string_value(path(), object(), Default) -> list() | Default.
get_string_value(Key, JObj) ->
    get_string_value(Key, JObj, 'undefined').
get_string_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> safe_cast(Value, Default, fun kz_term:to_list/1)
    end.

-spec get_list_value(path(), object() | objects()) -> api_list().
-spec get_list_value(path(), object() | objects(), Default) -> Default | list().
get_list_value(Key, JObj) ->
    get_list_value(Key, JObj, 'undefined').
get_list_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        List when is_list(List) -> List;
        _Else -> Default
    end.

-spec get_binary_value(path(), object() | objects()) -> api_binary().
-spec get_binary_value(path(), object() | objects(), Default) -> binary() | Default.
get_binary_value(Key, JObj) ->
    get_binary_value(Key, JObj, 'undefined').
get_binary_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> safe_cast(Value, Default, fun kz_term:to_binary/1)
    end.

-spec get_ne_binary_value(path(), object() | objects()) -> api_ne_binary().
-spec get_ne_binary_value(path(), object() | objects(), Default) -> ne_binary() | Default.
get_ne_binary_value(Key, JObj) ->
    get_ne_binary_value(Key, JObj, 'undefined').
get_ne_binary_value(Key, JObj, Default) ->
    case get_binary_value(Key, JObj, Default) of
        Default -> Default;
        <<>> -> Default;
        Value -> Value
    end.

-spec get_lower_binary(path(), object() | objects()) -> api_binary().
-spec get_lower_binary(path(), object() | objects(), Default) -> binary() | Default.
get_lower_binary(Key, JObj) ->
    get_lower_binary(Key, JObj, 'undefined').
get_lower_binary(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> safe_cast(Value, Default, fun kz_term:to_lower_binary/1)
    end.

%% must be an existing atom
-spec get_atom_value(path(), object() | objects()) -> api_atom().
-spec get_atom_value(path(), object() | objects(), Default) -> atom() | Default.
get_atom_value(Key, JObj) ->
    get_atom_value(Key, JObj, 'undefined').
get_atom_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> safe_cast(Value, Default, fun kz_term:to_atom/1)
    end.

-spec get_boolean_value(path(), object() | objects()) -> api_atom().
-spec get_boolean_value(path(), object() | objects(), Default) -> atom() | Default.
get_boolean_value(Key, JObj) ->
    get_boolean_value(Key, JObj, 'undefined').
get_boolean_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> safe_cast(Value, Default, fun kz_term:to_boolean/1)
    end.

-spec get_integer_value(path(), object() | objects()) -> api_integer().
-spec get_integer_value(path(), object() | objects(), Default) -> integer() | Default.
get_integer_value(Key, JObj) ->
    get_integer_value(Key, JObj, 'undefined').
get_integer_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> safe_cast(Value, Default, fun kz_term:to_integer/1)
    end.

-type caster() :: fun((json_term()) -> json_term()).
-spec safe_cast(json_term(), json_term(), caster()) -> json_term().
safe_cast(Value, Default, CastFun) ->
    try CastFun(Value)
    catch
        _:_ -> Default
    end.

-spec get_number_value(path(), object() | objects()) -> api_number().
-spec get_number_value(path(), object() | objects(), Default) -> number() | Default.
get_number_value(Key, JObj) ->
    get_number_value(Key, JObj, 'undefined').
get_number_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> safe_cast(Value, Default, fun kz_term:to_number/1)
    end.

-spec get_float_value(path(), object() | objects()) -> api_float().
-spec get_float_value(path(), object() | objects(), Default) -> float() | Default.
get_float_value(Key, JObj) ->
    get_float_value(Key, JObj, 'undefined').
get_float_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> safe_cast(Value, Default, fun kz_term:to_float/1)
    end.

-spec is_false(path(), object() | objects()) -> boolean().
-spec is_false(path(), object() | objects(), Default) -> boolean() | Default.
is_false(Key, JObj) ->
    kz_term:is_false(get_value(Key, JObj)).
is_false(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        V -> kz_term:is_false(V)
    end.

-spec is_true(path(), object() | objects()) -> boolean().
-spec is_true(path(), object() | objects(), Default) -> boolean() | Default.
is_true(Key, JObj) ->
    is_true(Key, JObj, 'false').
is_true(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        V -> kz_term:is_true(V)
    end.

-spec get_binary_boolean(path(), object() | objects()) -> api_ne_binary().
-spec get_binary_boolean(path(), object() | objects(), Default) -> Default | ne_binary().
get_binary_boolean(Key, JObj) ->
    get_binary_boolean(Key, JObj, 'undefined').

get_binary_boolean(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:to_binary(kz_term:is_true(Value))
    end.

-spec get_keys(object() | flat_object()) -> keys() | [keys(),...] | [].
-spec get_keys(path(), object() | flat_object()) -> keys() | [keys(),...] | [].
get_keys(JObj) -> get_keys1(JObj).

get_keys([], JObj) -> get_keys1(JObj);
get_keys(Keys, JObj) -> get_keys1(get_json_value(Keys, JObj, new())).

-spec get_keys1(list() | object() | flat_object()) -> keys() | [keys(),...] | [].
get_keys1(KVs) when is_list(KVs) -> lists:seq(1, length(KVs));
get_keys1(JObj) -> props:get_keys(to_proplist(JObj)).

-spec get_ne_value(path(), object() | objects()) -> api_json_term().
-spec get_ne_value(path(), object() | objects(), Default) -> json_term() | Default.
get_ne_value(Key, JObj) ->
    get_ne_value(Key, JObj, 'undefined').
get_ne_value(Key, JObj, Default) ->
    Value = get_value(Key, JObj),
    case kz_term:is_empty(Value) of
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
-spec find(path(), objects()) -> api_json_term().
-spec find(path(), objects(), Default) -> json_term() | Default.
find(Key, Docs) ->
    find(Key, Docs, 'undefined').
find(_, [], Default) -> Default;
find(Key, [JObj|JObjs], Default) when is_list(JObjs) ->
    case get_value(Key, JObj) of
        'undefined' -> find(Key, JObjs, Default);
        V -> V
    end.

-spec find_first_defined(paths(), objects()) -> api_json_term().
-spec find_first_defined(paths(), objects(), Default) -> json_term() | Default.
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
-spec find_value(path(), json_term(), objects()) -> api_object().
-spec find_value(path(), json_term(), objects(), Default) -> object() | Default.
find_value(Key, Value, JObjs) ->
    find_value(Key, Value, JObjs, 'undefined').
find_value(_Key, _Value, [], Default) -> Default;
find_value(Key, Value, [JObj|JObjs], Default) ->
    case get_value(Key, JObj) of
        Value -> JObj;
        _Value -> find_value(Key, Value, JObjs, Default)
    end.

-spec get_first_defined(paths(), object()) -> json_term() | 'undefined'.
-spec get_first_defined(paths(), object(), Default) -> json_term() | Default.
get_first_defined(Keys, JObj) ->
    get_first_defined(Keys, JObj, 'undefined').
get_first_defined([], _JObj, Default) -> Default;
get_first_defined([H|T], JObj, Default) ->
    case get_value(H, JObj) of
        'undefined' -> get_first_defined(T, JObj, Default);
        V -> V
    end.

-spec get_value(path(), object() | objects()) -> json_term() | 'undefined'.
-spec get_value(path(), object() | objects(), Default) -> json_term() | Default.
get_value(Key, JObj) ->
    get_value(Key, JObj, 'undefined').
get_value([Key|Ks], L, Default) when is_list(L) ->
    try
        get_value1(Ks, lists:nth(kz_term:to_integer(Key), L), Default)
    catch
        'error':'badarg' -> Default;
        'error':'badarith' -> Default;
        'error':'function_clause' -> Default
    end;
get_value(K, Doc, Default) ->
    get_value1(K, Doc, Default).

-spec get_value1(path(), object() | objects(), Default) ->
                        json_term() | Default.
get_value1([], JObj, _Default) -> JObj;
get_value1(Key, JObj, Default) when not is_list(Key)->
    get_value1([Key], JObj, Default);
get_value1([K|Ks], JObjs, Default) when is_list(JObjs) ->
    try lists:nth(kz_term:to_integer(K), JObjs) of
        'undefined' -> Default;
        JObj1 -> get_value1(Ks, JObj1, Default)
    catch
        _:_ -> Default
    end;
get_value1([K|Ks], ?JSON_WRAPPER(Props)=_JObj, Default) ->
    get_value1(Ks, props:get_value(K, Props, Default), Default);
get_value1(_, _, Default) -> Default.

-spec values(object()) -> json_terms().
-spec values(path(), object()) -> json_terms().
values(JObj) ->
    [get_value(Key, JObj)
     || Key <- ?MODULE:get_keys(JObj)
    ].

values(Key, JObj) ->
    values(get_value(Key, JObj, new())).

%% split the json object into values and the corresponding keys
-spec get_values(object()) -> {json_terms(), keys()} | {[], []}.
get_values(JObj) ->
    lists:foldr(fun(Key, {Vs, Ks}) ->
                        {[get_value(Key, JObj)|Vs], [Key|Ks]}
                end
               ,{[], []}
               ,?MODULE:get_keys(JObj)
               ).

-spec get_values(path(), object()) -> {json_terms(), keys()}.
get_values(Key, JObj) ->
    get_values(get_value(Key, JObj, new())).

%% Figure out how to set the current key among a list of objects

-type set_value_fun() :: {fun((object(), json_term()) -> object()), json_term()} |
                         fun((object()) -> object()).
-type set_value_funs() :: [set_value_fun(),...].

-spec set_values([{path(), json_term()}] | set_value_funs(), object()) -> object().
set_values(KVs, JObj) when is_list(KVs) ->
    lists:foldr(fun set_value_fold/2, JObj, KVs).

-spec set_value_fold(set_value_fun() | {path(), json_term()}, object()) -> object().
set_value_fold({F, V}, JObj) when is_function(F, 2) ->
    F(JObj, V);
set_value_fold(F, JObj) when is_function(F, 1) ->
    F(JObj);
set_value_fold({K, V}, JObj) ->
    set_value(K, V, JObj).

-spec insert_value(path(), json_term(), object()) -> object().
insert_value(Key, Value, JObj) ->
    case get_value(Key, JObj) of
        'undefined' -> set_value(Key, Value, JObj);
        _V -> JObj
    end.

-spec insert_values(json_proplist(), object()) -> object().
insert_values(KVs, JObj) ->
    lists:foldl(fun insert_value_fold/2, JObj, KVs).

-spec insert_value_fold({path(), json_term()}, object()) -> object().
insert_value_fold({Key, Value}, JObj) ->
    insert_value(Key, Value, JObj).

-spec set_value(path(), json_term() | 'null', object() | objects()) -> object() | objects().
set_value(Keys, Value, JObj) when is_list(Keys) -> set_value1(Keys, Value, JObj);
set_value(Key, Value, JObj) -> set_value1([Key], Value, JObj).

-spec set_value1(keys(), json_term() | 'null', object() | objects()) -> object() | objects().
set_value1([Key|_]=Keys, Value, []) when not is_integer(Key) ->
    set_value1(Keys, Value, new());
set_value1([Key|T], Value, JObjs) when is_list(JObjs) ->
    Key1 = kz_term:to_integer(Key),
    case Key1 > length(JObjs) of
        %% The object index does not exist so try to add a new one to the list
        'true' ->
            try
                %% Create a new object with the next key as a property
                JObjs ++ [set_value1(T, Value, set_value1([hd(T)], [], new()))]
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
set_value1([_|_]=Keys, 'null', JObj) -> delete_key(Keys, JObj);
set_value1([_|_]=Keys, 'undefined', JObj) -> delete_key(Keys, JObj);
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete_key(path(), object() | objects()) -> object() | objects().
-spec delete_key(path(), object() | objects(), 'prune' | 'no_prune') -> object() | objects().
delete_key(Keys, JObj) when is_list(Keys) ->
    delete_key(Keys, JObj, 'no_prune');
delete_key(Key, JObj) ->
    delete_key([Key], JObj, 'no_prune').

%%--------------------------------------------------------------------
%% @public
%% @doc
%%  No 'prune' leaves the parent intact (default).
%%  With 'prune': removes the parent key if the result of the delete is an empty list.
%%  So, delete_key([<<"k1">>, <<"k1.1">>], {[{<<"k1">>, {[{<<"k1.1">>, <<"v1.1">>}]}}]}) would result in
%%    'no_prune' -> {[{<<"k1">>, []}]}
%%    'prune' -> {[]}
%% @end
%%--------------------------------------------------------------------
delete_key(Key, JObj, 'prune') when not is_list(Key) ->
    prune([Key], JObj);
delete_key(Key, JObj, 'no_prune') when not is_list(Key) ->
    no_prune([Key], JObj);
delete_key(Keys, JObj, 'prune') ->
    prune(Keys, JObj);
delete_key(Keys, JObj, 'no_prune') ->
    no_prune(Keys, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete_keys(paths(), object()) -> object().
delete_keys(Keys, JObj) when is_list(Keys) ->
    %% Figure out how to set the current key among a list of objects
    lists:foldr(fun(K, JObj0) -> delete_key(K, JObj0) end, JObj, Keys).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec prune_keys(paths(), object()) -> object().
prune_keys(Keys, JObj) when is_list(Keys) ->
    lists:foldr(fun(K, JObj0) -> delete_key(K, JObj0, prune) end, JObj, Keys).

-spec prune(keys(), object() | objects()) -> object() | objects().
prune([], JObj) -> JObj;
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
    V = lists:nth(kz_term:to_integer(K), JObjs),
    case prune(T, V) of
        ?EMPTY_JSON_OBJECT -> replace_in_list(K, 'undefined', JObjs, []);
        V -> replace_in_list(K, 'undefined', JObjs, []);
        V1 -> replace_in_list(K, V1, JObjs, [])
    end.

-spec no_prune(keys(), object() | objects()) -> object() | objects().
no_prune([], JObj) -> JObj;
no_prune([K], JObj) when not is_list(JObj) ->
    case lists:keydelete(K, 1, to_proplist(JObj)) of
        [] -> new();
        L -> from_list(L)
    end;
no_prune([K|T], Array) when is_list(Array), is_integer(K) ->
    {Less, [V|More]} = lists:split(K-1, Array),
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
    V = lists:nth(kz_term:to_integer(K), JObjs),
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
%% @end
%%--------------------------------------------------------------------
-spec load_fixture_from_file(atom(), nonempty_string() | ne_binary()) ->
                                    object() |
                                    {'error', atom()}.

-spec load_fixture_from_file(atom(), nonempty_string() | ne_binary(), iodata()) ->
                                    object() |
                                    {'error', atom()}.

load_fixture_from_file(App, File) ->
    load_fixture_from_file(App, <<"couchdb">>, File).

load_fixture_from_file(App, Dir, File) ->
    Path = list_to_binary([code:priv_dir(App), "/", kz_term:to_list(Dir), "/", kz_term:to_list(File)]),
    lager:debug("read fixture for kapp ~s from JSON file: ~s", [App, Path]),
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

-ifdef(TEST).
-spec fixture(atom(), file:filename_all()) -> {ok,object()} | {error,any()}.
fixture(App, Path0) when is_atom(App) ->
    Path = filename:join(code:lib_dir(App, test), Path0),
    io:format(user, "reading fixture from ~s\n", [Path]),
    case file:read_file(Path) of
        {ok, Bin} -> {ok, decode(Bin)};
        {error, _R}=E ->
            io:format(user, "error fetching ~s: ~p\n", [Path0, _R]),
            E
    end.
-endif.

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
normalize_foldl(_K, 'null', JObj) -> JObj;
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
                                 {ne_binary(), ne_binary(), fun((any()) -> any())}.
-type search_replace_formatters() :: [search_replace_format()].
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

-spec flatten(object() | objects()) -> flat_object() | flat_objects().
flatten(L) when is_list(L) -> [flatten(JObj) || JObj <- L];
flatten(?JSON_WRAPPER(L)) when is_list(L) ->
    ?JSON_WRAPPER(lists:flatten([flatten_key(K,V) || {K,V} <- L])).

-spec flatten(object() | objects(), 'binary_join') -> flat_object() | flat_objects().
flatten(L, 'binary_join') -> keys_to_binary(flatten(L)).

-spec keys_to_binary(object()) -> object().
keys_to_binary(?JSON_WRAPPER(L)) when is_list(L) ->
    from_list([{key_to_binary(K), V} || {K,V} <- L]).

-spec key_to_binary(keys() | key()) -> binary().
key_to_binary(K) when is_list(K) -> kz_binary:join(K, <<"_">>);
key_to_binary(K) -> K.

-spec join_keys(keys() | key(), key()) -> keys().
join_keys(K1, K2) when is_list(K1) -> K1 ++ [K2];
join_keys(K1, K2) -> [K1, K2].

-spec flatten_key(key() | keys(), any()) -> flat_proplist().
flatten_key(K, V = ?EMPTY_JSON_OBJECT) when is_list(K) -> [{K, V}];
flatten_key(K, V = ?EMPTY_JSON_OBJECT) -> [{[K], V}];
flatten_key(K, ?JSON_WRAPPER(L)) when is_list(L) ->
    [flatten_key(join_keys(K, K1), V1) || {K1, V1} <- L];
flatten_key(K, V) when is_list(K) -> [{K, V}];
flatten_key(K, V) -> [{[K], V}].

-spec expand(flat_object()) -> object().
expand(?JSON_WRAPPER(L)) when is_list(L) ->
    lists:foldl(fun({K,V}, JObj) -> set_value(K, V, JObj) end, new(), L).

-spec diff(object(), object()) -> object().
diff(J1, J2) ->
    ?JSON_WRAPPER(L1) = flatten(J1),
    ?JSON_WRAPPER(L2) = flatten(J2),
    expand(from_list(L1 -- L2)).

-type exec_fun_1() :: fun((object()) -> object()).
-type exec_fun_2() :: {fun((_, object()) -> object()), _}.
-type exec_fun_3() :: {fun((_, _, object()) -> object()), _, _}.
-type exec_fun() :: exec_fun_1() | exec_fun_2() | exec_fun_3().
-type exec_funs() :: [exec_fun(),...].

-spec exec(exec_funs(), object()) -> object().
exec(Funs, ?JSON_WRAPPER(_)=JObj) ->
    lists:foldl(fun exec_fold/2, JObj, Funs).

-spec exec_fold(exec_fun(), object()) -> object().
exec_fold({F, K, V}, C) when is_function(F, 3) -> F(K, V, C);
exec_fold({F, V}, C) when is_function(F, 2) -> F(V, C);
exec_fold(F, C) when is_function(F, 1) -> F(C).
