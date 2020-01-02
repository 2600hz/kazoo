%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc proplists-like interface to json objects
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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
        ,exec/2, exec_first/2
        ]).

-export([get_ne_value/2, get_ne_value/3]).
-export([get_value/2, get_value/3
        ,get_values/1, get_values/2
        ,values/1, values/2
        ,take_value/2, take_value/3
        ]).
-export([get_keys/1, get_keys/2]).

-export([set_value_options/0
        ,set_value/3, set_value/4
        ,set_values/2, set_values/3
        ,insert_value/3, insert_values/2
        ,new/0
        ]).
-export([delete_key/2, delete_key/3
        ,delete_keys/2, prune_keys/2
        ]).
-export([merge_recursive/1
        ,merge_recursive/2
        ,merge_recursive/3
        ,merge/1, merge/2, merge/3, merge/4
        ,merge_options/0
        ,merge_left/2, merge_left/3
        ,merge_right/2, merge_right/3
        ]).

-export([from_list/1
        ,from_list_recursive/1, from_list_recursive/2
        ,merge_jobjs/2
        ]).

-export([load_fixture_from_file/2, load_fixture_from_file/3]).

-export([fixture/1
        ,fixture/2
        ]).

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

-export([sort/2
        ,order_by/3
        ,check_value_term/1
        ]).

-export([lift_common_properties/1, lift_common_properties/2]).

-include_lib("kazoo_stdlib/include/kz_log.hrl").

%% %% @headerfile "kazoo_stdlib/include/kazoo_json.hrl"
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-export_type([json_proplist/0
             ,object/0, objects/0
             ,flat_object/0, flat_objects/0
             ,flat_proplist/0
             ,path/0, paths/0
             ,key/0, keys/0
             ,get_key/0
             ,json_term/0, api_json_term/0, json_terms/0
             ,encode_options/0
             ]).

-type get_key() :: key() | path().

-spec new() -> object().
new() -> ?JSON_WRAPPER([]).

-spec encode(json_term()) -> kz_term:text().
encode(JObj) -> encode(JObj, []).

-spec encode(json_term(), encode_options()) -> kz_term:text().
encode(JObj, Options) -> jiffy:encode(JObj, Options).

-spec unsafe_decode(iolist() | kz_term:ne_binary()) -> json_term().
unsafe_decode(Thing) when is_list(Thing);
                          is_binary(Thing) ->
    unsafe_decode(Thing, <<"application/json">>).

-spec unsafe_decode(iolist() | kz_term:ne_binary(), kz_term:ne_binary()) -> json_term().
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

-spec decode(iolist() | kz_term:ne_binary()) -> json_term().
decode(Thing) when is_list(Thing)
                   orelse is_binary(Thing) ->
    decode(Thing, <<"application/json">>).

-spec decode(iolist() | kz_term:ne_binary(), kz_term:ne_binary()) -> json_term().
decode(JSON, <<"application/json">>) ->
    try unsafe_decode(JSON)
    catch
        _:{'invalid_json', {'error', {_Loc, _Msg}}, _JSON} ->
            lager:error_unsafe("decode error ~s near char # ~b => ~s", [_Msg, _Loc, binary:part(JSON, _Loc-5, 25)]),
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

-spec is_defined(get_key(), object()) -> boolean().
is_defined(Path, JObj) ->
    'undefined' =/= get_value(Path, JObj).

-spec is_empty(any()) -> boolean().
is_empty(MaybeJObj) ->
    MaybeJObj =:= ?EMPTY_JSON_OBJECT.

-spec is_json_object(any()) -> boolean().
is_json_object(?JSON_WRAPPER(P)) when is_list(P) -> 'true';
is_json_object(_) -> 'false'.

-spec is_json_object(get_key(), any()) -> boolean().
is_json_object(Key, JObj) ->
    is_json_object(get_value(Key, JObj)).

-spec are_json_objects(list()) -> boolean().
are_json_objects(JObjs) when is_list(JObjs) ->
    lists:all(fun is_json_object/1, JObjs).

-spec is_valid_json_object(any()) -> boolean().
is_valid_json_object(?JSON_WRAPPER(_)=JObj) ->
    lists:all(fun(K) ->
                      is_json_term(get_value([K], JObj))
              end
             ,get_keys(JObj)
             );
is_valid_json_object(_) -> 'false'.

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
is_json_term(MaybeJObj) -> is_valid_json_object(MaybeJObj).

%%------------------------------------------------------------------------------
%% @doc Finds out whether 2 JSON objects are recursively identical.
%% @end
%%------------------------------------------------------------------------------
-spec are_equal(kz_term:api_object(), kz_term:api_object()) -> boolean().
are_equal('undefined', 'undefined') -> 'true';
are_equal('undefined', _) -> 'false';
are_equal(_, 'undefined') -> 'false';
are_equal(JObj1, JObj2) ->
    to_map(JObj1) =:= to_map(JObj2).

%%------------------------------------------------------------------------------
%% @doc Converts top-level proplist to JSON object, but only if sub-proplists
%% have been converted first.
%%
%% For example:
%% ```
%% [{a, b}, {c, [{d, e}]}]
%% '''
%%
%% would be converted to JSON by:
%% ```
%% kz_json:from_list([{a,b}, {c, kz_json:from_list([{d, e}])}]).
%% '''
%%
%% The sub-proplist `[{d,e}]' needs converting before being passed to the next level.
%% @end
%%------------------------------------------------------------------------------
-spec from_list([set_value_kv()] | flat_proplist()) -> object() | flat_object().
from_list(PL) when is_list(PL) ->
    ?JSON_WRAPPER([{K, utf8_binary(V)} || {K, V} <- PL, V =/= 'undefined']).

%% @equiv from_list_recursive(L, #{})
-spec from_list_recursive(json_proplist()) -> object().
from_list_recursive(L) ->
    from_list_recursive(L, #{}).

-type from_list_options() :: #{ascii_list_enforced => boolean()
                              ,invalid_as_null => boolean()
                              }.

%%------------------------------------------------------------------------------
%% @doc Convert recursively proplist to JSON object.
%%
%%
%% Options are:
%%   - `ascii_list_enforced': If the value of a proplist item is list and all
%%     of the elements are ASCII characters, convert the value to binary.
%%     This also converts empty list `[]' to binary. If you are sure
%%     that you don't have any string value, but expect and empty list as value,
%%     set this option to `false` to not convert the empty list to binary
%%   - `invalid_as_null': If the value of a proplist item is not any of
%%     {@link list()}, {@link binary()}, {@link atom()}, {@link integer()},
%%     {@link float()}, {@link kz_time:date()}, {@link kz_time:datetime()},
%%     {@link kz_json:object()} or {@link kz_term:proplist()} then
%%     set this option to `null', otherwise throw `{error, kz_term:ne_binary()}'
%% @end
%%------------------------------------------------------------------------------
-spec from_list_recursive(json_proplist(), from_list_options()) -> object().
from_list_recursive([], _) -> new();
from_list_recursive(L, Opts)
  when is_list(L) ->
    Options = maps:merge(#{ascii_list_enforced => 'true'
                          ,invalid_as_null => 'true'
                          }
                        ,Opts
                        ),
    recursive_from_list(L, Options).

-spec recursive_from_list(list(), from_list_options()) -> object().
recursive_from_list([First | _]=List, Options)
  when is_list(List), is_tuple(First) ->
    from_list([{kz_term:to_binary(K), recursive_from_list(V, Options)}
               || {K,V} <- List
              ]);
recursive_from_list(X, _) when is_float(X) -> X;
recursive_from_list(X, _) when is_integer(X) -> X;
recursive_from_list(X, _) when is_boolean(X) -> X;
recursive_from_list(X, _) when is_atom(X) -> X;
recursive_from_list(X, #{ascii_list_enforced := 'false'}=Options) when is_list(X) ->
    [recursive_from_list(Xn, Options) || Xn <- X];
recursive_from_list(X, #{ascii_list_enforced := 'true'}=Options) when is_list(X) ->
    case lists:all(fun kz_term:is_ascii_code/1, X) of
        'true' -> kz_term:to_binary(X);
        'false' -> [recursive_from_list(Xn, Options) || Xn <- X]
    end;
recursive_from_list(X, _) when is_binary(X) -> utf8_binary(X);
recursive_from_list({_Y, _M, _D}=Date, _) -> kz_date:to_iso8601_extended(Date);
recursive_from_list({{_, _, _}, {_, _, _}}=DateTime, _) -> kz_time:iso8601(DateTime);
recursive_from_list(?JSON_WRAPPER(_)=JObj, _) -> JObj;
recursive_from_list(_Else, #{invalid_as_null := 'true'}) -> 'null';
recursive_from_list(_Else, #{invalid_as_null := 'false'}) ->
    throw({'error', kz_term:to_binary(io_lib:format("invalid json term ~p", [_Else]))}).

%% Lifted from Jesper's post on the ML (Nov 2016) on merging maps

-type merge_arg_2() :: {'left' | 'right', json_term()} | {'both', json_term(), json_term()}.
-type merge_fun_result() :: 'undefined' | {'ok', json_term()}.
-type merge_fun() :: fun((key(), merge_arg_2()) -> merge_fun_result()) |
                     fun((key(), merge_arg_2(), merge_options()) -> merge_fun_result()).

-type merge_options() :: #{'keep_null' => boolean()}.
-spec merge_options() -> merge_options().
merge_options() ->
    #{'keep_null' => 'false'}.

-spec merge(objects()) -> object().
merge(JObjs) ->
    merge(fun merge_right/2, JObjs).

-spec merge(object(), object()) -> object();
           (merge_fun(), objects()) -> object();
           (objects(), merge_options()) -> object().
merge(?JSON_WRAPPER(_)=JObj1, ?JSON_WRAPPER(_)=JObj2) ->
    merge(fun merge_right/2, JObj1, JObj2);
merge(MergeFun, [LeftJObj | RightJObjs]) when is_function(MergeFun, 2) ->
    lists:foldl(fun(Right, Left) -> merge(MergeFun, Left, Right) end
               ,LeftJObj
               ,RightJObjs
               );
merge(MergeFun, [LeftJObj | RightJObjs]) when is_function(MergeFun, 3) ->
    lists:foldl(fun(Right, Left) -> merge(MergeFun, Left, Right) end
               ,LeftJObj
               ,RightJObjs
               );
merge([LeftJObj | RightJObjs], Options) when is_map(Options) ->
    lists:foldl(fun(Right, Left) ->
                        merge(Left, Right, Options)
                end
               ,LeftJObj
               ,RightJObjs
               ).

-spec merge(merge_fun(), object(), object()) -> object();
           (object(), object(), merge_options()) -> object();
           (merge_fun(), objects(), merge_options()) -> object().
merge(MergeFun, ?JSON_WRAPPER(_)=JObj1, ?JSON_WRAPPER(_)=JObj2) when is_function(MergeFun, 2) ->
    merge(MergeFun, JObj1, JObj2, merge_options());
merge(MergeFun, ?JSON_WRAPPER(_)=JObj1, ?JSON_WRAPPER(_)=JObj2) when is_function(MergeFun, 3) ->
    merge(MergeFun, JObj1, JObj2, merge_options());
merge(?JSON_WRAPPER(_)=JObj1, ?JSON_WRAPPER(_)=JObj2, Options) when is_map(Options) ->
    merge(fun merge_right/3, JObj1, JObj2, Options);
merge(MergeFun, [LeftJObj | RightJObjs], Options) when is_function(MergeFun, 2) ->
    lists:foldl(fun(Right, Left) -> merge(MergeFun, Left, Right, Options) end
               ,LeftJObj
               ,RightJObjs
               );
merge(MergeFun, [LeftJObj | RightJObjs], Options) when is_function(MergeFun, 3) ->
    lists:foldl(fun(Right, Left) -> merge(MergeFun, Left, Right, Options) end
               ,LeftJObj
               ,RightJObjs
               ).

-spec merge(merge_fun(), object(), object(), merge_options()) -> object().
merge(MergeFun, ?JSON_WRAPPER(PropsA), ?JSON_WRAPPER(PropsB), Options) ->
    ListA = lists:sort(PropsA),
    ListB = lists:sort(PropsB),
    merge(MergeFun, ListA, ListB, [], Options).

merge(_MergeFun, [], [], Acc, _Options) ->
    from_list(Acc);
merge(MergeFun, [{KX, VX}|Xs], [], Acc, Options) when is_function(MergeFun, 2) ->
    merge(MergeFun, Xs, [], f(KX, MergeFun(KX, {'left', VX}), Acc), Options);
merge(MergeFun, [{KX, VX}|Xs], [], Acc, Options) when is_function(MergeFun, 3) ->
    merge(MergeFun, Xs, [], f(KX, MergeFun(KX, {'left', VX}, Options), Acc), Options);
merge(MergeFun, [], [{KY, VY}|Ys], Acc, Options) when is_function(MergeFun, 2) ->
    merge(MergeFun, Ys, [], f(KY, MergeFun(KY, {'right', VY}), Acc), Options);
merge(MergeFun, [], [{KY, VY}|Ys], Acc, Options) when is_function(MergeFun, 3) ->
    merge(MergeFun, Ys, [], f(KY, MergeFun(KY, {'right', VY}, Options), Acc), Options);
merge(MergeFun, [{KX, VX}|Xs]=Left, [{KY, VY}|Ys]=Right, Acc, Options) when is_function(MergeFun, 2) ->
    if
        KX < KY -> merge(MergeFun, Xs, Right, f(KX, MergeFun(KX, {'left', VX}), Acc), Options);
        KX > KY -> merge(MergeFun, Left, Ys, f(KY, MergeFun(KY, {'right', VY}), Acc), Options);
        KX =:= KY -> merge(MergeFun, Xs, Ys, f(KX, MergeFun(KX, {'both', VX, VY}), Acc), Options)
    end;
merge(MergeFun, [{KX, VX}|Xs]=Left, [{KY, VY}|Ys]=Right, Acc, Options) when is_function(MergeFun, 3) ->
    if
        KX < KY -> merge(MergeFun, Xs, Right, f(KX, MergeFun(KX, {'left', VX}, Options), Acc), Options);
        KX > KY -> merge(MergeFun, Left, Ys, f(KY, MergeFun(KY, {'right', VY}, Options), Acc), Options);
        KX =:= KY -> merge(MergeFun, Xs, Ys, f(KX, MergeFun(KX, {'both', VX, VY}, Options), Acc), Options)
    end.

-spec f(key(), merge_fun_result(), list()) -> list().
f(_K, 'undefined', Acc) -> Acc;
f(K, {'ok', R}, Acc) -> [{K, R} | Acc].

-spec merge_left(key(), merge_arg_2()) -> merge_fun_result().
merge_left(Key, Merger) ->
    merge_left(Key, Merger, merge_options()).

-spec merge_left(key(), merge_arg_2(), merge_options()) -> merge_fun_result().
merge_left(_K, {_Dir, 'null'}, #{'keep_null' := 'true'}) -> {'ok', 'null'};
merge_left(_K, {_Dir, 'null'}, _Options) -> 'undefined';
merge_left(_K, {_Dir, 'undefined'}, _Options) -> 'undefined';
merge_left(_K, {'both', 'null', _Right}, #{'keep_null' := 'true'}) -> {'ok', 'null'};
merge_left(_K, {'both', 'null', _Right}, _Options) -> 'undefined';
merge_left(_K, {'both', 'undefined', _Right}, _Options) -> 'undefined';

merge_left(_K, {'left', ?JSON_WRAPPER(_)=Left}, Options) ->
    {'ok', merge(fun merge_left/3, Left, new(), Options)};
merge_left(_K, {'left', V}, _Options) -> {'ok', V};

merge_left(_K, {'right', ?JSON_WRAPPER(_)=Right}, Options) ->
    {'ok', merge(fun merge_left/3, Right, new(), Options)};
merge_left(_K, {'right', V}, _Options) -> {'ok', V};

merge_left(_K, {'both', ?EMPTY_JSON_OBJECT=Left, ?JSON_WRAPPER(_)=_Right}, _Options) ->
    {'ok', Left};
merge_left(_K, {'both', ?JSON_WRAPPER(_)=Left, ?JSON_WRAPPER(_)=Right}, Options) ->
    {'ok', merge(fun merge_left/3, Left, Right, Options)};
merge_left(_K, {'both', Left, _Right}, _Options) -> {'ok', Left}.

-spec merge_right(key(), merge_arg_2()) -> merge_fun_result().
merge_right(Key, Merger) ->
    merge_right(Key, Merger, merge_options()).

-spec merge_right(key(), merge_arg_2(), merge_options()) -> merge_fun_result().
merge_right(_K, {_Dir, 'null'}, #{'keep_null' := 'true'}) -> {'ok', 'null'};
merge_right(_K, {_Dir, 'null'}, _Options) -> 'undefined';
merge_right(_K, {_Dir, 'undefined'}, _Options) -> 'undefined';
merge_right(_K, {'both', _Left, 'null'}, #{'keep_null' := 'true'}) -> {'ok', 'null'};
merge_right(_K, {'both', _Left, 'null'}, _Options) -> 'undefined';
merge_right(_K, {'both', _Left, 'undefined'}, _Options) -> 'undefined';

merge_right(_K, {'left', ?JSON_WRAPPER(_)=Left}, Options) ->
    {'ok', merge(fun merge_right/3, new(), Left, Options)};
merge_right(_K, {'left', V}, _Options) -> {'ok', V};

merge_right(_K, {'right', ?JSON_WRAPPER(_)=Right}, Options) ->
    {'ok', merge(fun merge_right/3, new(), Right, Options)};
merge_right(_K, {'right', V}, _Options) -> {'ok', V};

merge_right(_K, {'both', ?JSON_WRAPPER(_)=_Left, ?EMPTY_JSON_OBJECT=Right}, _Options) ->
    {'ok', Right};
merge_right(_K, {'both', ?JSON_WRAPPER(_)=Left, ?JSON_WRAPPER(_)=Right}, Options) ->
    {'ok', merge(fun merge_right/3, Left, Right, Options)};
merge_right(_K, {'both', _Left, Right}, _Options) -> {'ok', Right}.

%% @doc Only a top-level merge.
%% Merges JObj1 into JObj2
%% @end
-spec merge_jobjs(object(), object()) -> object().
merge_jobjs(?JSON_WRAPPER(Props1), ?JSON_WRAPPER(_)=JObj2) ->
    lists:foldr(fun({K, V}, JObj2Acc) ->
                        set_value(K, V, JObj2Acc)
                end
               ,JObj2
               ,Props1
               ).

-type merge_pred() :: fun((json_term(), json_term()) -> boolean()).

-spec merge_true(any(), any()) -> 'true'.
merge_true(_, _) -> 'true'.

-spec merge_recursive(objects()) -> object().
merge_recursive(JObjs) when is_list(JObjs) ->
    merge_recursive(JObjs, fun merge_true/2).

-spec merge_recursive(objects(), merge_pred()) -> object();
                     (objects(), merge_options()) -> object();
                     (object(), object()) -> object().
merge_recursive([], Pred) when is_function(Pred, 2) -> new();
merge_recursive([?JSON_WRAPPER(_)=J|JObjs], Pred) when is_function(Pred, 2) ->
    lists:foldl(fun(?JSON_WRAPPER(_)=JObj2, ?JSON_WRAPPER(_)=JObjAcc) ->
                        merge_recursive(JObjAcc, JObj2, Pred)
                end
               ,J
               ,JObjs
               );
merge_recursive([?JSON_WRAPPER(_)=J|JObjs], Options) when is_map(Options) ->
    lists:foldl(fun(?JSON_WRAPPER(_)=JObj2, ?JSON_WRAPPER(_)=JObjAcc) ->
                        merge_recursive(JObjAcc, JObj2, Options)
                end
               ,J
               ,JObjs
               );
merge_recursive(?JSON_WRAPPER(_)=JObj1, ?JSON_WRAPPER(_)=JObj2) ->
    merge_recursive(JObj1, JObj2, fun merge_true/2).

-spec merge_recursive(objects(), merge_pred(), merge_options()) -> object();
                     (object(), object(), merge_pred()) -> object();
                     (object(), object(), merge_options()) -> object().
merge_recursive([?JSON_WRAPPER(_)=J|JObjs], Pred, Options) ->
    lists:foldl(fun(?JSON_WRAPPER(_)=JObj2, ?JSON_WRAPPER(_)=JObjAcc) ->
                        merge_recursive(JObjAcc, JObj2, Pred, [], Options)
                end
               ,J
               ,JObjs
               );
merge_recursive(JObj1, JObj2, Pred) when is_function(Pred, 2) ->
    merge_recursive(JObj1, JObj2, Pred, [], merge_options());
merge_recursive(JObj1, JObj2, Options) when is_map(Options) ->
    merge_recursive(JObj1, JObj2, fun merge_true/2, [], Options).

%% inserts values from JObj2 into JObj1
-spec merge_recursive(object(), object() | json_term(), merge_pred(), keys() | [], merge_options()) -> object().
merge_recursive(?JSON_WRAPPER(_)=JObj1, ?JSON_WRAPPER(_)=JObj2, Pred, Keys, Options) when is_function(Pred, 2) ->
    foldl(fun(Key2, Value2, JObj1Acc) ->
                  merge_recursive(JObj1Acc, Value2, Pred, [Key2|Keys], Options)
          end
         ,JObj1
         ,JObj2
         );
merge_recursive(?JSON_WRAPPER(_)=JObj1, Value, Pred, Keys, Options) when is_function(Pred, 2) ->
    Syek = lists:reverse(Keys),
    case Pred(get_value(Syek, JObj1), Value) of
        'false' -> JObj1;
        'true' -> set_value(Syek, Value, JObj1, Options)
    end.

%% @equiv sum(JObj1, JObj2, fun kz_json:default_sumer/2)

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

%%------------------------------------------------------------------------------
%% @doc Sum two (deep) JSON objects.
%% Default sumer function only sums numbers. For other kinds of values,
%% the value from `JObj1' is kept untouched. If it is undefined it's the
%% one from `JObj2'.
%% @end
%%------------------------------------------------------------------------------
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

%% @equiv sum_jobjs(JObjs, fun kz_json:default_sumer/2)

-spec sum_jobjs(objects()) -> object().
sum_jobjs(JObjs) -> sum_jobjs(JObjs, fun default_sumer/2).

%%------------------------------------------------------------------------------
%% @doc Sum (deep) a list of JSON objects.
%% This is like {@link sum/3} but fold over a list of JSON objects.
%% @see sum/3
%% @end
%%------------------------------------------------------------------------------
-spec sum_jobjs(objects(), sumer()) -> object().
sum_jobjs([], Sumer)
  when is_function(Sumer, 2) -> new();
sum_jobjs([?JSON_WRAPPER(_)=JObj], Sumer)
  when is_function(Sumer, 2) -> JObj;
sum_jobjs([FirstJObj|JObjs], Sumer)
  when is_function(Sumer, 2) ->
    F = fun (JObj, Carry) -> sum(Carry, JObj, fun default_sumer/2) end,
    lists:foldl(F, FirstJObj, JObjs).

%%------------------------------------------------------------------------------
%% @doc Reorder JSON objects according to the given sort function.
%% Returns a sorted list of JObjs, according to the ordering function Fun.
%% Fun(A, B) is to return true if A compares less than or equal to B in the ordering, otherwise false.
%% @end
%%------------------------------------------------------------------------------
-type sort_fun() :: fun((object(), object()) -> boolean()).
-spec sort(sort_fun(), objects()) -> objects().
sort(Fun, ListOfJObjs) ->
    lists:sort(Fun, ListOfJObjs).

%%------------------------------------------------------------------------------
%% @doc Reorder JSON objects according to the given list of binaries.
%% Given Path MUST resolve to all distinct values in the given objects.
%% These resolved values MUST all be in the list of binaries too.
%% List of binaries MUST NOT contain duplicates.
%% Both lists MUST be of same size.
%% @end
%%------------------------------------------------------------------------------
-spec order_by(get_key(), kz_term:ne_binaries(), [objects()]) -> objects().
order_by(Path, Ids, ListOfJObjs)
  when is_list(Ids), is_list(ListOfJObjs) ->
    _ = [[put(get_value(Path, JObj), JObj) || JObj <- JObjs]
         || JObjs <- ListOfJObjs
        ],
    [erase(Id) || Id <- Ids].


%%%-----------------------------------------------------------------------------
%% @doc Lifts shared key/value pairs out of the list of objects
%%
%% @end
%%%-----------------------------------------------------------------------------
-spec lift_common_properties(objects()) ->
          {object(), objects()}.
lift_common_properties(JObjs) ->
    lift_common_properties(JObjs, []).

-spec lift_common_properties(objects(), [get_key()]) ->
          {object(), objects()}.
lift_common_properties([], _Unliftable) -> {new(), []};
lift_common_properties([JObj], _Unliftable) -> {new(), [JObj]};
lift_common_properties([JObj | JObjs], Unliftable) ->
    JobjProperties = jobj_properties(JObj, Unliftable),
    CommonProperties = lift_common_properties(JObjs, Unliftable, JobjProperties),

    {expand(CommonProperties)
    ,remove_common_properties([JObj | JObjs], CommonProperties)
    }.

-spec lift_common_properties(objects(), [get_key()], flat_proplist()) -> flat_object().
lift_common_properties(_JObjs, _Unliftable, []) -> new();
lift_common_properties([], _Unliftable, CommonProperties) ->
    from_list(CommonProperties);
lift_common_properties([JObj|JObjs], Unliftable, CommonProperties) ->
    JObjProperties = jobj_properties(JObj, Unliftable),
    lift_common_properties(JObjs, Unliftable, intersection(CommonProperties, JObjProperties)).

-spec intersection(flat_proplist(), flat_proplist()) -> flat_proplist().
intersection(CommonProperties, JObjProperties) ->
    sets:to_list(sets:intersection(sets:from_list(CommonProperties), sets:from_list(JObjProperties))).

-spec jobj_properties(object(), paths()) -> flat_proplist().
jobj_properties(JObj, []) ->
    lists:usort(to_proplist(flatten(JObj)));
jobj_properties(JObj, Unliftable) ->
    Properties = jobj_properties(JObj, []),
    lists:foldl(fun remove_unliftable/2, Properties, Unliftable).

-spec remove_unliftable(get_key(), flat_proplist()) -> flat_proplist().
remove_unliftable([_|_]=Path, Properties) ->
    lists:filter(fun(Property) -> should_remove_unliftable(Property, Path) end
                ,Properties
                );
remove_unliftable(Key, Properties) ->
    remove_unliftable([Key], Properties).

-spec should_remove_unliftable({path(), any()}, path()) -> boolean().
should_remove_unliftable({Path, _}, Unliftable) ->
    not lists:prefix(Unliftable, Path).

-spec remove_common_properties(objects(), object()) -> objects().
remove_common_properties(JObjs, CommonProperties) ->
    foldl(fun remove_common_property/3, JObjs, CommonProperties).

-spec remove_common_property(get_key(), any(), objects()) -> objects().
remove_common_property(Path, _Value, JObjs) ->
    lists:map(fun(JObj) -> delete_key(Path, JObj, 'prune') end, JObjs).

%% Convert a JSON object to a proplist %% only top-level conversion is supported

-spec to_proplist(object() | objects()) ->
          json_proplist() | json_proplists() | flat_proplist().
to_proplist(JObjs) when is_list(JObjs) -> [to_proplist(JObj) || JObj <- JObjs];
to_proplist(?JSON_WRAPPER(Prop)) -> Prop.

%% convert everything starting at a specific key

-spec to_proplist(get_key(), object() | objects()) ->
          json_proplist() | json_proplists() | flat_proplist().
to_proplist(Key, JObj) -> to_proplist(get_json_value(Key, JObj, new())).

-spec recursive_to_proplist(object() | objects() | kz_term:proplist()) -> kz_term:proplist().
recursive_to_proplist(?JSON_WRAPPER(Props)) ->
    [{K, recursive_to_proplist(V)} || {K, V} <- Props];
recursive_to_proplist(Props) when is_list(Props) ->
    [recursive_to_proplist(V) || V <- Props];
recursive_to_proplist(Else) -> Else.

%%------------------------------------------------------------------------------
%% @doc Convert a JSON object to a map.
%% @end
%%------------------------------------------------------------------------------

-spec to_map(object() | objects()) -> map() | list(map()).
to_map(JObjs) when is_list(JObjs) ->
    jiffy:decode(encode(JObjs), [return_maps]);
to_map(JObj) ->
    recursive_to_map(JObj).

%% convert everything starting at a specific key

-spec to_map(get_key(), object() | objects()) -> map().
to_map(Key, JObj) ->
    recursive_to_map(get_json_value(Key, JObj, new())).

-spec recursive_to_map(object() | objects() | kz_term:proplist()) -> map().
recursive_to_map(?JSON_WRAPPER(Props)) ->
    maps:from_list([{K, recursive_to_map(V)} || {K, V} <- Props]);
recursive_to_map(List) when is_list(List) ->
    [recursive_to_map(Item) || Item <- List];
recursive_to_map(Else) -> Else.

%%------------------------------------------------------------------------------
%% @doc Convert a map to a JSON object.
%% @end
%%------------------------------------------------------------------------------
-spec from_map(map()) -> object().
from_map(Map) when is_map(Map) ->
    recursive_from_map(Map).

-spec recursive_from_map(any()) -> any().
recursive_from_map(Map) when is_map(Map) ->
    from_list([{K, recursive_from_map(V)} || {K, V} <- maps:to_list(Map)]);
recursive_from_map([]) -> [];
recursive_from_map(List) when is_list(List) ->
    Res = [recursive_from_map(Item) || Item <- List],

    case lists:all(fun is_raw_tuple/1, Res) of
        'false' -> [maybe_tuple_to_json(R) || R  <- Res];
        'true' -> from_list(Res)
    end;
recursive_from_map({K, V}) ->
    {K, recursive_from_map(V)};
recursive_from_map(Else) -> Else.

is_raw_tuple(?JSON_WRAPPER(_)) -> 'false';
is_raw_tuple(Term) -> is_tuple(Term).

-spec maybe_tuple_to_json(json_term() | tuple()) -> json_term().
maybe_tuple_to_json({K, V})
  when is_binary(K);
       is_atom(K) ->
    from_list([{K, V}]);
maybe_tuple_to_json({K, V}) ->
    from_list([{kz_term:to_binary(K), V}]);
maybe_tuple_to_json(V) -> V.

-spec get_json_value(get_key(), object()) -> kz_term:api_object().
get_json_value(Key, JObj) -> get_json_value(Key, JObj, 'undefined').

-spec get_json_value(get_key(), object(), Default) -> Default | object().
get_json_value(Key, ?JSON_WRAPPER(_)=JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        ?JSON_WRAPPER(_)=V -> V;
        _ -> Default
    end.

-spec get_ne_json_value(get_key(), object()) -> kz_term:api_object().
get_ne_json_value(Key, JObj) ->
    get_ne_json_value(Key, JObj, 'undefined').

-spec get_ne_json_value(get_key(), object(), Default) -> Default | object().
get_ne_json_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        ?EMPTY_JSON_OBJECT -> Default;
        ?JSON_WRAPPER(_)=V -> V;
        _ -> Default
    end.

-type filter_pred() :: fun(({key(), json_term()}) -> boolean()).

-spec filter(filter_pred(), object()) -> object().
filter(Pred, ?JSON_WRAPPER(Prop)) when is_function(Pred, 1) ->
    from_list([E || {_,_}=E <- Prop, Pred(E)]).

-spec filter(filter_pred(), object(), path()) -> object() | objects().
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

-spec get_string_value(get_key(), object() | objects()) -> kz_term:api_list().
get_string_value(Key, JObj) ->
    get_string_value(Key, JObj, 'undefined').

-spec get_string_value(get_key(), object(), Default) -> list() | Default.
get_string_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:safe_cast(Value, Default, fun kz_term:to_list/1)
    end.

-spec get_list_value(get_key(), object() | objects()) -> kz_term:api_list().
get_list_value(Key, JObj) ->
    get_list_value(Key, JObj, 'undefined').

-spec get_list_value(get_key(), object() | objects(), Default) -> Default | list().
get_list_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        List when is_list(List) -> List;
        _Else -> Default
    end.

-spec get_binary_value(get_key(), object() | objects()) -> kz_term:api_binary().
get_binary_value(Key, JObj) ->
    get_binary_value(Key, JObj, 'undefined').

-spec get_binary_value(get_key(), object() | objects(), Default) -> binary() | Default.
get_binary_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:safe_cast(Value, Default, fun kz_term:to_binary/1)
    end.

-spec get_ne_binary_value(get_key(), object() | objects()) -> kz_term:api_ne_binary().
get_ne_binary_value(Key, JObj) ->
    get_ne_binary_value(Key, JObj, 'undefined').

-spec get_ne_binary_value(get_key(), object() | objects(), Default) -> kz_term:ne_binary() | Default.
get_ne_binary_value(Key, JObj, Default) ->
    case get_binary_value(Key, JObj, Default) of
        Default -> Default;
        <<>> -> Default;
        Value -> Value
    end.

-spec get_lower_binary(get_key(), object() | objects()) -> kz_term:api_binary().
get_lower_binary(Key, JObj) ->
    get_lower_binary(Key, JObj, 'undefined').

-spec get_lower_binary(get_key(), object() | objects(), Default) -> binary() | Default.
get_lower_binary(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:safe_cast(Value, Default, fun kz_term:to_lower_binary/1)
    end.

%% must be an existing atom

-spec get_atom_value(get_key(), object() | objects()) -> kz_term:api_atom().
get_atom_value(Key, JObj) ->
    get_atom_value(Key, JObj, 'undefined').

-spec get_atom_value(get_key(), object() | objects(), Default) -> atom() | Default.
get_atom_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:safe_cast(Value, Default, fun kz_term:to_atom/1)
    end.

-spec get_boolean_value(get_key(), object() | objects()) -> kz_term:api_atom().
get_boolean_value(Key, JObj) ->
    get_boolean_value(Key, JObj, 'undefined').

-spec get_boolean_value(get_key(), object() | objects(), Default) -> atom() | Default.
get_boolean_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:safe_cast(Value, Default, fun kz_term:to_boolean/1)
    end.

-spec get_integer_value(get_key(), object() | objects()) -> kz_term:api_integer().
get_integer_value(Key, JObj) ->
    get_integer_value(Key, JObj, 'undefined').

-spec get_integer_value(get_key(), object() | objects(), Default) -> integer() | Default.
get_integer_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:safe_cast(Value, Default, fun kz_term:to_integer/1)
    end.

-spec get_number_value(get_key(), object() | objects()) -> kz_term:api_number().
get_number_value(Key, JObj) ->
    get_number_value(Key, JObj, 'undefined').

-spec get_number_value(get_key(), object() | objects(), Default) -> number() | Default.
get_number_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:safe_cast(Value, Default, fun kz_term:to_number/1)
    end.

-spec get_float_value(get_key(), object() | objects()) -> kz_term:api_float().
get_float_value(Key, JObj) ->
    get_float_value(Key, JObj, 'undefined').

-spec get_float_value(get_key(), object() | objects(), Default) -> float() | Default.
get_float_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:safe_cast(Value, Default, fun kz_term:to_float/1)
    end.

-spec is_false(get_key(), object() | objects()) -> boolean().
is_false(Key, JObj) ->
    kz_term:is_false(get_value(Key, JObj)).

-spec is_false(get_key(), object() | objects(), Default) -> boolean() | Default.
is_false(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        V -> kz_term:is_false(V)
    end.

-spec is_true(get_key(), object() | objects()) -> boolean().
is_true(Key, JObj) ->
    is_true(Key, JObj, 'false').

-spec is_true(get_key(), object() | objects(), Default) -> boolean() | Default.
is_true(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        V -> kz_term:is_true(V)
    end.

-spec get_binary_boolean(get_key(), object() | objects()) -> kz_term:api_ne_binary().
get_binary_boolean(Key, JObj) ->
    get_binary_boolean(Key, JObj, 'undefined').

-spec get_binary_boolean(get_key(), object() | objects(), Default) -> Default | kz_term:ne_binary().
get_binary_boolean(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:to_binary(kz_term:is_true(Value))
    end.

-spec get_keys(object() | flat_object()) -> keys() | [keys(),...].
get_keys(JObj) -> get_keys1(JObj).

-spec get_keys(get_key(), object() | flat_object()) -> keys() | [keys(),...].
get_keys([], JObj) -> get_keys1(JObj);
get_keys(Keys, JObj) -> get_keys1(get_json_value(Keys, JObj, new())).

-spec get_keys1(list() | object() | flat_object()) -> keys() | [keys(),...].
get_keys1(KVs) when is_list(KVs) -> lists:seq(1, length(KVs));
get_keys1(JObj) -> props:get_keys(to_proplist(JObj)).

-spec get_ne_value(get_key(), object() | objects()) -> api_json_term().
get_ne_value(Key, JObj) ->
    get_ne_value(Key, JObj, 'undefined').

-spec get_ne_value(get_key(), object() | objects(), Default) -> json_term() | Default.
get_ne_value(Key, JObj, Default) ->
    Value = get_value(Key, JObj),
    case kz_term:is_empty(Value) of
        'true' -> Default;
        'false' -> Value
    end.

%%------------------------------------------------------------------------------
%% @doc Find first JSON object that has a non-empty value for `Key'.
%% Returns the value at `Key'.
%% @end
%%------------------------------------------------------------------------------

-spec find(get_key(), objects()) -> api_json_term().
find(Key, JObjs) ->
    find(Key, JObjs, 'undefined').

-spec find(get_key(), objects(), Default) -> json_term() | Default.
find(_, [], Default) -> Default;
find(Key, [JObj|JObjs], Default) when is_list(JObjs) ->
    try get_value(Key, JObj) of
        'undefined' -> find(Key, JObjs, Default);
        V -> V
    catch
        'error':'badarg' -> find(Key, JObjs, Default)
    end.

-spec find_first_defined(keys() | paths(), objects()) -> api_json_term().
find_first_defined(Keys, JObjs) ->
    find_first_defined(Keys, JObjs, 'undefined').

-spec find_first_defined(keys() | paths(), objects(), Default) -> json_term() | Default.
find_first_defined([], _JObjs, Default) -> Default;
find_first_defined([Key|Keys], JObjs, Default) ->
    try find(Key, JObjs) of
        'undefined' -> find_first_defined(Keys, JObjs, Default);
        V -> V
    catch
        'error':'badarg' -> find(Key, JObjs, Default)
    end.

%%------------------------------------------------------------------------------
%% @doc Find first JSON object that has a Value for Key.
%% Returns the JSON object or `undefined'.
%% @end
%%------------------------------------------------------------------------------

-spec find_value(get_key(), json_term(), objects()) -> kz_term:api_object().
find_value(Key, Value, JObjs) ->
    find_value(Key, Value, JObjs, 'undefined').

-spec find_value(get_key(), json_term(), objects(), Default) -> object() | Default.
find_value(_Key, _Value, [], Default) -> Default;
find_value(Key, Value, [JObj|JObjs], Default) ->
    try get_value(Key, JObj) of
        Value -> JObj;
        _Value -> find_value(Key, Value, JObjs, Default)
    catch
        'error':'badarg' -> find_value(Key, Value, JObjs, Default)
    end.

-spec get_first_defined(keys() | paths(), object()) -> json_term() | 'undefined'.
get_first_defined(Keys, JObj) ->
    get_first_defined(Keys, JObj, 'undefined').

-spec get_first_defined(keys() | paths(), object(), Default) -> json_term() | Default.
get_first_defined([], _JObj, Default) -> Default;
get_first_defined([H|T], JObj, Default) ->
    try get_value(H, JObj) of
        'undefined' -> get_first_defined(T, JObj, Default);
        V -> V
    catch
        'error':'badarg' -> get_first_defined(T, JObj, Default)
    end.

-type take_return() :: 'false' |
                       {'value', json_term(), object()}.
-spec take_value(get_key(), object()) -> take_return().
take_value(Key, JObj) ->
    take_value(Key, JObj, 'undefined').

-spec take_value(get_key(), object(), any()) -> take_return().
take_value(Key, JObj, Default) ->
    case get_value(Key, JObj, Default) of
        'undefined' -> 'false';
        Value -> {'value', Value, delete_key(Key, JObj)}
    end.

-spec get_value(get_key(), object() | objects()) -> json_term() | 'undefined'.
get_value(Key, JObj) ->
    get_value(Key, JObj, 'undefined').

-spec get_value(get_key(), object() | objects(), Default) -> json_term() | Default.
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

-spec get_value1(get_key(), kz_term:api_object() | objects(), Default) ->
          json_term() | Default.
get_value1([], 'undefined', Default) -> Default;
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
get_value1([K|Ks], ?JSON_WRAPPER(Props), Default) ->
    get_value1(Ks, props:get_value(K, Props), Default);
get_value1(_, 'undefined', Default) ->
    Default;
get_value1(_, ?JSON_WRAPPER(_), Default) ->
    Default;
get_value1(_K, _V, _D) ->
    erlang:error('badarg').

-spec values(object()) -> json_terms().
values(JObj) ->
    [get_value(Key, JObj)
     || Key <- ?MODULE:get_keys(JObj)
    ].

-spec values(get_key(), object()) -> json_terms().
values(Key, JObj) ->
    values(get_value(Key, JObj, new())).

%%------------------------------------------------------------------------------
%% @doc Split the JSON object into values and the corresponding keys.
%% @end
%%------------------------------------------------------------------------------
-spec get_values(object()) -> {json_terms(), keys()} | {[], []}.
get_values(JObj) ->
    lists:foldr(fun(Key, {Vs, Ks}) ->
                        {[get_value(Key, JObj)|Vs], [Key|Ks]}
                end
               ,{[], []}
               ,?MODULE:get_keys(JObj)
               ).

-spec get_values(get_key(), object()) -> {json_terms(), keys()}.
get_values(Key, JObj) ->
    get_values(get_value(Key, JObj, new())).

-type set_value_options() :: #{'keep_null' => boolean()}.
-spec set_value_options() -> set_value_options().
set_value_options() ->
    #{'keep_null' => 'false'}.

%% Figure out how to set the current key among a list of objects

-type set_value_fun() :: {fun((object(), json_term()) -> object()), json_term()} |
                          fun((object()) -> object()).
-type set_value_funs() :: [set_value_fun(),...].
-type set_value_kv() :: {get_key(), api_json_term() | 'null'}.
-type set_value_kvs() :: [set_value_kv()].

-spec set_values(set_value_kvs() | set_value_funs(), object()) -> object().
set_values(KVs, JObj) when is_list(KVs) ->
    set_values(KVs, JObj, set_value_options()).

-spec set_values(set_value_kvs() | set_value_funs(), object(), set_value_options()) -> object().
set_values(KVs, JObj, Options) when is_list(KVs) ->
    lists:foldr(fun(KV, J) -> set_value_fold(KV, J, Options) end
               ,JObj
               ,KVs
               ).

-spec set_value_fold(set_value_fun() | set_value_kv(), object(), set_value_options()) -> object().
set_value_fold({F, V}, JObj, _Options) when is_function(F, 2) ->
    F(JObj, V);
set_value_fold(F, JObj, _Options) when is_function(F, 1) ->
    F(JObj);
set_value_fold({K, V}, JObj, Options) ->
    set_value(K, V, JObj, Options).

-spec set_value(get_key(), api_json_term() | 'null', object() | objects()) -> object() | objects().
set_value(_Keys, 'undefined', JObj) -> JObj;
set_value(Keys, Value, JObj) when is_list(Keys) -> set_value1(Keys, Value, JObj);
set_value(Key, Value, JObj) -> set_value1([Key], Value, JObj).

-spec set_value(get_key(), api_json_term() | 'null', object() | objects(), set_value_options()) -> object() | objects().
set_value(_Keys, 'undefined', JObj, _Options) -> JObj;
set_value(Keys, Value, JObj, Options) when is_list(Keys) ->
    set_value1(Keys, check_value_term(Value), JObj, Options);
set_value(Key, Value, JObj, Options) ->
    set_value1([Key], check_value_term(Value), JObj, Options).

-spec set_value1(keys(), json_term() | 'null', object() | objects()) -> object() | objects().
set_value1(Keys, Value, JObj) ->
    set_value1(Keys, check_value_term(Value), JObj, set_value_options()).

-spec set_value1(keys(), json_term() | 'null', object() | objects(), set_value_options()) -> object() | objects().
set_value1([Key|_]=Keys, Value, [], Options) when not is_integer(Key) ->
    set_value1(Keys, Value, new(), Options);
set_value1([Key|T], Value, JObjs, Options) when is_list(JObjs) ->
    Key1 = kz_term:to_integer(Key),
    case Key1 > length(JObjs) of
        %% The object index does not exist so try to add a new one to the list
        'true' ->
            try
                %% Create a new object with the next key as a property
                JObjs ++ [set_value1(T, Value, set_value1([hd(T)], [], new(), Options), Options)]
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
                                              {set_value1(T, Value, E, Options), {Pos + 1, Pos}};
                                         (E, {Pos, Idx}) ->
                                              {E, {Pos + 1, Idx}}
                                      end
                                     ,{1, Key1}
                                     ,JObjs
                                     )
                   )
    end;

%% Figure out how to set the current key in an existing object
set_value1([_|_]=Keys, 'null', JObj, #{'keep_null' := 'false'}) -> delete_key(Keys, JObj);
set_value1([Key1|T], Value, ?JSON_WRAPPER(Props), Options) ->
    case lists:keyfind(Key1, 1, Props) of
        {Key1, ?JSON_WRAPPER(_)=V1} ->
            %% Replace or add a property in an object in the object at this key
            ?JSON_WRAPPER(lists:keyreplace(Key1, 1, Props, {Key1, set_value1(T, Value, V1, Options)}));
        {Key1, V1} when is_list(V1) ->
            %% Replace or add a member in an array in the object at this key
            ?JSON_WRAPPER(lists:keyreplace(Key1, 1, Props, {Key1, set_value1(T, Value, V1, Options)}));
        {Key1, _} when T == [] ->
            %% This is the final key and the objects property should just be replaced
            ?JSON_WRAPPER(lists:keyreplace(Key1, 1, Props, {Key1, utf8_binary(Value)}));
        {Key1, _} ->
            %% This is not the final key and the objects property should just be
            %% replaced so continue looping the keys creating the necessary json as we go
            ?JSON_WRAPPER(lists:keyreplace(Key1, 1, Props, {Key1, set_value1(T, Value, new(), Options)}));
        'false' when T == [] ->
            %% This is the final key and doesn't already exist, just add it to this
            %% objects existing properties
            ?JSON_WRAPPER(Props ++ [{Key1, utf8_binary(Value)}]);
        'false' ->
            %% This is not the final key and this object does not have this key
            %% so continue looping the keys creating the necessary json as we go
            ?JSON_WRAPPER(Props ++ [{Key1, set_value1(T, Value, new(), Options)}])
    end;

%% There are no more keys to iterate through! Override the value here...
set_value1([], Value, _JObj, _Options) -> Value.

-spec insert_value(get_key(), json_term(), object()) -> object().
insert_value(Key, Value, JObj) ->
    case get_value(Key, JObj) of
        'undefined' -> set_value(Key, Value, JObj);
        _V -> JObj
    end.

-spec insert_values([{get_key(), json_term()}], object()) -> object().
insert_values(KVs, JObj) ->
    lists:foldl(fun insert_value_fold/2, JObj, KVs).

-spec insert_value_fold({get_key(), json_term()}, object()) -> object().
insert_value_fold({Key, Value}, JObj) ->
    insert_value(Key, Value, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec delete_key(get_key(), object() | objects()) -> object() | objects().
delete_key(Keys, JObj) when is_list(Keys) ->
    delete_key(Keys, JObj, 'no_prune');
delete_key(Key, JObj) ->
    delete_key([Key], JObj, 'no_prune').

%%------------------------------------------------------------------------------
%% @doc No `prune' leaves the parent intact (default).
%% With `prune': removes the parent key if the result of the delete is an empty list.
%% So, `` delete_key([<<"k1">>, <<"k1.1">>], {[{<<"k1">>, {[{<<"k1.1">>, <<"v1.1">>}]}}]}) '' would result in:
%%
%% ```
%%    'no_prune' -> {[{<<"k1">>, []}]}
%%    'prune' -> {[]}
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec delete_key(get_key(), object() | objects(), 'prune' | 'no_prune') -> object() | objects().
delete_key(Key, JObj, 'prune') when not is_list(Key) ->
    prune([Key], JObj);
delete_key(Key, JObj, 'no_prune') when not is_list(Key) ->
    no_prune([Key], JObj);
delete_key(Keys, JObj, 'prune') ->
    prune(Keys, JObj);
delete_key(Keys, JObj, 'no_prune') ->
    no_prune(Keys, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_keys(paths() | keys(), object()) -> object().
delete_keys(Keys, JObj) when is_list(Keys) ->
    %% Figure out how to set the current key among a list of objects
    lists:foldr(fun(K, JObj0) -> delete_key(K, JObj0) end, JObj, Keys).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec prune_keys(keys() | paths(), object()) -> object().
prune_keys(Keys, JObj) when is_list(Keys) ->
    lists:foldr(fun(K, JObj0) -> delete_key(K, JObj0, 'prune') end
               ,JObj
               ,Keys
               ).

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
        ?JSON_WRAPPER(_)=V -> prune_tail(K, T, JObj, V);
        V when is_list(V) -> prune_tail(K, T, JObj, V);
        _ -> erlang:error('badarg')
    end;
prune(_, []) -> [];
prune([K|T], [_|_]=JObjs) ->
    V = lists:nth(kz_term:to_integer(K), JObjs),
    case prune(T, V) of
        ?EMPTY_JSON_OBJECT -> replace_in_list(K, 'undefined', JObjs, []);
        V -> replace_in_list(K, 'undefined', JObjs, []);
        V1 -> replace_in_list(K, V1, JObjs, [])
    end.

-spec prune_tail(key(), keys(), object() | objects(), object() | objects()) ->
                        object() | objects().
prune_tail(K, T, JObj, V) ->
    case prune(T, V) of
        ?EMPTY_JSON_OBJECT -> from_list(lists:keydelete(K, 1, to_proplist(JObj)));
        [] -> from_list(lists:keydelete(K, 1, to_proplist(JObj)));
        V1 -> from_list([{K, V1} | lists:keydelete(K, 1, to_proplist(JObj))])
    end.

-spec no_prune(keys(), object() | objects()) -> object() | objects().
no_prune([], ?JSON_WRAPPER(_)=JObj) -> JObj;
no_prune([K], ?JSON_WRAPPER(Props)) ->
    case lists:keydelete(K, 1, Props) of
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
no_prune([K|T], ?JSON_WRAPPER(_)=JObj) ->
    case get_value(K, JObj) of
        'undefined' -> JObj;
        ?JSON_WRAPPER(_)=V ->
            from_list([{K, no_prune(T, V)} | lists:keydelete(K, 1, to_proplist(JObj))]);
        V when is_list(V) ->
            from_list([{K, no_prune(T, V)} | lists:keydelete(K, 1, to_proplist(JObj))]);
        _ -> erlang:error('badarg')
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
    erlang:error('badarg');
replace_in_list(1, 'undefined', [_OldV | Vs], Acc) ->
    lists:reverse(Acc) ++ Vs;
replace_in_list(1, V1, [_OldV | Vs], Acc) ->
    lists:reverse([V1 | Acc]) ++ Vs;
replace_in_list(N, V1, [V | Vs], Acc) ->
    replace_in_list(N-1, V1, Vs, [V | Acc]).

%%------------------------------------------------------------------------------
%% @doc Read a JSON fixture file from the file system into memory.
%% @end
%%------------------------------------------------------------------------------

-spec load_fixture_from_file(atom(), nonempty_string() | kz_term:ne_binary()) ->
                                    object() |
                                    {'error', atom()}.

load_fixture_from_file(App, File) ->
    load_fixture_from_file(App, <<"couchdb">>, File).

-spec load_fixture_from_file(atom(), nonempty_string() | kz_term:ne_binary(), iodata()) ->
                                    object() |
                                    {'error', atom()}.
load_fixture_from_file(App, Dir, File) ->
    Path = list_to_binary([code:priv_dir(App), "/", kz_term:to_list(Dir), "/", kz_term:to_list(File)]),
    lager:debug("read fixture for kapp ~s from JSON file: ~s", [App, Path]),
    try
        {'ok', Bin} = file:read_file(Path),
        decode(Bin)
    catch
        _Type:{'badmatch', {'error', 'enoent'}} ->
            lager:error("failed to find ~s to read", [Path]),
            {'error', 'enoent'};
        _Type:{'badmatch',{'error',Reason}} ->
            lager:debug("badmatch error: ~p", [Reason]),
            {'error', Reason};
        _Type:Reason ->
            lager:debug("exception: ~p", [Reason]),
            {'error', Reason}
    end.

-spec fixture(file:filename_all()) -> {'ok', object()} | {'error', 'not_found'}.
fixture(Path) ->
    case file:read_file(Path) of
        {'ok', Bin} -> {'ok', decode(Bin)};
        {'error', _} -> {'error', 'not_found'}
    end.

-spec fixture(atom(), file:filename_all()) -> {'ok', object()} | {'error', 'not_found'}.
fixture(App, Path) when is_atom(App) ->
    fixture(filename:join(code:lib_dir(App, 'test'), Path)).

%%------------------------------------------------------------------------------
%% @doc Normalize a JSON object for storage as a Document.
%% All dashes are replaced by underscores, all upper case character are
%% converted to lower case.
%%
%% @end
%%------------------------------------------------------------------------------
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

-spec normalize_key(kz_term:ne_binary()) -> kz_term:ne_binary().
normalize_key(Key) when is_binary(Key) ->
    << <<(normalize_key_char(B))>> || <<B>> <= Key>>.

-spec normalize_key_char(char()) -> char().
normalize_key_char($-) -> $_;
normalize_key_char(C) when is_integer(C), $A =< C, C =< $Z -> C + 32;
%% Converts latin capital letters to lowercase, skipping 16#D7 (extended ASCII 215) "multiplication sign: x"
normalize_key_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 -> C + 32; % from string:to_lower
normalize_key_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE -> C + 32; % so we only loop once
normalize_key_char(C) -> C.

-type search_replace_format() :: {get_key(), get_key()} |
                                 {get_key(), get_key(), fun((any()) -> any())}.
-type search_replace_formatters() :: [search_replace_format()].

-spec normalize_jobj(object(), paths() | keys(), search_replace_formatters()) -> object().
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

%% @doc What is in J1 that is not in J2?
-spec diff(object(), object()) -> object().
diff(J1, J2) ->
    ?JSON_WRAPPER(L1) = flatten(J1),
    ?JSON_WRAPPER(L2) = flatten(J2),
    UniqueSet1 = sets:subtract(sets:from_list(L1), sets:from_list(L2)),
    expand(from_list(sets:to_list(UniqueSet1))).

-type exec_fun_1() :: fun((object()) -> object()).
-type exec_fun_2() :: {fun((_, object()) -> object()), _}.
-type exec_fun_3() :: {fun((_, _, object()) -> object()), _, _}.
-type exec_fun() :: exec_fun_1() | exec_fun_2() | exec_fun_3().
-type exec_funs() :: [exec_fun(),...].

-spec exec(exec_funs(), object()) -> object().
exec(Funs, ?JSON_WRAPPER(_)=JObj) ->
    lists:foldl(fun exec_fold/2, JObj, Funs).

-spec exec_fold(exec_fun(), object()) -> object().
exec_fold({F, K, V}, JObj) when is_function(F, 3) -> F(K, V, JObj);
exec_fold({F, V}, JObj) when is_function(F, 2) -> F(V, JObj);
exec_fold(F, JObj) when is_function(F, 1) -> F(JObj).

-type exec_first_fun_1() :: fun((object()) -> object()).
-type exec_first_fun_2() :: {fun((_, object()) -> object()), _}.
-type exec_first_fun_3() :: {fun((_, _, object()) -> object()), _, _}.
-type exec_first_fun() :: exec_first_fun_1() | exec_first_fun_2() | exec_first_fun_3().
-type exec_first_funs() :: [exec_first_fun(),...].

-spec exec_first(exec_first_funs(), object()) -> object().
exec_first(Funs, ?JSON_WRAPPER(_)=JObj) ->
    lists:foldl(fun exec_first_fold/2, JObj, Funs).

-spec exec_first_fold(exec_first_fun(), object()) -> object().
exec_first_fold({F, K, V}, JObj) when is_function(F, 3) -> F(JObj, K, V);
exec_first_fold({F, V}, JObj) when is_function(F, 2) -> F(JObj, V);
exec_first_fold(F, JObj) when is_function(F, 1) -> F(JObj).

-spec utf8_binary(json_term()) -> json_term().
utf8_binary(<<V/binary>>) -> kz_binary:to_utf8(V);
utf8_binary(Else) -> Else.

-spec check_value_term(json_term()) -> json_term().
check_value_term(<<Term/binary>>) -> utf8_binary(Term);
check_value_term(?JSON_WRAPPER(Prop)) -> ?JSON_WRAPPER([{K, check_value_term(V)} || {K, V} <- Prop]);
check_value_term([_|_]=Terms) -> [check_value_term(Term) || Term <- Terms];
check_value_term(Term) -> Term.
