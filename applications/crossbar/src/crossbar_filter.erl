%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Roman Galeev
%%%-------------------------------------------------------------------
-module(crossbar_filter).

-include("crossbar.hrl").

-export([build/1
        ,build_with_mapper/2, build_with_mapper/3
        ,is_defined/1
        ]).

-type filter_fun() :: fun((kz_json:object(), kz_json:objects()) -> kz_json:objects()).

-spec true(any()) -> 'true'.
true(_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Equivalent of build/2, will call is_defined/1 on Context.
%% @end
%%--------------------------------------------------------------------
-spec build(cb_context:context()) -> function().
build(Context) ->
    build(Context, is_defined(Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Build filter function with arity 1 if filter is requested by the
%% client.
%% @end
%%--------------------------------------------------------------------
-spec build(cb_context:context(), boolean()) -> function().
build(_, 'false') -> fun true/1;
build(Context, 'true') ->
    fun(Doc) ->
            filter_doc_by_querystring(Doc, cb_context:query_string(Context))
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Equivalent of build_with_mapper/3, will call is_defined/1 on Context.
%% @end
%%--------------------------------------------------------------------
-spec build_with_mapper(cb_context:context(), function()) -> filter_fun().
build_with_mapper(Context, UserMapper) ->
    build_with_mapper(Context, UserMapper, is_defined(Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Build a function with arity 2 if filter is requested by the client.
%% This function will filter documents and then applies caller map function
%% on the view result.
%% @end
%%--------------------------------------------------------------------
-spec build_with_mapper(cb_context:context(), function(), boolean()) -> filter_fun().
build_with_mapper(_, UserMapper, 'false') ->
    UserMapper;
build_with_mapper(Context, UserMapper, 'true') ->
    FilterFun = build(Context, 'true'),
    build_filter_map_fun(Context, FilterFun, UserMapper).

-spec build_filter_map_fun(cb_context:context(), function(), function()) -> filter_fun().
build_filter_map_fun(_, FilterFun, UserMapper) when is_function(UserMapper, 1) ->
    fun(Object, Acc) ->
            case FilterFun(kz_json:get_value(<<"doc">>, Object)) of
                'true' -> [UserMapper(Object)|Acc];
                'false' -> Acc
            end
    end;
build_filter_map_fun(_, FilterFun, UserMapper) when is_function(UserMapper, 2) ->
    fun(Object, Acc) ->
            case FilterFun(kz_json:get_value(<<"doc">>, Object)) of
                'true' -> UserMapper(Object, Acc);
                'false' -> Acc
            end
    end;
build_filter_map_fun(Context, FilterFun, UserMapper) when is_function(UserMapper, 3) ->
    fun(Object, Acc) ->
            case FilterFun(kz_json:get_value(<<"doc">>, Object)) of
                'true' -> UserMapper(Context, Object, Acc);
                'false' -> Acc
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check if there is any filter request in query string.
%% @end
%%--------------------------------------------------------------------
-spec is_defined(cb_context:context()) -> boolean().
is_defined(Context) ->
    kz_json:any(fun is_filter_key/1, cb_context:query_string(Context)).

-spec is_filter_key({binary(), any()}) -> boolean().
is_filter_key({<<"filter_", _/binary>>, _}) -> 'true';
is_filter_key({<<"has_key", _/binary>>, _}) -> 'true';
is_filter_key({<<"key_missing", _/binary>>, _}) -> 'true';
is_filter_key({<<"has_value", _/binary>>, _}) -> 'true';
is_filter_key({<<"created_from">>, _}) -> 'true';
is_filter_key({<<"created_to">>, _}) -> 'true';
is_filter_key({<<"modified_from">>, _}) -> 'true';
is_filter_key({<<"modified_to">>, _}) -> 'true';
is_filter_key(_) -> 'false'.

-spec filter_doc_by_querystring(kz_json:object(), kz_json:object()) -> boolean().
filter_doc_by_querystring(Doc, QueryString) ->
    kz_json:all(fun({K, V}) -> should_filter_doc(Doc, K, V) end, QueryString).

-spec should_filter_doc(kz_json:object(), ne_binary(), kz_json:json_term()) -> boolean().
should_filter_doc(Doc, K, V) ->
    try filter_prop(Doc, K, V) of
        'undefined' -> 'true';
        Bool -> Bool
    catch
        _E:_R ->
            lager:debug("failed to process filter ~s: ~s:~p", [K, _E, _R]),
            'false'
    end.

-spec filter_prop(kz_json:object(), ne_binary(), any()) -> api_boolean().
filter_prop(Doc, <<"filter_not_", Key/binary>>, Val) ->
    not should_filter(Doc, Key, Val);
filter_prop(Doc, <<"filter_", Key/binary>>, Val) ->
    should_filter(Doc, Key, Val);
filter_prop(Doc, <<"has_key">>, Key) ->
    has_key(Doc, Key);
filter_prop(Doc, <<"key_missing">>, Key) ->
    not has_key(Doc, Key);
filter_prop(Doc, <<"has_value">>, Key) ->
    has_value(Doc, Key);
filter_prop(Doc, <<"created_from">>, Val) ->
    lowerbound(kz_doc:created(Doc), kz_term:to_integer(Val));
filter_prop(Doc, <<"created_to">>, Val) ->
    upperbound(kz_doc:created(Doc), kz_term:to_integer(Val));
filter_prop(Doc, <<"modified_from">>, Val) ->
    lowerbound(kz_doc:modified(Doc), kz_term:to_integer(Val));
filter_prop(Doc, <<"modified_to">>, Val) ->
    upperbound(kz_doc:modified(Doc), kz_term:to_integer(Val));
filter_prop(_, _, _) ->
    'undefined'.

-spec upperbound(integer(), integer()) -> boolean().
upperbound(DocTimestamp, QSTimestamp) ->
    QSTimestamp >= DocTimestamp.

-spec lowerbound(integer(), integer()) -> boolean().
lowerbound(DocTimestamp, QSTimestamp) ->
    QSTimestamp =< DocTimestamp.

-spec should_filter(binary(), ne_binary()) -> boolean().
-spec should_filter(kz_json:object(), ne_binary(), kz_json:json_term()) -> boolean().
should_filter(Val, Val) -> 'true';
should_filter(Val, FilterVal) ->
    try kz_json:unsafe_decode(FilterVal) of
        List when is_list(List) ->
            lists:member(Val, List);
        _Data ->
            lager:debug("data is not a list: ~p", [_Data]),
            'false'
    catch
        _Error -> 'false'
    end.

should_filter(Doc, Key, Val) ->
    Keys = binary_key_to_json_key(Key),
    should_filter(
      kz_json:get_binary_value(Keys, Doc, <<>>)
                 ,kz_term:to_binary(Val)
     ).

-spec has_key(kz_json:object(), ne_binary()) -> boolean().
has_key(Doc, Key) ->
    Keys = binary_key_to_json_key(Key),
    kz_json:get_value(Keys, Doc) =/= 'undefined'.

-spec has_value(kz_json:object(), ne_binary()) -> boolean().
has_value(Doc, Key) ->
    Keys = binary_key_to_json_key(Key),
    kz_json:get_ne_value(Keys, Doc) =/= 'undefined'.

-spec binary_key_to_json_key(ne_binary()) -> ne_binaries().
binary_key_to_json_key(Key) ->
    binary:split(Key, <<".">>, ['global']).
