%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Roman Galeev
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(crossbar_filter).

-export([build/1
        ,build_with_mapper/2, build_with_mapper/3
        ,is_defined/1, is_only_time_filter/2

        ,by_doc/2, by_doc/3
        ]).

-include("crossbar.hrl").

-ifdef(TEST).
-export([filter_doc_by_querystring/2
        ]).
-endif.

-type filter_fun() :: fun((kz_json:object()) -> boolean()).

-export_type([filter_fun/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Build filter function if filter is requested by the client.
%% @end
%%--------------------------------------------------------------------
-spec build(cb_context:context()) -> filter_fun().
build(Context) ->
    build(Context, is_defined(Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Equivalent of build_with_mapper/3, will call is_defined/1 on Context.
%% @end
%%--------------------------------------------------------------------
-spec build_with_mapper(cb_context:context(), crossbar_view:user_mapper_fun()) -> crossbar_view:mapper_fun().
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
-spec build_with_mapper(cb_context:context(), crossbar_view:user_mapper_fun(), boolean()) -> crossbar_view:mapper_fun().
build_with_mapper(Context, UserMapper, 'false') when is_function(UserMapper, 3) ->
    fun(Object, Acc) -> UserMapper(Context, Object, Acc) end;
build_with_mapper(_, UserMapper, 'false') ->
    UserMapper;
build_with_mapper(Context, UserMapper, 'true') ->
    FilterFun = build(Context, 'true'),
    build_filter_map_fun(Context, FilterFun, UserMapper).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check if there is any filter request in query string.
%% @end
%%--------------------------------------------------------------------
-spec is_defined(cb_context:context()) -> boolean().
is_defined(Context) ->
    cb_context:fetch(Context, 'has_qs_filter', 'true') %% set by crossbar_view build load params only
        andalso kz_json:any(fun is_filter_key/1, cb_context:query_string(Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check if only time filters are defined in query string, useful to
%% crossbar_view to not add `include_docs` if only they are defined.
%% @end
%%--------------------------------------------------------------------
-spec is_only_time_filter(cb_context:context(), kz_term:ne_binary()) -> boolean().
is_only_time_filter(Context, FilterKey) ->
    Fun = fun({<<"created_from">>, _}) -> 'true';
             ({<<"created_to">>, _}) -> 'true';
             ({<<"modified_from">>, _}) -> 'true';
             ({<<"modified_to">>, _}) -> 'true';
             ({Key, _}) ->
                  Key =/= <<FilterKey/binary, "_from">>
                      andalso Key =/= <<FilterKey/binary, "_to">>
          end,
    kz_json:all(Fun, cb_context:query_string(Context)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns 'true' if all of the requested props are found, 'false' if one is not found
%% @end
%%--------------------------------------------------------------------
-spec by_doc(kz_term:api_object(), cb_context:context()) -> boolean().
by_doc(Doc, Context) ->
    by_doc(Doc, Context, is_defined(Context)).

-spec by_doc(kz_term:api_object(), cb_context:context(), boolean()) -> boolean().
by_doc(_, _, 'false') ->
    'true';
by_doc('undefined', _, 'true') ->
    lager:debug("no doc was returned (no include_docs?)"),
    'true';
by_doc(Doc, Context, 'true') ->
    filter_doc_by_querystring(Doc, cb_context:query_string(Context)).

%%%===================================================================
%%% Load view internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Build filter function if filter is requested by the client.
%% @end
%%--------------------------------------------------------------------
-spec build(cb_context:context(), boolean()) -> filter_fun().
build(_, 'false') -> fun kz_term:always_true/1;
build(Context, 'true') ->
    fun(Doc) ->
            filter_doc_by_querystring(Doc, cb_context:query_string(Context))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Build a function with arity 2 if filter is requested by the client.
%% This function will filter documents and then applies caller map function
%% on the view result.
%% @end
%%--------------------------------------------------------------------
-spec build_filter_map_fun(cb_context:context(), filter_fun(), crossbar_view:user_mapper_fun()) -> crossbar_view:mapper_fun().
build_filter_map_fun(_, FilterFun, 'undefined') ->
    fun(JObj, Acc) ->
            case FilterFun(kz_json:get_value(<<"doc">>, JObj)) of
                'true' -> [JObj|Acc];
                'false' -> Acc
            end
    end;
build_filter_map_fun(_, FilterFun, UserMapper) when is_function(UserMapper, 1) ->
    fun(JObjs) ->
            Filtered0 = [JObj
                         || JObj <- JObjs,
                            FilterFun(kz_json:get_value(<<"doc">>, JObj))
                        ],
            UserMapper(Filtered0)
    end;
build_filter_map_fun(_, FilterFun, UserMapper) when is_function(UserMapper, 2) ->
    fun(JObj, Acc) ->
            case FilterFun(kz_json:get_value(<<"doc">>, JObj)) of
                'true' -> UserMapper(JObj, Acc);
                'false' -> Acc
            end
    end;
build_filter_map_fun(Context, FilterFun, UserMapper) when is_function(UserMapper, 3) ->
    fun(JObj, Acc) ->
            case FilterFun(kz_json:get_value(<<"doc">>, JObj)) of
                'true' -> UserMapper(Context, JObj, Acc);
                'false' -> Acc
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return `true` if key is a filter key
%% @end
%%--------------------------------------------------------------------
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

-spec should_filter_doc(kz_json:object(), kz_term:ne_binary(), kz_json:json_term()) -> boolean().
should_filter_doc(Doc, K, V) ->
    try filter_prop(Doc, K, V) of
        'undefined' -> 'true';
        Bool -> Bool
    catch
        _E:_R ->
            lager:debug("failed to process filter ~s: ~s:~p", [K, _E, _R]),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns 'true' or 'false' if the prop is found inside the doc
%% @end
%%--------------------------------------------------------------------
-spec filter_prop(kz_json:object(), kz_term:ne_binary(), any()) -> kz_term:api_boolean().
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

-spec should_filter(binary(), kz_term:ne_binary()) -> boolean().
should_filter(Val, Val) -> 'true';
should_filter(Val, FilterVal) ->
    try kz_json:unsafe_decode(FilterVal) of
        List when is_list(List) -> lists:member(Val, List);
        Val -> 'true';
        _Data ->
            lager:debug("data is not a list: ~p", [_Data]),
            'false'
    catch
        _Error -> 'false'
    end.

-spec should_filter(kz_json:object(), kz_term:ne_binary(), kz_json:json_term()) -> boolean().
should_filter(Doc, Key, Val) ->
    Keys = binary_key_to_json_key(Key),
    should_filter(kz_json:get_binary_value(Keys, Doc, <<>>)
                 ,kz_term:to_binary(Val)
                 ).

-spec has_key(kz_json:object(), kz_term:ne_binary()) -> boolean().
has_key(Doc, Key) ->
    Keys = binary_key_to_json_key(Key),
    kz_json:get_value(Keys, Doc) =/= 'undefined'.

-spec has_value(kz_json:object(), kz_term:ne_binary()) -> boolean().
has_value(Doc, Key) ->
    Keys = binary_key_to_json_key(Key),
    kz_json:get_ne_value(Keys, Doc) =/= 'undefined'.

-spec binary_key_to_json_key(kz_term:ne_binary()) -> kz_term:ne_binaries().
binary_key_to_json_key(Key) ->
    binary:split(Key, <<".">>, ['global']).
