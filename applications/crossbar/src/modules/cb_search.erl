%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(cb_search).

-export([init/0
        ,allowed_methods/0 ,allowed_methods/1
        ,resource_exists/0 ,resource_exists/1
        ,validate/1 ,validate/2
        ,authorize/1, authorize/2
        ]).

-include("crossbar.hrl").

-define(QUERY_TPL, <<"search/search_by_">>).
-define(MULTI, <<"multi">>).

-define(SEARCHABLE, [<<"account">>, <<"user">>, <<"callflow">>, <<"devices">>]).
-define(ACCOUNT_QUERY_OPTIONS, [<<"name">>, <<"number">>, <<"name_and_number">>]).
-define(ACCOUNTS_QUERY_OPTIONS, [<<"name">>, <<"realm">>, <<"id">>]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.search">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.search">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.authorize.search">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.validate.search">>, ?MODULE, 'validate').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

allowed_methods(?MULTI) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /skels => []
%%    /skels/foo => [<<"foo">>]
%%    /skels/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(?MULTI) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context) ->
    cb_context:auth_account_id(Context) =/= 'undefined'.

authorize(Context, ?MULTI) ->
    cb_context:auth_account_id(Context) =/= 'undefined'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /skels mights load a list of skel objects
%% /skels/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    Type = cb_context:req_value(Context, <<"t">>),
    validate_search(Context, Type).

validate(Context, ?MULTI) ->
    Type = cb_context:req_value(Context, <<"t">>),
    Ctx = case cb_context:account_id(Context) of
              'undefined' -> cb_context:set_account_db(Context, ?KZ_ACCOUNTS_DB);
              _AccountId -> Context
          end,
    validate_multi(Ctx, Type).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_search(cb_context:context(), api_binary()) -> cb_context:context().
-spec validate_search(cb_context:context(), ne_binary(), api_binary()) ->
                             cb_context:context().
-spec validate_search(cb_context:context(), ne_binary(), ne_binary(), api_binary()) ->
                             cb_context:context().
validate_search(Context, 'undefined') ->
    cb_context:add_validation_error(<<"t">>
                                   ,<<"required">>
                                   ,kz_json:from_list([{<<"message">>, <<"Search needs a document type to search on">>}])
                                   ,Context
                                   );
validate_search(Context, <<"account">>=Type) ->
    validate_search(cb_context:set_account_db(Context, ?KZ_ACCOUNTS_DB)
                   ,Type
                   ,cb_context:req_value(Context, <<"q">>)
                   );
validate_search(Context, Type) ->
    validate_search(Context, Type, cb_context:req_value(Context, <<"q">>)).

validate_search(Context, _Type, 'undefined') ->
    Context1 = cb_context:add_validation_error(<<"q">>
                                              ,<<"required">>
                                              ,kz_json:from_list([{<<"message">>, <<"Search needs a view to search in">>}])
                                              ,Context
                                              ),
    cb_context:add_validation_error(<<"q">>
                                   ,<<"enum">>
                                   ,kz_json:from_list(
                                      [{<<"message">>, <<"Value not found in enumerated list of values">>}
                                      ,{<<"target">>, query_options(cb_context:account_db(Context1))}
                                      ])
                                   ,Context1
                                   );
validate_search(Context, Type, Query) ->
    Context1 = validate_query(Context, Query),
    case cb_context:resp_status(Context1) of
        'success' ->
            validate_search(Context, Type, Query, cb_context:req_value(Context, <<"v">>));
        _Status ->
            Context1
    end.

validate_search(Context, _Type, _Query, 'undefined') ->
    cb_context:add_validation_error(<<"v">>
                                   ,<<"required">>
                                   ,kz_json:from_list([{<<"message">>, <<"Search needs a value to search with">>}])
                                   ,Context
                                   );
validate_search(Context, Type, Query, <<_/binary>> = Value) ->
    search(Context, Type, Query, Value);
validate_search(Context, Type, Query, Value) ->
    case kz_term:is_true(Value) of
        'true' -> validate_search(Context, Type, Query, <<>>);
        'false' -> validate_search(Context, Type, Query, 'undefined')
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_multi(cb_context:context(), api_binary()) -> cb_context:context().
-spec validate_multi(cb_context:context(), ne_binary(), kz_proplist()) -> cb_context:context().
validate_multi(Context, 'undefined') ->
    cb_context:add_validation_error(<<"t">>
                                   ,<<"required">>
                                   ,kz_json:from_list([{<<"message">>, <<"Search needs a document type to search on">>}])
                                   ,Context
                                   );
validate_multi(Context, Type) ->
    case kz_json:to_proplist(cb_context:query_string(Context)) of
        [_|_]=Props -> validate_multi(Context, Type, Props);
        _Other ->
            cb_context:add_validation_error(<<"multi">>
                                           ,<<"enum">>
                                           ,kz_json:from_list([{<<"message">>, <<"Multi Search needs something to search like a doc type">>}
                                                              ,{<<"target">>, ?SEARCHABLE}
                                                              ])
                                           ,Context
                                           )
    end.

validate_multi(Context, Type, Props) ->
    QueryOptions = query_options(cb_context:account_db(Context)),
    Context1 = validate_query(Context, QueryOptions, Props),
    case cb_context:resp_status(Context1) of
        'success' ->
            multi_search(Context1, Type, Props);
        _Status ->
            Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_query(cb_context:context(), ne_binary()) -> cb_context:context().
-spec validate_query(cb_context:context(), kz_proplist(), kz_proplist() | ne_binary()) -> cb_context:context().
validate_query(Context, Query) ->
    QueryOptions = query_options(cb_context:account_db(Context)),
    validate_query(Context, QueryOptions, Query).

validate_query(Context, _Available, []) ->
    cb_context:set_resp_status(Context, 'success');
validate_query(Context, Available, [{<<"by_", Query/binary>>, _}|Props]) ->
    Context1 = validate_query(Context, Available, Query),
    case cb_context:resp_status(Context1) of
        'success' ->
            validate_query(Context1, Available, Props);
        _Status ->
            Context1
    end;
validate_query(Context, Available, [{Query, _}|Props]) ->
    lager:debug("ignoring query string ~s", [Query]),
    validate_query(Context, Available, Props);
validate_query(Context, Available, Query) when is_binary(Query) ->
    case lists:member(Query, Available) of
        'true' ->
            cb_context:set_resp_status(Context, 'success');
        'false' ->
            cb_context:add_validation_error(<<"q">>
                                           ,<<"enum">>
                                           ,kz_json:from_list([{<<"message">>, <<"Value not found in enumerated list of values">>}
                                                              ,{<<"target">>, Available}
                                                              ,{<<"cause">>, Query}
                                                              ])
                                           ,Context
                                           )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec query_options(ne_binary()) -> ne_binaries().
query_options(AccountDb) ->
    case kz_datamgr:open_cache_doc(AccountDb, <<"_design/search">>) of
        {'ok', JObj} ->
            format_query_options(kz_json:get_keys(<<"views">>, JObj));
        {'error', _E} when AccountDb =:= ?KZ_ACCOUNTS_DB ->
            ?ACCOUNTS_QUERY_OPTIONS;
        {'error', _E} ->
            ?ACCOUNT_QUERY_OPTIONS
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format_query_options(ne_binaries()) -> ne_binaries().
-spec format_query_option(ne_binary()) -> ne_binary().
format_query_options(Views) ->
    [format_query_option(View) || View <- Views].

format_query_option(<<"search_by_", Name/binary>>) -> Name;
format_query_option(Name) -> Name.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec search(cb_context:context(), ne_binary(), ne_binary(), binary()) -> cb_context:context().
search(Context, Type, Query, Val) ->
    ViewName = <<?QUERY_TPL/binary, Query/binary>>,
    Value = maybe_normalize_value(Type, Val),
    ViewOptions =
        [{'startkey', get_start_key(Context, Type, Value)}
        ,{'endkey', get_end_key(Context, Type, Value)}
        ,{'limit', crossbar_doc:pagination_page_size(Context)}
        ],
    fix_envelope(
      crossbar_doc:load_view(ViewName
                            ,ViewOptions
                            ,Context
                            ,fun normalize_view_results/2
                            )
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec multi_search(cb_context:context(), ne_binary(), kz_proplist()) -> cb_context:context().
-spec multi_search(cb_context:context(), ne_binary(), kz_proplist(), kz_json:object()) -> cb_context:context().
multi_search(Context, Type, Props) ->
    Context1 = cb_context:set_should_paginate(Context, 'false'),
    multi_search(Context1, Type, Props , kz_json:new()).

multi_search(Context, _Type, [], Acc) ->
    cb_context:set_resp_data(Context, Acc);
multi_search(Context, Type, [{<<"by_", Query/binary>>, Val}|Props], Acc) ->
    ViewName = <<?QUERY_TPL/binary, Query/binary>>,
    Value = maybe_normalize_value(Type, Val),
    ViewOptions =
        [{'startkey', get_start_key(Context, Type, Value)}
        ,{'endkey', get_end_key(Context, Type, Value)}
        ],
    Context1 =
        crossbar_doc:load_view(ViewName
                              ,ViewOptions
                              ,Context
                              ,fun normalize_view_results/2
                              ),
    case cb_context:resp_status(Context1) of
        'success' ->
            RespData = cb_context:resp_data(Context1),
            Acc1 = kz_json:set_value(Query, RespData, Acc),
            multi_search(Context1, Type, Props, Acc1);
        _ -> Context1
    end;
multi_search(Context, Type, [_|Props], Acc) ->
    multi_search(Context, Type, Props, Acc).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalize search term for using in accounts search view
%% @end
%%--------------------------------------------------------------------
-spec maybe_normalize_value(ne_binary(), ne_binary()) -> ne_binary().
maybe_normalize_value(<<"account">>, Value) ->
    kz_term:to_lower_binary(Value);
maybe_normalize_value(_, Value) ->
    Value.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec get_start_key(cb_context:context(), ne_binary(), ne_binary()) -> ne_binaries().
get_start_key(Context, <<"account">>=Type, Value) ->
    StartKey = cb_context:req_value(Context, <<"start_key">>, Value),
    case cb_context:account_id(Context) of
        'undefined' ->
            AuthId = cb_context:auth_account_id(Context),
            [AuthId, Type, StartKey];
        AccountId ->
            [AccountId, Type, StartKey]
    end;
get_start_key(Context, Type, Value) ->
    StartKey = cb_context:req_value(Context, <<"start_key">>, Value),
    case cb_context:account_id(Context) of
        'undefined' ->
            AuthId = cb_context:auth_account_id(Context),
            [AuthId, Type, StartKey];
        _ ->
            [Type, StartKey]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec get_end_key(cb_context:context(), ne_binary(), binary()) -> ne_binaries().
get_end_key(Context, <<"account">>=Type, Value) ->
    case cb_context:account_id(Context) of
        'undefined' ->
            AuthId = cb_context:auth_account_id(Context),
            [AuthId, Type, next_binary_key(Value)];
        AccountId ->
            [AccountId, Type, next_binary_key(Value)]
    end;
get_end_key(Context, Type, Value) ->
    case cb_context:account_id(Context) of
        'undefined' ->
            AuthId = cb_context:auth_account_id(Context),
            [AuthId, Type, next_binary_key(Value)];
        _AccountId ->
            [Type, next_binary_key(Value)]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% replaces last character in binary with next character
%% @end
%%--------------------------------------------------------------------
-spec next_binary_key(binary()) -> ne_binary().
next_binary_key(<<>>) ->
    <<"\ufff0">>;
next_binary_key(Bin) ->
    <<Bin/binary, "\ufff0">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec fix_envelope(cb_context:context()) -> cb_context:context().
fix_envelope(Context) ->
    RespContext = cb_context:set_resp_data(Context, lists:reverse(cb_context:resp_data(Context))),
    RespEnvelope = lists:foldl(fun fix_envelope_fold/2
                              ,cb_context:resp_envelope(Context)
                              ,[<<"start_key">>, <<"next_start_key">>]
                              ),

    cb_context:set_resp_envelope(RespContext, RespEnvelope).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec fix_envelope_fold(binary(), kz_json:object()) -> kz_json:object().
fix_envelope_fold(Key, JObj) ->
    case fix_start_key(kz_json:get_value(Key, JObj)) of
        'undefined' -> kz_json:delete_key(Key, JObj);
        V -> kz_json:set_value(Key, V, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec fix_start_key(api_binaries()) -> api_binary().
fix_start_key('undefined') -> 'undefined';
fix_start_key([_ , StartKey]) -> StartKey;
fix_start_key([_ , _, StartKey]) -> StartKey.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the results of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].
