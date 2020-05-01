%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Crossbar API for search.
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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

-define(SEARCHABLE, [<<"account">>, <<"user">>, <<"callflow">>, <<"device">>]).
-define(ACCOUNT_QUERY_OPTIONS, [<<"name">>, <<"number">>, <<"name_and_number">>]).
-define(ACCOUNTS_QUERY_OPTIONS, [<<"name">>, <<"realm">>, <<"id">>]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.search">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.search">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.authorize.search">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.validate.search">>, ?MODULE, 'validate').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?MULTI) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /skels => []
%%    /skels/foo => [<<"foo">>]
%%    /skels/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(?MULTI) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    cb_context:auth_account_id(Context) =/= 'undefined'.

-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, ?MULTI) ->
    cb_context:auth_account_id(Context) =/= 'undefined'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /skels might load a list of skel objects
%% /skels/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    Type = cb_context:req_value(Context, <<"t">>),
    validate_search(Context, Type).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?MULTI) ->
    Type = cb_context:req_value(Context, <<"t">>),
    validate_multi(Context, Type).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec validate_search(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
validate_search(Context, 'undefined') ->
    Message = kz_json:from_list([{<<"message">>, <<"search needs a document type to search on">>}
                                ,{<<"target">>, ?SEARCHABLE}
                                ]),
    lager:debug("'t' required"),
    cb_context:add_validation_error(<<"t">>, <<"required">>, Message, Context);
validate_search(Context, <<"account">>=Type) ->
    lager:debug("validating search on accounts"),
    validate_search(cb_context:set_db_name(Context, ?KZ_ACCOUNTS_DB)
                   ,Type
                   ,cb_context:req_value(Context, <<"q">>)
                   );
validate_search(Context, Type) ->
    validate_search(Context, Type, cb_context:req_value(Context, <<"q">>)).

-spec validate_search(cb_context:context(), kz_term:ne_binary(), kz_term:api_binary()) ->
          cb_context:context().
validate_search(Context, _Type, 'undefined') ->
    lager:debug("'q' required"),
    NeedViewMsg = kz_json:from_list([{<<"message">>, <<"search needs a view to search in">>}
                                    ,{<<"target">>, available_query_options(cb_context:db_name(Context))}
                                    ]),
    cb_context:add_validation_error(<<"q">>, <<"required">>, NeedViewMsg, Context);
validate_search(Context, Type, Query) ->
    Context1 = validate_query(Context, Query),
    case cb_context:resp_status(Context1) of
        'success' ->
            validate_search(Context, Type, Query, cb_context:req_value(Context, <<"v">>));
        _Status ->
            Context1
    end.

-spec validate_search(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary()) ->
          cb_context:context().
validate_search(Context, _Type, _Query, 'undefined') ->
    Message = kz_json:from_list([{<<"message">>, <<"search needs a value to search for">>}]),
    cb_context:add_validation_error(<<"v">>, <<"required">>, Message, Context);
validate_search(Context, Type, Query, <<_/binary>> = Value) ->
    search(Context, Type, Query, Value, []);
validate_search(Context, Type, Query, Value) ->
    case kz_term:is_true(Value) of
        'true' -> validate_search(Context, Type, Query, <<>>);
        'false' -> validate_search(Context, Type, Query, 'undefined')
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_multi(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
validate_multi(Context, 'undefined') ->
    Message = kz_json:from_list([{<<"message">>, <<"Search needs a document type to search on">>}
                                ,{<<"target">>, ?SEARCHABLE}
                                ]),
    cb_context:add_validation_error(<<"t">>, <<"required">>, Message, Context);
validate_multi(Context, <<"account">>=Type) ->
    lager:debug("validating search on accounts"),
    validate_multi(cb_context:set_db_name(Context, ?KZ_ACCOUNTS_DB)
                  ,Type
                  ,kz_json:to_proplist(cb_context:query_string(Context))
                  );
validate_multi(Context, Type) ->
    validate_multi(Context, Type, kz_json:to_proplist(cb_context:query_string(Context))).

-spec validate_multi(cb_context:context(), kz_term:ne_binary(), kz_term:proplist()) -> cb_context:context().
validate_multi(Context, Type, Query) ->
    Context1 = validate_query(Context, Query),
    case cb_context:resp_status(Context1) of
        'success' -> multi_search(Context1, Type, Query);
        _Status -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec validate_query(cb_context:context(), kz_term:proplist() | kz_term:ne_binary()) -> cb_context:context().
validate_query(Context, Query) ->
    QueryOptions = available_query_options(cb_context:db_name(Context)),
    validate_query(Context, QueryOptions, Query).

-spec validate_query(cb_context:context(), kz_term:proplist(), kz_term:proplist() | kz_term:ne_binary()) -> cb_context:context().
validate_query(Context, Available, []) ->
    case cb_context:resp_status(Context) of
        'success' -> Context;
        _ ->
            lager:debug("multisearch has ~p available", [Available]),
            Message = kz_json:from_list([{<<"message">>, <<"multi search needs some values to search for">>}
                                        ,{<<"target">>, Available}
                                        ]),
            cb_context:add_validation_error(<<"multi">>, <<"enum">>, Message, Context)
    end;
validate_query(Context, Available, [{<<"by_", Query/binary>>, _}|Props]) ->
    Context1 = validate_query(Context, Available, Query),
    case cb_context:resp_status(Context1) of
        'success' ->
            lager:debug("query ~s is valid", [Query]),
            validate_query(Context1, Available, Props);
        _Status ->
            lager:debug("query ~s is not valid", [Query]),
            Context1
    end;
validate_query(Context, Available, [{Query, _}|Props]) ->
    lager:debug("ignoring query string ~s", [Query]),
    validate_query(Context, Available, Props);
validate_query(Context, Available, Query) when is_binary(Query) ->
    case lists:member(Query, Available) of
        'true' -> cb_context:set_resp_status(Context, 'success');
        'false' ->
            lager:debug("query ~s not allowed", [Query]),
            Message = kz_json:from_list([{<<"message">>, <<"value not found in enumerated list of values">>}
                                        ,{<<"cause">>, Query}
                                        ]),
            cb_context:add_validation_error(<<"q">>, <<"enum">>, Message, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec available_query_options(kz_term:api_ne_binary()) -> kz_term:ne_binaries().
available_query_options(AccountDb) ->
    case kz_datamgr:open_cache_doc(AccountDb, <<"_design/search">>) of
        {'ok', JObj} ->
            lager:debug("got ~s views from ~s", [kz_json:get_keys(<<"views">>, JObj), AccountDb]),
            format_query_options(kz_json:get_keys(<<"views">>, JObj));
        {'error', _E} when AccountDb =:= ?KZ_ACCOUNTS_DB ->
            lager:debug("using default query options"),
            ?ACCOUNTS_QUERY_OPTIONS;
        {'error', _E} ->
            lager:debug("using default query options after error ~p", [_E]),
            ?ACCOUNT_QUERY_OPTIONS
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec format_query_options(kz_term:ne_binaries()) -> kz_term:ne_binaries().
format_query_options([]) ->
    lager:debug("no query options found on design doc, using default"),
    ?ACCOUNTS_QUERY_OPTIONS;
format_query_options(Views) ->
    [format_query_option(View) || View <- Views].

-spec format_query_option(kz_term:ne_binary()) -> kz_term:ne_binary().
format_query_option(<<"search_by_", Name/binary>>) -> Name;
format_query_option(Name) -> Name.

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec search(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary(), binary(), kz_term:proplist()) -> cb_context:context().
search(Context, Type, Query, Val, Opts) ->
    ViewName = <<?QUERY_TPL/binary, Query/binary>>,
    Value = cb_modules_util:normalize_alphanum_name(Val),
    Options =
        [{'startkey', get_start_key(Context, Type, Value)}
        ,{'endkey', get_end_key(Context, Type, Value)}
        ,{'mapper', crossbar_view:get_value_fun()}
         | Opts
        ],
    crossbar_view:load(Context, ViewName, Options).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------

-spec multi_search(cb_context:context(), kz_term:ne_binary(), kz_term:proplist()) -> cb_context:context().
multi_search(Context, Type, Props) ->
    Context1 = cb_context:set_should_paginate(Context, 'false'),
    multi_search(Context1, Type, Props , kz_json:new()).

-spec multi_search(cb_context:context(), kz_term:ne_binary(), kz_term:proplist(), kz_json:object()) -> cb_context:context().
multi_search(Context, _Type, [], Acc) ->
    cb_context:set_resp_data(Context, Acc);
multi_search(Context, Type, [{<<"by_", Query/binary>>, Val}|Props], Acc) ->
    Context1 = search(Context, Type, Query, Val, [{'unchunkable', 'true'}]),
    case cb_context:resp_status(Context1) of
        'success' ->
            RespData = cb_context:resp_data(Context1),
            Acc1 = kz_json:set_value(Query, RespData, Acc),
            multi_search(Context1, Type, Props, Acc1);
        _ -> Context1
    end;
multi_search(Context, Type, [_|Props], Acc) ->
    multi_search(Context, Type, Props, Acc).

%%------------------------------------------------------------------------------
%% @doc resource.
%% @end
%%------------------------------------------------------------------------------
-spec get_start_key(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binaries().
get_start_key(Context, <<"account">>=Type, Value) ->
    [cb_context:auth_account_id(Context), Type, cb_context:req_value(Context, <<"start_key">>, Value)];
get_start_key(Context, Type, Value) ->
    [Type, cb_context:req_value(Context, <<"start_key">>, Value)].

%%------------------------------------------------------------------------------
%% @doc resource.
%% @end
%%------------------------------------------------------------------------------
-spec get_end_key(cb_context:context(), kz_term:ne_binary(), binary()) -> kz_term:ne_binaries().
get_end_key(Context, <<"account">>=Type, Value) ->
    [cb_context:auth_account_id(Context), Type, next_binary_key(Value)];
get_end_key(_, Type, Value) ->
    [Type, next_binary_key(Value)].

%%------------------------------------------------------------------------------
%% @doc replaces last character in binary with next character
%% @end
%%------------------------------------------------------------------------------
-spec next_binary_key(binary()) -> kz_term:ne_binary().
next_binary_key(<<>>) ->
    <<"Z">>;
next_binary_key(Bin) ->
    <<Bin/binary, "Z">>.
