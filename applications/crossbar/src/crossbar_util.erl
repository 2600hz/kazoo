%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(crossbar_util).

-export([response/2, response/3, response/4, response/5]).
-export([response_deprecated/1, response_deprecated_redirect/2, response_deprecated_redirect/3
         ,response_redirect/3, response_redirect/4
        ]).
-export([response_202/2]).
-export([response_402/2]).
-export([response_faulty_request/1]).
-export([response_bad_identifier/2]).
-export([response_conflicting_docs/1]).
-export([response_datastore_timeout/1]).
-export([response_datastore_conn_refused/1]).
-export([response_invalid_data/2]).
-export([response_missing_view/1]).
-export([response_db_missing/1]).
-export([response_db_fatal/1]).
-export([response_auth/1
         ,response_auth/2
         ,response_auth/3
        ]).
-export([get_account_realm/1, get_account_realm/2]).
-export([move_account/2]).
-export([get_descendants/1]).
-export([get_tree/1]).
-export([replicate_account_definition/1]).
-export([disable_account/1, enable_account/1, change_pvt_enabled/2]).
-export([get_path/2]).
-export([get_user_lang/2, get_account_lang/1]).
-export([get_user_timezone/2, get_account_timezone/1]).
-export([apply_response_map/2]).
-export([maybe_remove_attachments/1]).

-export([create_auth_token/2]).
-export([generate_year_month_sequence/2
         ,generate_year_month_sequence/3
        ]).

-include("crossbar.hrl").

-define(DEFAULT_LANGUAGE
        ,whapps_config:get(?CONFIG_CAT, <<"default_language">>, <<"en-US">>)
       ).

-type fails() :: 'error' | 'fatal'.
-type year_month_tuple() :: {pos_integer(),pos_integer()}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function set the response status to success, and load the provided
%% data.
%% @end
%%--------------------------------------------------------------------
-spec response(wh_json:json_term(), cb_context:context()) ->
                      cb_context:context().
response(JTerm, Context) ->
    create_response('success', 'undefined', 'undefined', JTerm, Context).

-spec response_202(wh_json:json_string(), cb_context:context()) ->
                          cb_context:context().
response_202(Msg, Context) ->
    create_response('success', Msg, 202, Msg, Context).

-spec response_402(wh_json:object(), cb_context:context()) ->
                          cb_context:context().
response_402(Data, Context) ->
    create_response('error', <<"accept charges">>, 402, Data, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function load the error message into a 500 response, of type
%% fatal or error.
%% @end
%%--------------------------------------------------------------------
-spec response(fails(), wh_json:json_string(), cb_context:context()) ->
                      cb_context:context().
response('error', Msg, Context) ->
    create_response('error', Msg, 500, wh_json:new(), Context);
response('fatal', Msg, Context) ->
    create_response('fatal', Msg, 500, wh_json:new(), Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function load the error message into a specifc code response,
%% of type fatal or error.
%% @end
%%--------------------------------------------------------------------
-spec response(fails(), wh_json:json_string(), api_integer(), cb_context:context()) ->
                      cb_context:context().
response('error', Msg, Code, Context) ->
    create_response('error', Msg, Code, wh_json:new(), Context);
response('fatal', Msg, Code, Context) ->
    create_response('fatal', Msg, Code, wh_json:new(), Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function load the error message into a specifc code response,
%% of type fatal or error with additional data
%% @end
%%--------------------------------------------------------------------
-spec response(fails(), wh_json:json_string(), api_integer(), wh_json:json_term(), cb_context:context()) -> cb_context:context().
response('error', Msg, Code, JTerm, Context) ->
    create_response('error', Msg, Code, JTerm, Context);
response('fatal', Msg, Code, JTerm, Context) ->
    create_response('fatal', Msg, Code, JTerm, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function loads the response vars in Context, soon it will
%% make smarter chooices about formating resp_data and filtering
%% other parameters.
%% @end
%%--------------------------------------------------------------------
-spec create_response(crossbar_status(), wh_json:json_string(), api_integer()
                      ,wh_json:json_term(), cb_context:context()
                     ) -> cb_context:context().
create_response(Status, Msg, Code, JTerm, Context) ->
    cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, Status}
                         ,{fun cb_context:set_resp_error_msg/2, Msg}
                         ,{fun cb_context:set_resp_error_code/2, Code}
                         ,{fun cb_context:set_resp_data/2, JTerm}
                        ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the request is faulty (doesnt have a
%% match in validation, or someother issue with it keeps it from being
%% processed, like nonsensical chains)
%% @end
%%--------------------------------------------------------------------
-spec response_faulty_request(cb_context:context()) -> cb_context:context().
response_faulty_request(Context) ->
    response('error', <<"faulty request">>, 404, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% When a module is no longer valid, alert the client of the deprecated status
%% by either sending a 410 Gone or a 301 Redirct (when using the arity
%% 3 version.
%%
%% The RedirectUrl should be relative to the accessed URL. So, if the
%% URL accessed that is deprecated is:
%% /v1/account/{AID}/module/{MID}
%% and that MID moved to module2, the RedirectURL should be:
%% ../../module2/{MID}
%%
%% If redirecting from module1 to module2, RedirectURL should be:
%% ../module2
%% @end
%%--------------------------------------------------------------------
-spec response_deprecated(cb_context:context()) ->
                                 cb_context:context().
response_deprecated(Context) ->
    create_response('error', <<"deprecated">>, 410, wh_json:new(), Context).

-spec response_deprecated_redirect(cb_context:context(), ne_binary()) ->
                                          cb_context:context().
-spec response_deprecated_redirect(cb_context:context(), ne_binary(), wh_json:object()) ->
                                          cb_context:context().
response_deprecated_redirect(Context, RedirectUrl) ->
    response_deprecated_redirect(Context, RedirectUrl, wh_json:new()).
response_deprecated_redirect(Context, RedirectUrl, JObj) ->
    create_response('error', <<"deprecated">>, 301, JObj
                    ,cb_context:add_resp_header(Context, <<"Location">>, RedirectUrl)
                   ).

-spec response_redirect(cb_context:context(), ne_binary(), wh_json:object()) ->
                               cb_context:context().
response_redirect(Context, RedirectUrl, JObj) ->
    response_redirect(Context, RedirectUrl, JObj, 301).

-spec response_redirect(cb_context:context(), ne_binary(), wh_json:object(), pos_integer()) ->
                               cb_context:context().
response_redirect(Context, RedirectUrl, JObj, Redirect) ->
    create_response('error', <<"redirect">>, Redirect, JObj
                    ,cb_context:add_resp_header(Context, <<"Location">>, RedirectUrl)
                   ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the requested ID did not match a
%% data record. Using 404 as 410 is a permanent Gone, while 404 is
%% a softer not found now.
%% @end
%%--------------------------------------------------------------------
-spec response_bad_identifier(ne_binary(), cb_context:context()) ->
                                     cb_context:context().
response_bad_identifier(?NE_BINARY = Id, Context) ->
    response('error', <<"bad identifier">>, 404, [Id], Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the requested resource update fails
%% because of a conflict in the DB
%% @end
%%--------------------------------------------------------------------
-spec response_conflicting_docs(cb_context:context()) ->
                                       cb_context:context().
response_conflicting_docs(Context) ->
    response('error', <<"conflicting documents">>, 409, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the requested data query was missing
%% @end
%%--------------------------------------------------------------------
-spec response_missing_view(cb_context:context()) ->
                                   cb_context:context().
response_missing_view(Context) ->
    response('fatal', <<"datastore missing view">>, 503, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the datastore time'd out
%% @end
%%--------------------------------------------------------------------
-spec response_datastore_timeout(cb_context:context()) ->
                                        cb_context:context().
response_datastore_timeout(Context) ->
    response('error', <<"datastore timeout">>, 503, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the datastore time'd out
%% @end
%%--------------------------------------------------------------------
-spec response_datastore_conn_refused(cb_context:context()) ->
                                             cb_context:context().
response_datastore_conn_refused(Context) ->
    response('error', <<"datastore connection refused">>, 503, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the provided data did not validate
%% @end
%%--------------------------------------------------------------------
-spec response_invalid_data(wh_json:json_term(), cb_context:context()) ->
                                   cb_context:context().
response_invalid_data(JTerm, Context) ->
    response('error', <<"invalid data">>, 400, JTerm, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the datastore does not have the requested
%% record collection
%% @end
%%--------------------------------------------------------------------
-spec response_db_missing(cb_context:context()) -> cb_context:context().
response_db_missing(Context) ->
    response('fatal', <<"data collection missing: database not found">>, 503, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the datastore does not have the requested
%% record collection
%% @end
%%--------------------------------------------------------------------
-spec response_db_fatal(cb_context:context()) -> cb_context:context().
response_db_fatal(Context) ->
    response('fatal', <<"datastore fatal error">>, 503, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieves the account realm
%% @end
%%--------------------------------------------------------------------
-spec get_account_realm(ne_binary() | cb_context:context()) -> api_binary().
-spec get_account_realm(api_binary(), ne_binary()) -> api_binary().

get_account_realm(AccountId) when is_binary(AccountId) ->
    get_account_realm(wh_util:format_account_id(AccountId, 'encoded'), AccountId);
get_account_realm(Context) ->
    Db = cb_context:account_db(Context),
    AccountId = cb_context:account_id(Context),
    get_account_realm(Db, AccountId).

get_account_realm('undefined', _) -> 'undefined';
get_account_realm(Db, AccountId) ->
    case couch_mgr:open_cache_doc(Db, AccountId) of
        {'ok', JObj} ->
            wh_json:get_ne_value(<<"realm">>, JObj);
        {'error', R} ->
            lager:debug("error while looking up account realm: ~p", [R]),
            'undefined'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec move_account(ne_binary(), ne_binary()) -> {'ok', wh_json:object()} | {'error',any()}.
move_account(AccountId, ToAccount) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case validate_move(AccountId, ToAccount) of
        {'error', _E}=Error -> Error;
        {'ok', JObj, ToTree} ->
            PreviousTree = wh_json:get_value(<<"pvt_tree">>, JObj),
            JObj1 = wh_json:set_values([{<<"pvt_tree">>, ToTree}
                                        ,{<<"pvt_previous_tree">>, PreviousTree}
                                        ,{<<"pvt_modified">>, wh_util:current_tstamp()}
                                       ], JObj),
            case couch_mgr:save_doc(AccountDb, JObj1) of
                {'error', _E}=Error -> Error;
                {'ok', _} ->
                    {'ok', _} = replicate_account_definition(JObj1),
                    {'ok', _} = move_descendants(AccountId, ToTree),
                    {'ok', _} = mark_dirty(AccountId),
                    move_service(AccountId, ToTree, 'true')
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_move(ne_binary(), ne_binary()) -> {'error', _} | {'ok', wh_json:object(), ne_binaries()}.
validate_move(AccountId, ToAccount) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {'error', _E}=Error -> Error;
        {'ok', JObj} ->
            ToTree = lists:append(crossbar_util:get_tree(ToAccount), [ToAccount]),
            case lists:member(AccountId, ToTree) of
                'true' -> {'error', 'forbidden'};
                'false' -> {'ok', JObj, ToTree}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec move_descendants(ne_binary(), ne_binaries()) -> {'ok', wh_json:object()} | {'error',any()}.
move_descendants(AccountId, Tree) when is_binary(AccountId) ->
    Descendants = crossbar_util:get_descendants(AccountId),
    move_descendants(Descendants, Tree);
move_descendants([], _) -> {'ok', 'done'};
move_descendants([Descendant|Descendants], Tree) ->
    AccountId = wh_util:format_account_id(Descendant, 'raw'),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {'error', _E}=Error -> Error;
        {'ok', JObj} ->
            PreviousTree = wh_json:get_value(<<"pvt_tree">>, JObj),
            {_, Tail} = lists:split(erlang:length(Tree), PreviousTree),
            ToTree = Tree ++ Tail,
            JObj1 = wh_json:set_values([{<<"pvt_tree">>, ToTree}
                                        ,{<<"pvt_previous_tree">>, PreviousTree}
                                        ,{<<"pvt_modified">>, wh_util:current_tstamp()}
                                       ], JObj),
            case couch_mgr:save_doc(AccountDb, JObj1) of
                {'error', _E}=Error -> Error;
                {'ok', _} ->
                    {'ok', _} = replicate_account_definition(JObj1),
                    {'ok', _} = move_service(AccountId, ToTree, 'undefined'),
                    move_descendants(Descendants, ToTree)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec move_service(ne_binary(), ne_binaries(), 'undefined' | 'true' | 'false') ->
                          {'ok', wh_json:object()} | {'error',any()}.
move_service(AccountId, NewTree, Dirty) ->
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {'error', _E}=Error -> Error;
        {'ok', JObj} ->
            PreviousTree = wh_json:get_value(<<"pvt_tree">>, JObj),
            Props = props:filter_undefined([{<<"pvt_tree">>, NewTree}
                                            ,{<<"pvt_dirty">>, Dirty}
                                            ,{<<"pvt_previous_tree">>, PreviousTree}
                                            ,{<<"pvt_modified">>, wh_util:current_tstamp()}
                                           ]),
            JObj1 = wh_json:set_values(Props, JObj),
            case couch_mgr:save_doc(?WH_SERVICES_DB, JObj1) of
                {'error', _E}=Error -> Error;
                {'ok', _R}=Ok -> Ok
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return all descendants of the account id
%% @end
%%--------------------------------------------------------------------
-spec get_descendants(api_binary()) -> 'ok' | {'error', _}.
get_descendants('undefined') -> [];
get_descendants(AccountId) ->
    ViewOptions = [{<<"startkey">>, [AccountId]}
                   ,{<<"endkey">>, [AccountId, wh_json:new()]}
                  ],
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, <<"accounts/listing_by_descendants">>, ViewOptions) of
        {'ok', JObjs} ->
            lists:foldl(
                fun(JObj, Acc) ->
                    Id = wh_json:get_value(<<"id">>, JObj),
                    case Id =:= AccountId of
                        'true' -> Acc;
                        'false' -> [Id|Acc]
                    end
                end
                ,[]
                ,JObjs
            );
        {'error', R} ->
            lager:debug("unable to disable descendants of ~s: ~p", [AccountId, R]),
            []
    end.

-spec mark_dirty(ne_binary() | wh_json:object()) -> wh_std_return().
mark_dirty(AccountId) when is_binary(AccountId) ->
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {'error', _}=E -> E;
        {'ok', JObj} -> mark_dirty(JObj)
    end;
mark_dirty(JObj) ->
    couch_mgr:save_doc(?WH_SERVICES_DB
                       ,wh_json:set_values([{<<"pvt_dirty">>, 'true'}
                                            ,{<<"pvt_modified">>, wh_util:current_tstamp()}
                                           ], JObj)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_tree(ne_binary()) -> ne_binaries().
get_tree(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', JObj} -> wh_json:get_value(<<"pvt_tree">>, JObj, []);
        {'error', _E} ->
            lager:error("could not load ~s in ~s", [AccountId, AccountDb]),
            []
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec replicate_account_definition(wh_json:object()) ->
                                          {'ok', wh_json:object()} |
                                          {'error', _}.
replicate_account_definition(JObj) ->
    AccountId = wh_json:get_value(<<"_id">>, JObj),
    case couch_mgr:lookup_doc_rev(?WH_ACCOUNTS_DB, AccountId) of
        {'ok', Rev} ->
            couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, wh_json:set_value(<<"_rev">>, Rev, JObj));
        _Else ->
            couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, wh_json:delete_key(<<"_rev">>, JObj))
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Flag all descendants of the account id as disabled
%% @end
%%--------------------------------------------------------------------
-spec disable_account(api_binary()) -> 'ok' | {'error', _}.
disable_account('undefined') -> 'ok';
disable_account(AccountId) ->
    ViewOptions = [{<<"startkey">>, [AccountId]}
                   ,{<<"endkey">>, [AccountId, wh_json:new()]}
                  ],
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, <<"accounts/listing_by_descendants">>, ViewOptions) of
        {'ok', JObjs} ->
            _ = [change_pvt_enabled('false', wh_json:get_value(<<"id">>, JObj)) || JObj <- JObjs],
            'ok';
        {'error', R}=E ->
            lager:debug("unable to disable descendants of ~s: ~p", [AccountId, R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Flag all descendants of the account id as enabled
%% @end
%%--------------------------------------------------------------------
-spec enable_account(api_binary()) -> 'ok' | {'error', _}.
enable_account('undefined') -> ok;
enable_account(AccountId) ->
    ViewOptions = [{<<"startkey">>, [AccountId]}
                   ,{<<"endkey">>, [AccountId, wh_json:new()]}
                  ],
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, <<"accounts/listing_by_descendants">>, ViewOptions) of
        {'ok', JObjs} ->
            _ = [change_pvt_enabled('true', wh_json:get_value(<<"id">>, JObj)) || JObj <- JObjs],
            'ok';
        {'error', R}=E ->
            lager:debug("unable to enable descendants of ~s: ~p", [AccountId, R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Helper to set data for all auth type
%% @end
%%--------------------------------------------------------------------
-spec response_auth(wh_json:object()) ->
                           wh_json:object().
-spec response_auth(wh_json:object(), api_binary()) ->
                           wh_json:object().
-spec response_auth(wh_json:object(), api_binary(), api_binary()) ->
                           wh_json:object().
response_auth(JObj) ->
    response_auth(JObj
                  ,wh_json:get_first_defined([<<"account_id">>, <<"pvt_account_id">>], JObj)
                  ,wh_json:get_first_defined([<<"owner_id">>, <<"user_id">>], JObj)
                 ).

response_auth(JObj, AccountId) ->
     UserId  = wh_json:get_value(<<"owner_id">>, JObj),
     response_auth(JObj, AccountId, UserId).

response_auth(JObj, AccountId, UserId) ->
    populate_resp(JObj, AccountId, UserId).

-spec populate_resp(wh_json:object(), api_binary(), api_binary()) -> wh_json:object().
populate_resp(JObj, 'undefined', _UserId) -> JObj;
populate_resp(JObj, AccountId, UserId) ->
    wh_json:set_values(
      props:filter_undefined(
        [{<<"apps">>, load_apps(AccountId, UserId)}
         ,{<<"language">>, get_language(AccountId, UserId)}
         ,{<<"account_name">>, whapps_util:get_account_name(AccountId)}
         ,{<<"is_reseller">>, wh_services:is_reseller(AccountId)}
         ,{<<"reseller_id">>, wh_services:find_reseller_id(AccountId)}
        ])
      ,JObj
     ).

-spec load_apps(ne_binary(), ne_binary()) -> api_object().
load_apps(AccountId, UserId) ->
    MasterAccountDb = get_master_account_db(),
    Lang = get_language(AccountId, UserId),
    case couch_mgr:get_all_results(MasterAccountDb, <<"apps_store/crossbar_listing">>) of
        {'error', _E} ->
            lager:error("failed to load lookup apps in ~p", [MasterAccountDb]),
            'undefined';
        {'ok', JObjs} -> filter_apps(JObjs, Lang)
    end.

-spec filter_apps(wh_json:objects(), ne_binary()) -> wh_json:objects().
-spec filter_apps(wh_json:objects(), ne_binary(), wh_json:objects()) -> wh_json:objects().
filter_apps(JObjs, Lang) ->
    filter_apps(JObjs, Lang, []).

filter_apps([], _, Acc) -> Acc;
filter_apps([JObj|JObjs], Lang, Acc) ->
    App = wh_json:get_value(<<"value">>, JObj, wh_json:new()),
    DefaultLabel = wh_json:get_value([<<"i18n">>, ?DEFAULT_LANGUAGE, <<"label">>], App),
    FormatedApp =
        wh_json:from_list(
            props:filter_undefined(
                [{<<"id">>, wh_json:get_value(<<"id">>, App)}
                 ,{<<"name">>, wh_json:get_value(<<"name">>, App)}
                 ,{<<"api_url">>, wh_json:get_value(<<"api_url">>, App)}
                 ,{<<"source_url">>, wh_json:get_value(<<"source_url">>, App)}
                 ,{<<"label">>, wh_json:get_value([<<"i18n">>, Lang, <<"label">>], App, DefaultLabel)}
                ]
            )
        ),
    filter_apps(JObjs, Lang, [FormatedApp|Acc]).

-spec get_language(ne_binary(), ne_binary()) -> ne_binary().
get_language(AccountId, UserId) ->
    case ?MODULE:get_user_lang(AccountId, UserId) of
        {'ok', Lang} -> Lang;
        'error' ->
            case ?MODULE:get_account_lang(AccountId) of
                {'ok', Lang} -> Lang;
                'error' -> ?DEFAULT_LANGUAGE
            end
    end.

-spec get_master_account_db() -> ne_binary().
get_master_account_db() ->
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    wh_util:format_account_id(MasterAccountId, 'encoded').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Update all descendants of the account id pvt_enabled flag with State
%% @end
%%--------------------------------------------------------------------
change_pvt_enabled(_, 'undefined') -> 'ok';
change_pvt_enabled(State, AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    try
        {'ok', JObj1} = couch_mgr:open_doc(AccountDb, AccountId),
        lager:debug("set pvt_enabled to ~s on account ~s", [State, AccountId]),
        {'ok', JObj2} = couch_mgr:ensure_saved(AccountDb, wh_json:set_value(<<"pvt_enabled">>, State, JObj1)),
        case couch_mgr:lookup_doc_rev(?WH_ACCOUNTS_DB, AccountId) of
            {'ok', Rev} ->
                couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, wh_json:set_value(<<"_rev">>, Rev, JObj2));
            _Else ->
                couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, wh_json:delete_key(<<"_rev">>, JObj2))
        end
    catch
        _:R ->
            lager:debug("unable to set pvt_enabled to ~s on account ~s: ~p", [State, AccountId, R]),
            {'error', R}
    end.

-spec get_user_lang(ne_binary(), ne_binary()) -> 'error' | {'ok', ne_binary()}.
get_user_lang(AccountId, UserId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, UserId) of
        {'ok', JObj} ->
            case wh_json:get_value(<<"language">>, JObj) of
                'undefined' -> 'error';
                Lang -> {'ok', Lang}
            end;
        {'error', _E} ->
            lager:error("failed to lookup user ~p in ~p : ~p", [UserId, AccountId, _E]),
            'error'
    end.

-spec get_account_lang(ne_binary()) -> 'error' | {'ok', ne_binary()}.
get_account_lang(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', JObj} ->
            case wh_json:get_value(<<"language">>, JObj) of
                'undefined' -> 'error';
                Lang -> {'ok', Lang}
            end;
        {'error', _E} ->
            lager:error("failed to lookup account ~p : ~p", [AccountId, _E]),
            'error'
    end.

-spec get_user_timezone(api_binary(), api_binary()) -> api_binary().
get_user_timezone(AccountId, 'undefined') ->
    get_account_timezone(AccountId);
get_user_timezone(AccountId, UserId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, UserId) of
        {'ok', JObj} ->
            wh_json:get_value(<<"timezone">>, JObj);
        {'error', _E} -> get_account_timezone(AccountId)
    end.

-spec get_account_timezone(api_binary()) -> api_binary().
get_account_timezone('undefined') ->
    'undefined';
get_account_timezone(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', JObj} ->
            wh_json:get_value(<<"timezone">>, JObj);
        {'error', _E} -> 'undefined'
    end.

-spec apply_response_map(cb_context:context(), wh_proplist()) -> cb_context:context().
apply_response_map(Context, Map) ->
    JObj = cb_context:doc(Context),
    Id = wh_json:get_first_defined([<<"_id">>,<<"Id">>], JObj),
    RespJObj = cb_context:resp_data(Context),
    RespData = lists:foldl(
                 fun({Key, Fun}, J) when is_function(Fun,1) ->
                         wh_json:set_value(Key,Fun(JObj),J);
                    ({Key, Fun}, J) when is_function(Fun,2) ->
                         wh_json:set_value(Key,Fun(Id, JObj),J);
                    ({Key, ExistingKey}, J) ->
                         wh_json:set_value(Key, wh_json:get_value(ExistingKey, JObj), J)
                 end, RespJObj, Map),
    cb_context:set_resp_data(Context, RespData).

-spec get_path(cowboy_req:req() | ne_binary(), ne_binary()) -> ne_binary().
get_path(<<_/binary>> = RawPath, Relative) ->
    PathTokensRev = lists:reverse(binary:split(RawPath, <<"/">>, ['global'])),
    UrlTokens = binary:split(Relative, <<"/">>),

    wh_util:join_binary(
      lists:reverse(
        lists:foldl(fun get_path_fold/2, PathTokensRev, UrlTokens)
       ), <<"/">>);
get_path(Req, Relative) ->
    {RawPath, _} = cowboy_req:path(Req),
    get_path(RawPath, Relative).

get_path_fold(Segment, [<<>> | PathTokens]) ->
    get_path_fold(Segment, PathTokens);
get_path_fold(<<"..">>, []) -> [];
get_path_fold(<<"..">>, [_ | PathTokens]) -> PathTokens;
get_path_fold(<<".">>, PathTokens) -> PathTokens;
get_path_fold(<<>>, PathTokens) -> PathTokens;
get_path_fold(Segment, PathTokens) -> [Segment | PathTokens].

-spec maybe_remove_attachments(cb_context:context()) -> cb_context:context().
maybe_remove_attachments(Context) ->
    JObj = cb_context:doc(Context),
    maybe_remove_attachments(Context, JObj
                             ,wh_json:get_value(<<"_attachments">>, JObj)
                            ).
maybe_remove_attachments(Context, _JObj, 'undefined') ->
    Context;
maybe_remove_attachments(Context, JObj, _Attachments) ->
    lager:debug("removing old attachments"),
    crossbar_doc:save(cb_context:set_doc(Context
                                         ,wh_json:delete_key(<<"_attachments">>, JObj)
                                        )).

-spec create_auth_token(cb_context:context(), atom()) -> cb_context:context().
create_auth_token(Context, Method) ->
    JObj = cb_context:doc(Context),
    case wh_json:is_empty(JObj) of
        'true' ->
            lager:debug("empty doc, no auth token created"),
            ?MODULE:response('error', <<"invalid credentials">>, 401, Context);
        'false' ->
            Data = cb_context:req_data(Context),

            AccountId = wh_json:get_value(<<"account_id">>, JObj),
            OwnerId = wh_json:get_value(<<"owner_id">>, JObj),

            Token = props:filter_undefined(
                      [{<<"account_id">>, AccountId}
                       ,{<<"owner_id">>, OwnerId}
                       ,{<<"as">>, wh_json:get_value(<<"as">>, Data)}
                       ,{<<"api_key">>, wh_json:get_value(<<"api_key">>, Data)}
                       ,{<<"restrictions">>, wh_json:get_value(<<"restrictions">>, Data)}
                       ,{<<"method">>, wh_util:to_binary(Method)}
                      ]),
            JObjToken = wh_doc:update_pvt_parameters(wh_json:from_list(Token)
                                                     ,wh_util:format_account_id(AccountId, 'encoded')
                                                     ,Token
                                                    ),

            case couch_mgr:save_doc(?TOKEN_DB, JObjToken) of
                {'ok', Doc} ->
                    AuthToken = wh_json:get_value(<<"_id">>, Doc),
                    lager:debug("created new local auth token ~s", [AuthToken]),
                    ?MODULE:response(?MODULE:response_auth(JObj, AccountId, OwnerId)
                                     ,cb_context:setters(
                                        Context
                                        ,[{fun cb_context:set_auth_token/2, AuthToken}
                                          ,{fun cb_context:set_auth_doc/2, Doc}
                                         ])
                                    );
                {'error', R} ->
                    lager:debug("could not create new local auth token, ~p", [R]),
                    cb_context:add_system_error('invalid_credentials', Context)
            end
    end.

-spec generate_year_month_sequence(year_month_tuple(), year_month_tuple(), wh_proplist()) ->
                                          wh_proplist().
generate_year_month_sequence(From, To) ->
    generate_year_month_sequence(From, To, []).

generate_year_month_sequence({Year, Month}, {Year, Month}, Range) ->
    lists:reverse([{Year, Month} | Range]);
generate_year_month_sequence({FromYear, 13}, {ToYear, ToMonth}, Range) ->
    generate_year_month_sequence({FromYear+1, 1}
                                 ,{ToYear, ToMonth}
                                 ,Range
                                );
generate_year_month_sequence({FromYear, FromMonth}, {ToYear, ToMonth}, Range) ->
    'true' = (FromYear * 12 + FromMonth) =< (ToYear * 12 + ToMonth),
    generate_year_month_sequence({FromYear, FromMonth+1}
                                 ,{ToYear, ToMonth}
                                 ,[{FromYear, FromMonth} | Range]
                                ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

year_month_sequence_test() ->
    ?assertEqual([{2013, 11}, {2013, 12}, {2014, 1}]
                 ,generate_year_month_sequence({2013, 11}
                                               ,{2014, 1}
                                              )).

get_path_test() ->
    RawPath = <<"/v1/accounts/acct_id/module">>,
    Relative = <<"../other_mod">>,
    ?assertEqual(get_path1(RawPath, Relative), <<"/v1/accounts/acct_id/other_mod">>),
    ?assertEqual(get_path1(RawPath, <<Relative/binary, "/mod_id">>), <<"/v1/accounts/acct_id/other_mod/mod_id">>).

-endif.
