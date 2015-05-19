%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%% Token auth module
%%%
%%% This is a simple auth mechanism, once the user has aquired an
%%% auth token this module will allow access.  This module should be
%%% updated to be FAR more robust.
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_token_auth).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,validate/1
         ,delete/1
         ,authenticate/1
         ,authorize/1
         ,finish_request/1
         ,clean_expired/0, clean_expired/1
        ]).

-include("../crossbar.hrl").

-define(LOOP_TIMEOUT
        ,whapps_config:get_integer(?APP_NAME, <<"token_auth_expiry">>, ?SECONDS_IN_HOUR)
       ).
-define(PERCENT_OF_TIMEOUT
        ,whapps_config:get_integer(?APP_NAME, <<"expiry_percentage">>, 75)
       ).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    couch_mgr:db_create(?KZ_TOKEN_DB),

    _ = couch_mgr:revise_doc_from_file(?KZ_TOKEN_DB, 'crossbar', "views/token_auth.json"),

    crossbar_bindings:bind(crossbar_cleanup:binding_hour(), ?MODULE, 'clean_expired'),

    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.token_auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.token_auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.token_auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.token_auth">>, ?MODULE, 'delete'),

    crossbar_bindings:bind(<<"*.finish_request.*.*">>, ?MODULE, 'finish_request').

-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_DELETE].

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    case cb_context:auth_doc(Context) of
        'undefined' -> Context;
        AuthDoc ->
            cb_context:setters(Context
                               ,[{fun cb_context:set_resp_status/2, 'success'}
                                 ,{fun cb_context:set_doc/2, AuthDoc}
                                ])
    end.

-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    AuthToken = cb_context:auth_token(Context),
    case couch_mgr:del_doc(?KZ_TOKEN_DB, AuthToken) of
        {'ok', _} ->
            cb_context:setters(Context
                               ,[{fun cb_context:set_resp_status/2, 'success'}
                                 ,{fun cb_context:set_resp_data/2, 'undefined'}
                                 ,{fun cb_context:set_doc/2, 'undefined'}
                                 ,{fun cb_context:set_auth_doc/2, 'undefined'}
                                 ,{fun cb_context:set_auth_token/2, 'undefined'}
                                 ,{fun cb_context:set_auth_account_id/2, 'undefined'}
                                ]);
        {'error', _E} ->
            lager:debug("failed to delete auth token ~s: ~p", [AuthToken, _E]),
            Context
    end.

-spec finish_request(cb_context:context()) -> 'ok'.
-spec finish_request(cb_context:context(), api_object()) -> 'ok'.
finish_request(Context) ->
    finish_request(Context, cb_context:auth_doc(Context)).

finish_request(_Context, 'undefined') -> 'ok';
finish_request(Context, AuthDoc) ->
    cb_context:put_reqid(Context),
    maybe_save_auth_doc(AuthDoc).

-spec maybe_save_auth_doc(wh_json:object()) -> any().
maybe_save_auth_doc(OldAuthDoc) ->
    OldAuthModified = wh_json:get_integer_value(<<"pvt_modified">>, OldAuthDoc),
    Now = wh_util:current_tstamp(),

    ToSaveTimeout = (?LOOP_TIMEOUT * ?PERCENT_OF_TIMEOUT) div 100,

    TimeLeft = Now - (OldAuthModified + ToSaveTimeout),

    case TimeLeft > 0 of
        'true' ->
            lager:debug("auth doc is past time (~ps after) to be saved, saving", [TimeLeft]),
            couch_mgr:ensure_saved(?KZ_TOKEN_DB
                                   ,wh_json:set_value(<<"pvt_modified">>, Now, OldAuthDoc)
                                  );
        'false' ->
            lager:debug("auth doc is too new (~ps to go), not saving", [TimeLeft*-1])
    end.

-spec clean_expired() -> 'ok'.
-spec clean_expired(gregorian_seconds()) -> 'ok'.
clean_expired() ->
    clean_expired(wh_util:current_tstamp() - ?LOOP_TIMEOUT).

clean_expired(CreatedBefore) ->
    ViewOpts = [{'startkey', 0}
                ,{'endkey', CreatedBefore}
                ,{'limit', couch_util:max_bulk_insert()}
               ],

    case couch_mgr:get_results(?KZ_TOKEN_DB, <<"token_auth/listing_by_mtime">>, ViewOpts) of
        {'error', _E} -> lager:debug("failed to lookup expired tokens: ~p", [_E]);
        {'ok', []} -> lager:debug("no expired tokens found");
        {'ok', L} ->
            lager:debug("removing ~b expired tokens", [length(L)]),

            couch_mgr:suppress_change_notice(),
            _ = couch_mgr:del_docs(?KZ_TOKEN_DB, L),
            couch_mgr:enable_change_notice(),

            lager:debug("removed tokens")
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) ->
                          'false' |
                          {'true' | 'halt', cb_context:context()}.
authenticate(Context) ->
    _ = cb_context:put_reqid(Context),
    case kz_buckets:consume_tokens(?APP_NAME
                                   ,cb_modules_util:bucket_name(Context)
                                   ,cb_modules_util:token_cost(Context)
                                  )
    of
        'true' ->
            check_auth_token(Context
                             ,cb_context:auth_token(Context)
                             ,cb_context:magic_pathed(Context)
                            );
        'false' ->
            lager:warning("rate limiting threshold hit for ~s!", [cb_context:client_ip(Context)]),
            {'halt', cb_context:add_system_error('too_many_requests', Context)}
    end.

-spec check_auth_token(cb_context:context(), api_binary(), boolean()) ->
                              boolean() |
                              {'true', cb_context:context()}.
check_auth_token(_Context, <<>>, MagicPathed) -> MagicPathed;
check_auth_token(_Context, 'undefined', MagicPathed) -> MagicPathed;
check_auth_token(Context, AuthToken, _MagicPathed) ->
    lager:debug("checking auth token: '~s'", [AuthToken]),
    case couch_mgr:open_cache_doc(?KZ_TOKEN_DB, AuthToken) of
        {'ok', JObj} -> is_expired(Context, JObj);
        {'error', R} ->
            lager:debug("failed to authenticate token auth, ~p", [R]),
            'false'
    end.

-spec is_expired(cb_context:context(), wh_json:object()) -> boolean() | {'halt', cb_context:context()}.
is_expired(Context, JObj) ->
    AccountId = wh_json:get_value(<<"account_id">>, JObj),
    case wh_util:is_account_expired(AccountId) of
        'false' -> check_as(Context, JObj);
        'true' ->
            _ = spawn(fun() -> maybe_disable_account(AccountId) end),
            Cause = wh_json:from_list([{<<"cause">>, <<"account expired">>}]),
            {'halt', cb_context:add_system_error('forbidden', Cause, Context)}
    end.

-spec maybe_disable_account(ne_binary()) -> 'ok'.
maybe_disable_account(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {'ok', JObj} ->
            case wh_json:get_value(<<"pvt_enabled">>, JObj, 'false') of
                'false' -> 'ok';
                _ -> disable_account(AccountId)
            end;
        {'error', _R} -> disable_account(AccountId)
    end.

-spec disable_account(ne_binary()) -> 'ok'.
disable_account(AccountId) ->
    case crossbar_maintenance:disable_account(AccountId) of
        'ok' -> lager:info("account ~s disabled because expired", [AccountId]);
        'failed' -> lager:error("falied to disable account ~s", [AccountId])
    end.
%%
%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) ->
                          'false' |
                          {'true' | 'halt', cb_context:context()}.
authorize(Context) ->
    case maybe_check_restrictions(Context) of
        'true' -> 
            lager:dbug("authorizing the request"),
            {'true', Context};
        'false' -> 
            Cause = wh_json:from_list([{<<"cause">>, <<"URI restricted">>}]),
            {'halt', cb_context:add_system_error('forbidden', Cause, Context)}
    end.

-spec maybe_check_restrictions(cb_context:context()) -> boolean().
maybe_check_restrictions(Context) ->
    AuthDoc = cb_context:auth_doc(Context),
    case wh_json:get_ne_value(<<"restrictions">>, AuthDoc) of
        'undefined' ->
            lager:debug("no restrictions"),
            'true';
        Rs ->
            check_restrictions(Context, Rs)
    end.

-spec check_restrictions(cb_context:context(), wh_json:object()) -> boolean().
check_restrictions(Context, Rs) ->
    MatchFuns = [fun match_endpoint/2
                ,fun match_account/2
                ,fun match_param/2
                ,fun match_verb/2
                ],
    R1 = lists:foldl(fun(F, R) -> 
                             F(Context, R) 
                     end, 
                     Rs,
                     MatchFuns
                    ),
    lager:debug("result matching restriction rules: ~p",[R1]),
    lists:member('true', R1).

-spec match_endpoint(cb_context:context(), wh_json:object() | 'undefined') -> wh_json:object() | 'undefined'.
match_endpoint(_Context, 'undefined') -> 'undefined';
match_endpoint(Context, Rs) -> 
    [{ReqEndpoint, _}|_] = cb_context:req_nouns(Context),
    case wh_json:get_value(ReqEndpoint, Rs) of
        'undefined' -> wh_json:get_value(<<"_">>, Rs);
        R -> R
    end.

-spec match_account(cb_context:context(), wh_json:object() | 'undefined') -> wh_json:object() | 'undefined'.
match_account(_Context, 'undefined') -> 'undefined';
match_account(Context, Rs) -> 
    Accounts = allowed_accounts(Context),
    match_accounts(Accounts, Rs, []).

match_accounts(_Accounts, [], []) -> 'undefined';
match_accounts(_Accounts, [], Acc) -> lists:reverse(Acc);
match_accounts(Accounts, [R|Rs], Acc) ->
    case wh_json:get_value(<<"accounts">>, R) of
        'undefined' -> 
            lager:debug("no \"accounts\" found in rule, match any account"),
            Params = wh_json:get_value(<<"params">>, R),
            match_accounts(Accounts, Rs, [Params| Acc]);
        RsAccounts -> 
            SetsAccounts = sets:from_list(Accounts),
            SetsRsAccounts = sets:from_list(RsAccounts),
            SetsMatch = sets:intersection(SetsAccounts, SetsRsAccounts),
            case sets:to_list(SetsMatch) of
                [] -> match_accounts(Accounts, Rs, Acc);
                Match ->
                    lager:debug("DEBUG found match: ~p", [Match]),
                    Params = wh_json:get_value(<<"params">>, R),
                    match_accounts(Accounts, Rs, Params ++ Acc)
            end
    end.

-spec allowed_accounts(cb_context:context()) -> ne_binaries().
allowed_accounts(Context) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    %% #cb_context.account_id not always contain AccountId, try found it from req_nouns
    AccountId = case cb_context:account_id(Context) of
        'undefined' -> 
            Nouns = cb_context:req_nouns(Context),
            case props:get_value(?WH_ACCOUNTS_DB, Nouns) of
                'undefined' -> 'undefined';
                [] -> 'undefined';
                [A|_] -> A
            end;
        A -> A
    end,
    case AccountId =:= AuthAccountId of
        'true' -> [ AuthAccountId, <<"{AUTH_ACCOUNT_ID}">> ];
        'false' ->
            case wh_util:is_in_account_hierarchy(AuthAccountId, AccountId, 'false') of
                'true' -> [ AccountId, <<"{DESCENDANT_ACCOUNT_ID}">> ];
                'false' -> [ AccountId ]
            end
    end.

-spec match_param(cb_context:context(), wh_json:object() | 'undefined') -> wh_json:object() | 'undefined'.
match_param(_Context, 'undefined') -> 'undefined';
match_param(Context, Rs) -> 
    [{_, ReqParam}|_] = cb_context:req_nouns(Context),
    match_params(ReqParam, Rs, []).

match_params(_ReqParam, [], []) -> 'undefined';
match_params(_ReqParam, [], Acc) -> lists:reverse(Acc);
match_params(ReqParam, [R|Rs], Acc) ->
    Fun = fun(K, V, A) ->
                  Rule = binary:split(K, <<"/">>, [global]),
                  case kazoo_bindings:matches(Rule, ReqParam) of
                      'true' -> [ V | A ];
                      'false' -> A
                  end
          end,
    Result = lists:reverse(wh_json:foldl(Fun, [], R)),
    case wh_util:is_empty(Result) of
        'true' -> match_params(ReqParam, Rs, Acc);
        'false' -> match_params(ReqParam, Rs, Result ++ Acc)
    end.


match_verb(Context, Rs) ->
    Verb = cb_context:req_verb(Context),
    match_verbs(Verb, Rs, []).

match_verbs(_Verb, [], Acc) -> Acc;
match_verbs(Verb, [R|Rs], Acc) ->
    Result = case wh_json:get_value(Verb, R) of
        'undefined' -> wh_json:get_value(<<"_">>, R);
        Res -> Res
    end,
    match_verbs(Verb, Rs, [Result | Acc]).

-spec check_as(cb_context:context(), wh_json:object()) ->
                      boolean() |
                      {'true', cb_context:context()}.
check_as(Context, JObj) ->
    case wh_json:get_value(<<"account_id">>, JObj, 'undefined') of
        'undefined' -> {'true', set_auth_doc(Context, JObj)};
        AccountId -> check_as_payload(Context, JObj, AccountId)
    end.

-spec check_as_payload(cb_context:context(), wh_json:object(), ne_binary()) ->
                              boolean() |
                              {'true', cb_context:context()}.
check_as_payload(Context, JObj, AccountId) ->
    case {wh_json:get_value([<<"as">>, <<"account_id">>], JObj, 'undefined')
          ,wh_json:get_value([<<"as">>, <<"owner_id">>], JObj, 'undefined')
         }
    of
        {'undefined', _} -> {'true', set_auth_doc(Context, JObj)};
        {_, 'undefined'} -> {'true', set_auth_doc(Context, JObj)};
        {AsAccountId, AsOwnerId} -> check_descendants(Context, JObj, AccountId, AsAccountId, AsOwnerId)
    end.

-spec check_descendants(cb_context:context(), wh_json:object()
                        ,ne_binary() ,ne_binary() ,ne_binary()) ->
                               boolean() |
                               {'true', cb_context:context()}.
check_descendants(Context, JObj, AccountId, AsAccountId, AsOwnerId) ->
    case get_descendants(AccountId) of
        {'error', _} -> 'false';
        {'ok', Descendants} ->
            case lists:member(AsAccountId, Descendants) of
                'false' -> 'false';
                'true' ->
                    JObj1 = wh_json:set_values([{<<"account_id">>, AsAccountId}
                                                 ,{<<"owner_id">>, AsOwnerId}
                                               ], JObj),
                    {'true', set_auth_doc(Context, JObj1)}
            end
    end.

-spec get_descendants(ne_binary()) ->
                             {'ok', wh_json:objects()} |
                             {'error', any()}.
get_descendants(AccountId) ->
    case couch_mgr:get_results(<<"accounts">>
                               ,<<"accounts/listing_by_descendants">>
                              ,[{'startkey', [AccountId]}
                                ,{'endkey', [AccountId, wh_json:new()]}
                               ])
    of
        {'error', _}=Error -> Error;
        {'ok', JObjs} ->
            {'ok', [wh_json:get_value(<<"id">>, JObj) || JObj <- JObjs]}
    end.

-spec set_auth_doc(cb_context:context(), wh_json:object()) -> cb_context:context().
set_auth_doc(Context, JObj) ->
    Setters = [{fun cb_context:set_auth_doc/2, JObj}
               ,{fun cb_context:set_auth_account_id/2
                 ,wh_json:get_ne_value(<<"account_id">>, JObj)
                }
              ],
    cb_context:setters(Context, Setters).
