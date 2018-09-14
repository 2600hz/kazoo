%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_services).

-export([fetch/1]).
-export([maybe_dry_run/2
        ,maybe_dry_run/3
        ]).
-export([update_subscriptions/2
        ,update_subscriptions/3
        ]).
-export([reconcile/1]).
-export([audit_log/1]).
-export([transaction_to_error/2]).

-include("crossbar.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch(cb_context:context()) -> kz_services:services().
fetch(Context) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    AccountId = cb_context:account_id(Context),
    FetchOptions = [],
    case kz_services_reseller:is_reseller(AuthAccountId) of
        'true' -> kz_services:fetch(AuthAccountId, FetchOptions);
        'false' -> kz_services:fetch(AccountId, FetchOptions)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_dry_run(cb_context:context(), kz_services_invoices:jobjs()) -> cb_context:context().
maybe_dry_run(Context, ProposedJObj) ->
    CurrentJObj = cb_context:fetch(Context, 'db_doc'),
    maybe_dry_run(Context, CurrentJObj, ProposedJObj).

-spec maybe_dry_run(cb_context:context(), kz_services_invoices:jobjs(), kz_services_invoices:jobjs()) -> cb_context:context().
maybe_dry_run(Context, CurrentJObj, ProposedJObj) ->
    case should_dry_run(Context) of
        'true' -> dry_run(Context, CurrentJObj, ProposedJObj);
        'false' -> Context
    end.

-spec dry_run(cb_context:context(), kz_services_invoices:jobjs(), kz_services_invoices:jobjs()) -> cb_context:context().
dry_run(Context, CurrentJObj, ProposedJObj) ->
    AccountId = cb_context:account_id(Context),
    Services = kz_services:set_updates(fetch(Context)
                                      ,AccountId
                                      ,CurrentJObj
                                      ,ProposedJObj
                                      ),
    Quotes = kz_services_invoices:create(Services),
    case kz_services_invoices:has_changes(Quotes) of
        'true' ->
            JObj = kz_services_invoices:public_json(Quotes),
            crossbar_util:response_402(JObj, Context);
        'false' -> Context
    end.

-spec should_dry_run(cb_context:context()) -> boolean().
should_dry_run(Context) ->
    cb_context:accepting_charges(Context) =/= 'true'
        andalso cb_context:api_version(Context) =/= ?VERSION_1.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_subscriptions(cb_context:context(), kz_services_invoices:jobjs()) -> 'ok'.
update_subscriptions(Context, ProposedJObj) ->
    CurrentJObj = cb_context:fetch(Context, 'db_doc'),
    update_subscriptions(Context, CurrentJObj, ProposedJObj).

-spec update_subscriptions(cb_context:context(), kz_services_invoices:jobjs(), kz_services_invoices:jobjs()) -> 'ok'.
update_subscriptions(Context, CurrentJObj, ProposedJObj) ->
    update_subscriptions(Context, CurrentJObj, ProposedJObj, cb_context:account_id(Context)).

update_subscriptions(_Context, _CurrentJObj, _ProposedJObj, 'undefined') ->
    lager:debug("not updating subscriptions on non-account-related change");
update_subscriptions(Context, CurrentJObj, ProposedJObj, AccountId) ->
    AuditLog = audit_log(Context),
    lager:info("committing updates to ~s", [AccountId]),
    kz_services:commit_updates(AccountId, CurrentJObj, ProposedJObj, AuditLog).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reconcile(kz_term:ne_binary() | cb_context:context()) -> kz_services:services().
reconcile(Account=?NE_BINARY) ->
    kz_services:reconcile(Account);
reconcile(Context) ->
    AccountId = cb_context:account_id(Context),
    AuditLog = audit_log(Context),
    kz_services:reconcile(AccountId, AuditLog).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec audit_log(cb_context:context()) -> kz_json:object().
audit_log(Context) ->
    kz_json:from_list_recursive(
      [{<<"crossbar_request">>, 'true'}
      ,{<<"authentication">>, audit_log_auth(Context)}
      ,{<<"agent">>, audit_log_agent(Context)}
      ,{<<"request">>, audit_log_request(Context)}
      ]
     ).

-spec audit_log_request(cb_context:context()) -> kz_term:proplist().
audit_log_request(Context) ->
    [{<<"id">>, cb_context:req_id(Context)}
    ,{<<"client_ip">>, cb_context:client_ip(Context)}
    ,{<<"method">>, cb_context:req_verb(Context)}
    ,{<<"path">>, cb_context:raw_path(Context)}
    ].

-spec audit_log_agent(cb_context:context()) -> kz_term:proplist().
audit_log_agent(Context) ->
    case cb_context:auth_user_id(Context) of
        'undefined' -> 'undefined';
        UserId -> audit_log_user(Context, UserId)
    end.

-spec audit_log_user(cb_context:context(), kz_term:ne_binary()) -> kz_term:proplist().
audit_log_user(Context, UserId) ->
    AccountDb = kz_util:format_account_db(
                  cb_context:auth_account_id(Context)
                 ),
    case kz_datamgr:open_cache_doc(AccountDb, UserId) of
        {'error', _} -> 'undefined';
        {'ok', JObj} ->
            [{<<"type">>, <<"user">>}
            ,{<<"type_id">>, kz_doc:id(JObj)}
            ,{<<"account_id">>, cb_context:auth_account_id(Context)}
            ,{<<"first_name">>, kzd_user:first_name(JObj)}
            ,{<<"last_name">>, kzd_user:last_name(JObj)}
            ]
    end.

-spec audit_log_auth(cb_context:context()) -> kz_term:proplist().
audit_log_auth(Context) ->
    [{<<"type">>, cb_context:auth_token_type(Context)}
    ,{<<"account_id">>, cb_context:auth_account_id(Context)}
    ,{<<"account_name">>, kzd_accounts:name(cb_context:auth_account_doc(Context))}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec transaction_to_error(cb_context:context(), kz_transaction:transaction()) -> cb_context:context().
transaction_to_error(Context, Transaction) ->
    case kz_transaction:status_completed(Transaction) of
        'true' ->
            crossbar_util:response(kz_transaction:public_json(Transaction), Context);
        'false' ->
            BookkeeperResults = kz_transaction:bookkeeper_results(Transaction),
            Reason = kz_json:get_ne_binary_value(<<"reason">>, BookkeeperResults),
            transaction_to_error(Context, Transaction, Reason)
    end.

-spec transaction_to_error(cb_context:context(), kz_transaction:transaction(), kz_term:api_ne_binary()) -> cb_context:context().
transaction_to_error(Context, Transaction, 'undefined') ->
    cb_context:add_system_error(500, 'unspecified_fault', kz_transaction:public_json(Transaction), Context);
transaction_to_error(Context, _Transaction, <<"no_payment_token">>) ->
    cb_context:add_system_error('no_payment_token', Context);
transaction_to_error(Context, Transaction, Reason) ->
    cb_context:add_system_error(500, Reason, kz_transaction:public_json(Transaction), Context).
