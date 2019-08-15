%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_services).

-export([maybe_dry_run/2
        ,maybe_dry_run/3
        ]).
-export([update_subscriptions/2
        ,update_subscriptions/3
        ]).
-export([reconcile/1]).
-export([audit_log/1]).
-export([audit_log_auth/1
        ,audit_log_agent/1
        ,audit_log_request/1
        ]).
-export([transaction_to_error/2]).

-include("crossbar.hrl").

-type billables() :: kz_services_quantities:billables() |
                     kz_services_quantities:billable().

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_dry_run(cb_context:context(), billables()) -> cb_context:context().
maybe_dry_run(Context, ProposedJObj) ->
    CurrentJObj = cb_context:fetch(Context, 'db_doc'),
    maybe_dry_run(Context, CurrentJObj, ProposedJObj).

-spec maybe_dry_run(cb_context:context(), billables(), billables()) -> cb_context:context().
maybe_dry_run(Context, CurrentJObj, ProposedJObj) ->
    AccountId = cb_context:account_id(Context),
    lager:debug("verifying billing services requirements for account ~s"
               ,[AccountId]
               ),
    AuthAccountId = cb_context:auth_account_id(Context),
    Services = kz_services:fetch(AuthAccountId),
    Updated = kz_services:set_updates(Services
                                     ,AccountId
                                     ,kz_services:to_billables(CurrentJObj)
                                     ,kz_services:to_billables(ProposedJObj)
                                     ),
    Quotes = kz_services_invoices:create(Updated),
    HasAdditions = kz_services_invoices:has_billable_additions(Quotes),
    case should_dry_run(Context) of
        'true' -> dry_run(Context, Quotes, HasAdditions);
        'false' -> check_creditably(Context, Services, Quotes, HasAdditions)
    end.

-spec dry_run(cb_context:context(), kz_services_invoices:invoices(), boolean()) ->
                     cb_context:context().
dry_run(Context, _Quotes, 'false') ->
    lager:debug("request has no billable additions, allowing"),
    Context;
dry_run(Context, Quotes, 'true') ->
    lager:debug("request has not accepted notice of billable additions, rejecting"),
    JObj = kz_services_invoices:public_json(Quotes),
    crossbar_util:response_402(JObj, Context).

-spec should_dry_run(cb_context:context()) -> boolean().
should_dry_run(Context) ->
    cb_context:accepting_charges(Context) =/= 'true'
        andalso cb_context:api_version(Context) =/= ?VERSION_1.

-spec check_creditably(cb_context:context(), kz_services:services(), kz_services_invoices:invoices(), boolean() | number()) ->
                              cb_context:context().
check_creditably(Context, _Services, _Quotes, 'false') ->
    lager:debug("request has no billable additions, skipping standing check"),
    Context;
check_creditably(Context, Services, Quotes, 'true') ->
    lager:debug("request has billable additions, verifying account standing"),
    Key = [<<"difference">>, <<"billable">>],
    Additions = [begin
                     Changes = kz_services_item:changes(Item),
                     BillableQuantity = kz_json:get_integer_value(Key, Changes, 0),
                     Rate = kz_services_item:rate(Item),
                     BillableQuantity * Rate
                 end
                 || Invoice <- kz_services_invoices:billable_additions(Quotes),
                    Item <- kz_services_invoice:items(Invoice),
                    kz_services_item:has_billable_additions(Item)
                ],
    Activations = [kz_services_activation_items:calculate_total(
                     kz_services_invoice:activation_charges(Invoice)
                    )
                   || Invoice <- kz_services_invoices:billable_additions(Quotes)
                  ],
    check_creditably(Context, Services, Quotes, lists:sum(Additions ++ Activations));
check_creditably(Context, _Services, _Quotes, Amount) when Amount =< 0 ->
    Context;
check_creditably(Context, Services, Quotes, Amount) ->
    Options = #{amount => kz_currency:dollars_to_units(Amount)
               ,quotes => Quotes
               },
    case kz_services_standing:acceptable(Services, Options) of
        {'true', _} -> Context;
        {'false', Reason} ->
            ErrorJObj = kz_json:from_map(Reason),
            lager:debug("request denied for billing reasons: ~p", [ErrorJObj]),
            cb_context:add_system_error(402, 'billing_issue', ErrorJObj, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_subscriptions(cb_context:context()
                          ,kz_services_quantities:billables() | kz_services_quantities:billable()
                          ) -> 'ok'.
update_subscriptions(Context, ProposedJObj) ->
    CurrentJObj = cb_context:fetch(Context, 'db_doc'),
    update_subscriptions(Context, CurrentJObj, ProposedJObj).

-spec update_subscriptions(cb_context:context()
                          ,kz_services_quantities:billables() | kz_services_quantities:billable()
                          ,kz_services_quantities:billables() | kz_services_quantities:billable()
                          ) -> 'ok'.
update_subscriptions(Context, CurrentJObj, ProposedJObj) ->
    update_subscriptions(Context, CurrentJObj, ProposedJObj, cb_context:account_id(Context)).

-spec update_subscriptions(cb_context:context()
                          ,kz_services_quantities:billables() | kz_services_quantities:billable()
                          ,kz_services_quantities:billables() | kz_services_quantities:billable()
                          ,kz_term:api_ne_binary()
                          ) -> 'ok'.
update_subscriptions(_Context, _CurrentJObj, _ProposedJObj, 'undefined') ->
    lager:debug("not updating subscriptions on non-account-related change");
update_subscriptions(Context, CurrentJObj, ProposedJObj, AccountId) ->
    AuditLog = audit_log(Context),
    lager:info("committing updates to ~s", [AccountId]),
    _ = kz_services:commit_updates(AccountId
                                  ,CurrentJObj
                                  ,ProposedJObj
                                  ,AuditLog
                                  ),
    'ok'.

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

-spec audit_log_agent(cb_context:context()) -> kz_term:api_proplist().
audit_log_agent(Context) ->
    case cb_context:auth_user_id(Context) of
        'undefined' -> 'undefined';
        UserId -> audit_log_user(Context, UserId)
    end.

-spec audit_log_user(cb_context:context(), kz_term:ne_binary()) -> kz_term:api_proplist().
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
            ,{<<"first_name">>, kzd_users:first_name(JObj)}
            ,{<<"last_name">>, kzd_users:last_name(JObj)}
            ,{<<"full_name">>, kzd_users:full_name(JObj, kzd_users:username(JObj, kzd_users:email(JObj)))}
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
