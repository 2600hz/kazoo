%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_bookkeeper).

-export([sync/1]).
-export([maybe_update/1]).

-include("services.hrl").

-define(STATUS_PENDING, <<"pending">>).
-define(STATUS_COMPLETED, <<"completed">>).
-define(STATUS_FAILED, <<"failed">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sync(kz_term:ne_binary()) -> 'ok'.
sync(AccountId) ->
    FetchOptions = ['hydrate_account_quantities'
                   ,'hydrate_cascade_quantities'
                   ,'hydrate_plans'
                   ,'hydrate_invoices'
                   ],
    Services = kz_services:fetch(AccountId, FetchOptions),
    Invoices = kz_services:invoices(Services),
    _ = kz_services_invoices:foldl(invoices_foldl_fun(Services)
                                  ,[]
                                  ,Invoices
                                  ),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update(kz_services:services()) -> kz_services:services().
maybe_update(Services) ->
    case kz_services_invoices:has_changes(Services) of
        'false' ->
            lager:debug("no changes to any invoices", []),
            Services;
        'true' ->
            Invoices = kz_services_invoices:changed(Services),
            _BookkeeperResults =
                kz_services_invoices:foldl(invoices_foldl_fun(Services)
                                          ,[]
                                          ,Invoices
                                          ),
            Services
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type invoices_acc() :: [{kz_json:object(), kz_amqp_worker:request_return()}].
-spec invoices_foldl_fun(kz_services:services()) ->
                                fun((kz_json:object(), invoices_acc()) -> invoices_acc()).
invoices_foldl_fun(Services) ->
    fun(Invoice, Results) ->
            Type = kz_services_invoice:bookkeeper_type(Invoice),
            AuditJObj = maybe_store_audit_log(Services, Invoice),
            Result =
                case kzd_services:default_bookkeeper_type() =:= Type of
                    'false' -> update_bookkeeper(Type, Invoice, Services, AuditJObj);
                    'true' ->
                        [{<<"status">>, ?STATUS_COMPLETED}
                        ,{[<<"bookkeeper">>, <<"results">>], []}
                        ]
                end,
            FinalAuditJObj = update_audit_log(Services, AuditJObj, Result),
            _ = notify_reseller(Services, Invoice, FinalAuditJObj),
            [{Invoice, Result} | Results]
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_store_audit_log(kz_services:services(), kz_services_invoice:invoice()) ->
                                   kz_term:api_object().
maybe_store_audit_log(Services, Invoice) ->
    case kz_services:account_id(Services) =:= master_account_id()
        andalso (not ?KZ_SERVICE_STORE_MASTER_AUDIT)
    of
        'true' -> 'undefined';
        'false' -> store_audit_log(Services, Invoice)
    end.

-spec master_account_id() -> kz_term:api_binary().
master_account_id() ->
    case kapps_util:get_master_account_id() of
        {'ok', MasterAccountId} -> MasterAccountId;
        _Else -> 'undefined'
    end.

-spec store_audit_log(kz_services:services(), kz_services_invoice:invoice()) ->
                             kz_term:api_object().
store_audit_log(Services, Invoice) ->
    AccountId = kz_services:account_id(Services),
    AuditJObj = kz_services:audit_log(Services),
    InvoiceJObj = kz_services_invoice:public_json(Invoice),
    Props = [{<<"audit">>, AuditJObj}
            ,{<<"invoice">>, InvoiceJObj}
            ,{<<"status">>, ?STATUS_PENDING}
            ,{[<<"bookkeeper">>, <<"id">>], kz_services_invoice:bookkeeper_id(Invoice)}
            ,{[<<"bookkeeper">>, <<"type">>], kz_services_invoice:bookkeeper_type(Invoice)}
            ,{[<<"bookkeeper">>, <<"vendor_id">>], kz_services_invoice:bookkeeper_vendor_id(Invoice)}
            ],
    JObj = kz_doc:update_pvt_parameters(kz_json:set_values(Props, kz_json:new())
                                       ,kz_util:format_account_db(AccountId)
                                       ,[{'account_id', AccountId}
                                        ,{'crossbar_doc_vsn', 2}
                                        ,{'id', kazoo_modb_util:modb_id()}
                                        ,{'type', <<"audit_log">>}
                                        ]
                                       ),
    case kazoo_modb:save_doc(AccountId, JObj) of
        {'ok', AuditDbJObj} -> AuditDbJObj;
        {'error', _R} ->
            lager:debug("failed to store audit log: ~p", [_R]),
            'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_audit_log(kz_services:services(), kz_term:api_object(), kz_amqp_worker:request_return() | kz_term:proplist()) ->
                              kz_term:api_object().
update_audit_log(_Services, 'undefined', _Result) -> 'undefined';
update_audit_log(Services, AuditJObj, {'error', 'timeout'}) ->
    Props = [{<<"status">>, ?STATUS_FAILED}
            ,{<<"reason">>, <<"timeout">>}
            ,{<<"message">>, <<"bookkeeper did not respond to update request">>}
            ],
    update_audit_log(Services, AuditJObj, Props);
update_audit_log(Services, AuditJObj, {'ok', Result}) ->
    Props = [{<<"status">>, final_status(Result)}
            ,{<<"reason">>, kz_json:get_value(<<"Reason">>, Result)}
            ,{<<"message">>, kz_json:get_value(<<"Message">>, Result)}
            ,{[<<"bookkeeper">>, <<"results">>]
             ,kz_json:get_ne_value(<<"Details">>, Result, [])
             }
            ],
    update_audit_log(Services, AuditJObj, Props);
update_audit_log(Services, AuditLog, Props) ->
    AccountId = kz_services:account_id(Services),
    case kazoo_modb:save_doc(AccountId, kz_json:set_values(Props, AuditLog)) of
        {'ok', FinalAuditJObj} -> FinalAuditJObj;
        {'error', _R} ->
            lager:debug("failed to update audit log: ~p", [_R]),
            'undefined'
    end.

-spec final_status(kz_json:object()) -> kz_term:ne_binary().
final_status(JObj) ->
    case kz_json:get_ne_value(<<"Status">>, JObj) =:= <<"success">> of
        'true' -> ?STATUS_COMPLETED;
        'false' -> ?STATUS_FAILED
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec notify_reseller(kz_services:services(), kz_services_invoice:invoice(), kz_term:api_object()) -> 'ok'.
notify_reseller(Services, Invoice, 'undefined') ->
    AuditJObj = kz_json:from_list([{<<"audit">>, kz_services:audit_log(Services)}]),
    notify_reseller(Services, Invoice, AuditJObj);
notify_reseller(Services, Invoice, AuditJObj) ->
    Props = [{<<"Account-ID">>, kz_services:account_id(Services)}
            ,{<<"Audit-Log">>, AuditJObj}
            ,{<<"Items">>
             ,kz_services_items:public_json(
                kz_services_invoice:items(Invoice)
               )
             }
            ,{<<"Timestamp">>, kz_time:now_s()}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    kapps_notify_publisher:cast(Props, fun kapi_notifications:publish_service_added/1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_bookkeeper(kz_term:ne_binary(), kz_services_invoice:invoice(), kz_services:services(), kz_term:api_object()) ->
                               kz_amqp_worker:request_return().
update_bookkeeper(Type, Invoice, Services, 'undefined') ->
    AuditJObj = kz_json:from_list([{<<"audit">>, kz_services:audit_log(Services)}]),
    update_bookkeeper(Type, Invoice, Services, AuditJObj);
update_bookkeeper(_Type, Invoice, Services, AuditJObj) ->
    Request = [{<<"Account-ID">>, kz_services:account_id(Services)}
              ,{<<"Bookkeeper-ID">>, kz_services_invoice:bookkeeper_id(Invoice)}
              ,{<<"Bookkeeper-Type">>, kz_services_invoice:bookkeeper_type(Invoice)}
              ,{<<"Vendor-ID">>, kz_services_invoice:bookkeeper_vendor_id(Invoice)}
              ,{<<"Invoice">>, kz_json:delete_key(<<"plan">>, kz_services_invoice:public_json(Invoice))}
              ,{<<"Call-ID">>, kz_log:get_callid()}
              ,{<<"Audit-Log">>, kz_doc:public_fields(AuditJObj)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    kz_amqp_worker:call(Request
                       ,fun kapi_bookkeepers:publish_update_req/1
                       ,fun kapi_bookkeepers:update_resp_v/1
                       ,20 * ?MILLISECONDS_IN_SECOND
                       ).
