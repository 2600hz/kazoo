%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_bookkeeper).

-export([sync/1]).
-export([maybe_update/1]).

-include("services.hrl").

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
            Result = update_bookkeeper(Type, Invoice, Services),
            _ = store_audit_log(Services, Invoice, Result),
            _ = notify_reseller(Services, Invoice),
            [{Invoice, Result} | Results]
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec store_audit_log(kz_services:services(), kz_services_invoice:invoice(), kz_amqp_worker:request_result()) -> 'ok'.
store_audit_log(Services, Invoice, {'ok', Result}) ->
    Details =
        [kz_json:normalize(JObj)
         || JObj <- kz_json:get_list_value(<<"Details">>, Result, [])
        ],
    store_audit_log_to_db(Services, Invoice, Details);
store_audit_log(Services, Invoice, {'error', 'timeout'}) ->
    Result = kz_json:from_list([{<<"status">>, <<"error">>}
                               ,{<<"reason">>, <<"timeout">>}
                               ,{<<"message">>, <<"bookkeeper did not respond to update request">>}
                               ]
                              ),
    store_audit_log_to_db(Services, Invoice, Result).

-spec store_audit_log_to_db(kz_services:services(), kz_services_invoice:invoice(), kz_json:object() | kz_json:objects()) -> 'ok'.
store_audit_log_to_db(Services, Invoice, Result) ->
    AccountId = kz_services:account_id(Services),
    AuditJObj = kz_services:audit_log(Services),
    InvoiceJObj = kz_services_invoice:public_json(Invoice),
    Props = [{<<"audit">>, AuditJObj}
            ,{<<"invoice">>, InvoiceJObj}
            ,{<<"status">>, kz_json:get_value(<<"status">>, Result)}
            ,{<<"reason">>, kz_json:get_value(<<"reason">>, Result)}
            ,{<<"message">>, kz_json:get_value(<<"message">>, Result)}
            ,{[<<"bookkeeper">>, <<"id">>], kz_services_invoice:bookkeeper_id(Invoice)}
            ,{[<<"bookkeeper">>, <<"type">>], kz_services_invoice:bookkeeper_type(Invoice)}
            ,{[<<"bookkeeper">>, <<"vendor_id">>], kz_services_invoice:bookkeeper_vendor_id(Invoice)}
            ,{[<<"bookkeeper">>, <<"results">>], Result}
            ],
    JObj = kz_doc:update_pvt_parameters(kz_json:set_values(Props, kz_json:new())
                                       ,kz_util:format_account_db(AccountId)
                                       ,[{'account_id', AccountId}
                                        ,{'crossbar_doc_vsn', 2}
                                        ,{'id', kazoo_modb_util:modb_id()}
                                        ,{'type', <<"audit_log">>}
                                        ]
                                       ),
    _ = kazoo_modb:save_doc(AccountId, JObj),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec notify_reseller(kz_services:services(), kz_services_invoice:invoice()) -> 'ok'.
notify_reseller(Services, Invoice) ->
    AuditJObj = kz_services:audit_log(Services),
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
-spec update_bookkeeper(kz_term:ne_binary(), kz_services_invoice:invoice(), kz_services:services()) ->
                               kz_amqp_worker:request_return().
update_bookkeeper(Type, Invoice, Services) ->
    Request = [{<<"Account-ID">>, kz_services:account_id(Services)}
              ,{<<"Bookkeeper-ID">>, kz_services_invoice:bookkeeper_id(Invoice)}
              ,{<<"Bookkeeper-Type">>, kz_services_invoice:bookkeeper_type(Invoice)}
              ,{<<"Vendor-ID">>, kz_services_invoice:bookkeeper_vendor_id(Invoice)}
              ,{<<"Items">>
               ,kz_services_items:public_json(
                  kz_services_invoice:items(Invoice)
                 )
               }
              ,{<<"Call-ID">>, kz_util:get_callid()}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    kz_amqp_worker:call(Request
                       ,fun kapi_bookkeepers:publish_update_req/1
                       ,fun kapi_bookkeepers:update_resp_v/1
                       ).
