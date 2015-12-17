%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(crossbar_services).

-export([maybe_dry_run/2, maybe_dry_run/3
         ,reconcile/1
        ]).

-include("crossbar.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-type callback() :: fun(() -> cb_context:context()).

-spec maybe_dry_run(cb_context:context(), callback()) -> cb_context:context().
-spec maybe_dry_run(cb_context:context(), callback(), ne_binary() | wh_proplist()) ->
                           cb_context:context().
maybe_dry_run(Context, Callback) ->
    Type = wh_doc:type(cb_context:doc(Context)),
    maybe_dry_run(Context, Callback, Type).

maybe_dry_run(Context, Callback, Type) when is_binary(Type) ->
    maybe_dry_run_by_type(Context, Callback, Type, cb_context:accepting_charges(Context));
maybe_dry_run(Context, Callback, Props) ->
    maybe_dry_run_by_props(Context, Callback, Props, cb_context:accepting_charges(Context)).

-spec maybe_dry_run_by_props(cb_context:context(), callback(), wh_proplist(), boolean()) ->
                                    cb_context:context().
maybe_dry_run_by_props(Context, Callback, Props, 'true') ->
    Type = props:get_ne_binary_value(<<"type">>, Props),

    UpdatedServices = calc_service_updates(Context, Type, props:delete(<<"type">>, Props)),
    RespJObj = dry_run(UpdatedServices),
    lager:debug("accepting charges: ~s", [wh_json:encode(RespJObj)]),
    _ = accepting_charges(Context, RespJObj, UpdatedServices),
    Callback();
maybe_dry_run_by_props(Context, Callback, Props, 'false') ->
    Type = props:get_ne_binary_value(<<"type">>, Props),
    UpdatedServices = calc_service_updates(Context, Type, props:delete(<<"type">>, Props)),
    RespJObj = dry_run(UpdatedServices),
    lager:debug("not accepting charges: ~s", [wh_json:encode(RespJObj)]),

    handle_dry_run_resp(Context, Callback, UpdatedServices, RespJObj).

-spec handle_dry_run_resp(cb_context:context(), callback(), wh_services:services(), wh_json:object()) ->
                                 cb_context:context().
handle_dry_run_resp(Context, Callback, Services, RespJObj) ->
    case wh_json:is_empty(RespJObj) of
        'true' ->
            lager:debug("this service update is not a drill people!"),
            save_an_audit_log(Context, Services),
            Callback();
        'false' ->
            lager:debug("this service update is just a test, do not be alarmed"),
            crossbar_util:response_402(RespJObj, Context)
    end.

-spec maybe_dry_run_by_type(cb_context:context(), callback(), ne_binary(), boolean()) ->
                                   cb_context:context().
maybe_dry_run_by_type(Context, Callback, Type, 'true') ->
    UpdatedServices = calc_service_updates(Context, Type),
    RespJObj = dry_run(UpdatedServices),
    lager:debug("accepting charges: ~s", [wh_json:encode(RespJObj)]),
    _ = accepting_charges(Context, RespJObj, UpdatedServices),
    Callback();
maybe_dry_run_by_type(Context, Callback, Type, 'false') ->
    UpdatedServices = calc_service_updates(Context, Type),
    RespJObj = dry_run(UpdatedServices),
    lager:debug("not accepting charges: ~s", [wh_json:encode(RespJObj)]),

    handle_dry_run_resp(Context, Callback, UpdatedServices, RespJObj).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec accepting_charges(cb_context:context(), wh_json:object(), wh_services:services()) -> 'ok' | 'error'.
accepting_charges(Context, JObj, Services) ->
    Items = extract_items(wh_json:delete_key(<<"activation_charges">>, JObj)),
    Transactions =
        lists:foldl(
          fun(Item, Acc) ->
                  create_transactions(Context, Item, Acc)
          end
          ,[]
          ,Items
         ),
    case wh_services:commit_transactions(Services, Transactions) of
        'ok' -> save_an_audit_log(Context, Services);
        'error' -> 'error'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec extract_items(wh_json:object()) -> wh_json:objects().
extract_items(JObj) ->
    wh_json:foldl(fun extract_items_from_category/3
                  ,[]
                  ,JObj
                 ).

-spec extract_items_from_category(wh_json:key(), wh_json:object(), wh_json:objects()) ->
                                         wh_json:objects().
extract_items_from_category(_, CategoryJObj, Acc) ->
    wh_json:foldl(fun extract_item_from_category/3
                  ,Acc
                  ,CategoryJObj
                 ).

-spec extract_item_from_category(wh_json:key(), wh_json:object(), wh_json:objects()) ->
                                         wh_json:objects().
extract_item_from_category(_, ItemJObj, Acc) ->
    [ItemJObj|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_transactions(cb_context:context()
                          ,wh_json:object()
                          ,wh_transaction:transactions()) -> wh_transaction:transactions().
-spec create_transactions(cb_context:context()
                          ,wh_json:object()
                          ,wh_transaction:transactions()
                          ,integer()) -> wh_transaction:transactions().
create_transactions(Context, Item, Acc) ->
    Quantity = wh_json:get_integer_value(<<"quantity">>, Item, 0),
    create_transactions(Context, Item, Acc, Quantity).

create_transactions(_Context, _Item, Acc, 0) -> Acc;
create_transactions(Context, Item, Acc, Quantity) ->
    AccountId = cb_context:account_id(Context),
    Amount = wh_json:get_integer_value(<<"activation_charges">>, Item, 0),
    Units = wht_util:dollars_to_units(Amount),
    Routines = [fun set_meta_data/3
                ,fun set_event/3
               ],
    Transaction =
        lists:foldl(
          fun(F, T) -> F(Context, Item, T) end
          ,wh_transaction:debit(AccountId, Units)
          ,Routines
         ),
    create_transactions(Context, Item, [Transaction|Acc], Quantity-1).

-spec set_meta_data(cb_context:context()
                    ,wh_json:object()
                    ,wh_transaction:transaction()
                   ) -> wh_transaction:transaction().
set_meta_data(Context, Item, Transaction) ->
    MetaData =
        wh_json:from_list(
          [{<<"auth_account_id">>, cb_context:auth_account_id(Context)}
           ,{<<"category">>, wh_json:get_value(<<"category">>, Item)}
           ,{<<"item">>, wh_json:get_value(<<"item">>, Item)}
          ]),
    wh_transaction:set_metadata(MetaData, Transaction).

-spec set_event(cb_context:context() ,wh_json:object()
                ,wh_transaction:transaction()) -> wh_transaction:transaction().
set_event(_Context, Item, Transaction) ->
    ItemValue = wh_json:get_value(<<"item">>, Item, <<>>),
    Event = <<"Activation charges for ", ItemValue/binary>>,
    wh_transaction:set_event(Event, Transaction).

-spec dry_run(wh_services:services() | 'undefined') -> wh_json:object().
dry_run('undefined') -> wh_json:new();
dry_run(Services) ->
    lager:debug("updated services, checking for dry run"),
    wh_services:dry_run(Services).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec calc_service_updates(cb_context:context(), ne_binary()) ->
                                  wh_services:services() | 'undefined'.
-spec calc_service_updates(cb_context:context(), ne_binary(), wh_proplist()) ->
                                  wh_services:services() | 'undefined'.
calc_service_updates(Context, <<"device">>) ->
    DeviceType = kz_device:device_type(cb_context:doc(Context)),
    Services = fetch_service(Context),

    wh_service_devices:reconcile(Services, DeviceType);
calc_service_updates(Context, <<"user">>) ->
    Services = fetch_service(Context),
    JObj = cb_context:doc(Context),
    UserType = wh_json:get_value(<<"priv_level">>, JObj),
    wh_service_users:reconcile(Services, UserType);
calc_service_updates(Context, <<"limits">>) ->
    Services = fetch_service(Context),
    ReqData = cb_context:req_data(Context),
    Updates =
        wh_json:from_list(
          [{<<"twoway_trunks">>, wh_json:get_integer_value(<<"twoway_trunks">>, ReqData, 0)}
           ,{<<"inbound_trunks">>, wh_json:get_integer_value(<<"inbound_trunks">>, ReqData, 0)}
           ,{<<"outbound_trunks">>, wh_json:get_integer_value(<<"outbound_trunks">>, ReqData, 0)}
          ]),
    wh_service_limits:reconcile(Services, Updates);
calc_service_updates(Context, <<"port_request">>) ->
    Services = fetch_service(Context),
    JObj = cb_context:doc(Context),
    Numbers = wh_json:get_value(<<"numbers">>, JObj),
    PhoneNumbers =
        wh_json:foldl(
          fun port_request_foldl/3
          ,wh_json:new()
          ,Numbers
         ),
    wh_service_phone_numbers:reconcile(PhoneNumbers, Services);
calc_service_updates(Context, <<"app">>) ->
    [{<<"apps_store">>, [Id]} | _] = cb_context:req_nouns(Context),
    case wh_service_ui_apps:is_in_use(cb_context:req_data(Context)) of
        'false' -> 'undefined';
        'true' ->
            Services = fetch_service(Context),
            AppName = wh_json:get_value(<<"name">>, cb_context:fetch(Context, Id)),
            wh_service_ui_apps:reconcile(Services, AppName)
    end;
calc_service_updates(Context, <<"ips">>) ->
    Services = fetch_service(Context),
    UpdatedServices = wh_service_ips:reconcile(Services, <<"dedicated">>),
    wh_services:dry_run(UpdatedServices);
calc_service_updates(Context, <<"branding">>) ->
    Services = fetch_service(Context),
    wh_service_whitelabel:reconcile(Services, <<"whitelabel">>);
calc_service_updates(_Context, _Type) ->
    lager:warning("unknown type ~p, cannot calculate service updates", [_Type]),
    'undefined'.

calc_service_updates(Context, <<"ips">>, Props) ->
    Services = fetch_service(Context),
    wh_service_ips:reconcile(Services, Props);
calc_service_updates(_Context, _Type, _Props) ->
    lager:warning("unknown type ~p, cannot execute dry run", [_Type]),
    'undefined'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec port_request_foldl(ne_binary(), wh_json:object(), wh_json:object()) -> wh_json:object().
port_request_foldl(Number, NumberJObj, JObj) ->
    wh_json:set_value(
      Number
      ,wh_json:set_value(
         <<"features">>
         ,[<<"port">>]
         ,NumberJObj
        )
      ,JObj
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_service(cb_context:context()) -> wh_services:services().
fetch_service(Context) ->
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    case wh_services:is_reseller(AuthAccountId) of
        'false' ->
            lager:debug("auth account ~s is not a reseller, loading service account ~s"
                        ,[AuthAccountId, AccountId]
                       ),
            wh_services:fetch(AccountId);
        'true' ->
            lager:debug("auth account ~s is a reseller, loading service from reseller", [AuthAccountId]),
            wh_services:fetch(AuthAccountId)
    end.

-spec reconcile(cb_context:context()) -> cb_context:context().
reconcile(Context) ->
    case cb_context:resp_status(Context) =:= 'success'
        andalso cb_context:req_verb(Context) =/= ?HTTP_GET
    of
        'false' -> Context;
        'true' ->
            lager:debug("maybe reconciling services for account ~s"
                        ,[cb_context:account_id(Context)]
                       ),
            _ = wh_services:save_as_dirty(cb_context:account_id(Context)),
            Context
    end.

-spec base_audit_log(cb_context:context()) -> wh_json:object().
base_audit_log(Context) ->
    AccountJObj = cb_context:account_doc(Context),
    Tree = kz_account:tree(AccountJObj) ++ [cb_context:account_id(Context)],

    lists:foldl(fun base_audit_log_fold/2
                ,kzd_audit_log:new()
                ,[{fun kzd_audit_log:set_tree/2, Tree}
                  ,{fun kzd_audit_log:set_authenticating_user/2, base_auth_user(Context)}
                  ,{fun kzd_audit_log:set_audit_account/3
                    ,cb_context:account_id(Context)
                    ,base_audit_account(Context)
                   }
                 ]
               ).

-type audit_log_fun_2() :: {fun((kzd_audit_log:doc(), Term) -> kzd_audit_log:doc()), Term}.
-type audit_log_fun_3() :: {fun((kzd_audit_log:doc(), Term1, Term2) -> kzd_audit_log:doc()), Term1, Term2}.
-type audit_log_fun() ::  audit_log_fun_2() | audit_log_fun_3().

-spec base_audit_log_fold(audit_log_fun(), kzd_audit_log:doc()) -> kzd_audit_log:doc().
base_audit_log_fold({F, V}, Acc) -> F(Acc, V);
base_audit_log_fold({F, V1, V2}, Acc) -> F(Acc, V1, V2).

-spec base_audit_account(cb_context:context()) -> wh_json:object().
base_audit_account(Context) ->
    AccountName = kz_account:name(cb_context:account_doc(Context)),

    wh_json:from_list(
      props:filter_empty(
        [{<<"account_name">>, AccountName}]
       )).

-spec base_auth_user(cb_context:context()) -> wh_json:object().
base_auth_user(Context) ->
    AuthJObj = cb_context:auth_doc(Context),
    AccountJObj = cb_context:auth_account_doc(Context),

    AccountName = kz_account:name(AccountJObj),
    wh_json:set_value(<<"account_name">>
                      ,AccountName
                      ,leak_auth_pvt_fields(AuthJObj)
                     ).

-spec leak_auth_pvt_fields(wh_json:object()) -> wh_json:object().
leak_auth_pvt_fields(JObj) ->
    wh_json:set_values([{<<"account_id">>, wh_doc:account_id(JObj)}
                        ,{<<"auth_token">>, wh_doc:id(JObj)}
                        ,{<<"created">>, wh_doc:created(JObj)}
                       ]
                       ,wh_json:public_fields(JObj)
                      ).

-spec save_an_audit_log(cb_context:context(), wh_services:services() | 'undefined') -> 'ok'.
save_an_audit_log(_Context, 'undefined') -> 'ok';
save_an_audit_log(Context, Services) ->
    BaseAuditLog = base_audit_log(Context),
    kzd_audit_log:save(Services, BaseAuditLog).
