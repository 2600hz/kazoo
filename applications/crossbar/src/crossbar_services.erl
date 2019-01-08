%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_services).

-export([maybe_dry_run/2, maybe_dry_run/3
        ,reconcile/1
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl"). %% FEATURE_PORT

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type callback() :: fun(() -> cb_context:context()).

-spec maybe_dry_run(cb_context:context(), callback()) -> cb_context:context().
maybe_dry_run(Context, Callback) ->
    Type = kz_doc:type(cb_context:doc(Context)),
    maybe_dry_run(Context, Callback, Type).

-spec maybe_dry_run(cb_context:context(), callback(), kz_term:ne_binary() | kz_term:proplist()) ->
                           cb_context:context().
maybe_dry_run(Context, Callback, Type) when is_binary(Type) ->
    maybe_dry_run(Context, Callback, Type, [], cb_context:accepting_charges(Context));
maybe_dry_run(Context, Callback, Props) ->
    Type = props:get_ne_binary_value(<<"type">>, Props),
    maybe_dry_run(Context, Callback, Type, Props, cb_context:accepting_charges(Context)).

-spec maybe_dry_run(cb_context:context(), callback(), kz_term:ne_binary(), kz_term:proplist(), boolean()) ->
                           cb_context:context().
maybe_dry_run(Context, Callback, Type, Props, 'true') ->
    UpdatedServices = calc_service_updates(Context, Type, Props),
    RespJObj = dry_run(UpdatedServices),
    lager:debug("accepting charges: ~s", [kz_json:encode(RespJObj)]),
    BillingId = kz_services:get_billing_id(cb_context:account_id(Context)),
    Transactions = accepting_charges(Context, RespJObj),
    Amount = lists:sum([kz_transaction:amount(Transaction) || Transaction <- Transactions]),
    case kz_services:check_bookkeeper(BillingId, Amount) of
        'true' ->
            commit_transactions(Context, Transactions, UpdatedServices, Callback);
        'false' ->
            cb_context:add_system_error('no_credit', Context)
    end;
maybe_dry_run(Context, Callback, Type, Props, 'false') ->
    UpdatedServices = calc_service_updates(Context, Type, Props),
    RespJObj = dry_run(UpdatedServices),
    handle_dry_run_resp(Context, Callback, UpdatedServices, RespJObj).


-spec handle_dry_run_resp(cb_context:context(), callback(), kz_services:services(), kz_json:object()) ->
                                 cb_context:context().
handle_dry_run_resp(Context, Callback, Services, RespJObj) ->
    case kz_json:is_empty(RespJObj) of
        'true' ->
            lager:debug("no dry_run charges to accept; this service update is not a drill people!"),
            save_an_audit_log(Context, Services),
            Callback();
        'false' ->
            lager:debug("this update requires service changes to be accepted, do not be alarmed"),
            crossbar_util:response_402(RespJObj, Context)
    end.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec accepting_charges(cb_context:context(), kz_json:object()) -> kz_transaction:transactions().
accepting_charges(Context, JObj) ->
    Items = extract_items(kz_json:delete_key(<<"activation_charges">>, JObj)),
    Fun = fun(Item, Acc) -> create_transactions(Context, Item, Acc) end,
    lists:foldl(Fun, [], Items).

-spec commit_transactions(cb_context:context(), kz_transaction:transactions(), kz_services:services(), callback()) -> cb_context:context().
commit_transactions(Context, Transactions, Services, Callback) ->
    case kz_services:commit_transactions(Services, Transactions) of
        'ok' -> save_an_audit_log(Context, Services),
                Callback();
        'error' -> cb_context:add_system_error('datastore_fault', Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec extract_items(kz_json:object()) -> kz_json:objects().
extract_items(JObj) ->
    kz_json:foldl(fun extract_items_from_category/3, [], JObj).

-spec extract_items_from_category(kz_json:path(), kz_json:object(), kz_json:objects()) ->
                                         kz_json:objects().
extract_items_from_category(CategoryKey, CategoryJObj, Acc) ->
    Fun = fun(K, V, Acc1) -> extract_item_from_category(CategoryKey, K, V, Acc1) end,
    kz_json:foldl(Fun, Acc, CategoryJObj).

-spec extract_item_from_category(kz_json:path(), kz_json:path(), kz_json:object(), kz_json:objects()) ->
                                        kz_json:objects().
extract_item_from_category(CategoryKey, ItemKey, ItemJObj, Acc) ->
    [kz_json:set_values([{<<"category">>, CategoryKey}
                        ,{<<"item">>, ItemKey}
                        ], ItemJObj)|Acc].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_transactions(cb_context:context()
                         ,kz_json:object()
                         ,kz_transaction:transactions()) -> kz_transaction:transactions().
create_transactions(Context, Item, Acc) ->
    Quantity = kz_json:get_integer_value(<<"activate_quantity">>, Item, 0),
    create_transactions(Context, Item, Acc, Quantity).

-spec create_transactions(cb_context:context()
                         ,kz_json:object()
                         ,kz_transaction:transactions()
                         ,integer()) -> kz_transaction:transactions().
create_transactions(_Context, _Item, Acc, 0) -> Acc;
create_transactions(Context, Item, Acc, Quantity) ->
    AccountId = cb_context:account_id(Context),
    Amount = kz_json:get_integer_value(<<"activation_charges">>, Item, 0),
    Units = wht_util:dollars_to_units(Amount * Quantity),
    Routines = [fun set_meta_data/3
               ,fun set_event/3
               ],
    App = fun(F, T) -> F(Context, Item, T) end,
    Debit = kz_transaction:debit(AccountId, Units),
    [lists:foldl(App, Debit, Routines) | Acc].

-spec set_meta_data(cb_context:context(), kz_json:object(), kz_transaction:transaction()) ->
                           kz_transaction:transaction().
set_meta_data(Context, Item, Transaction) ->
    MetaData =
        kz_json:from_list(
          [{<<"auth_account_id">>, cb_context:auth_account_id(Context)}
          ,{<<"category">>, kz_json:get_value(<<"category">>, Item)}
          ,{<<"item">>, kz_json:get_value(<<"item">>, Item)}
          ]),
    kz_transaction:set_metadata(MetaData, Transaction).

-spec set_event(cb_context:context(), kz_json:object(), kz_transaction:transaction()) ->
                       kz_transaction:transaction().
set_event(_Context, Item, Transaction) ->
    ItemValue = kz_json:get_value(<<"item">>, Item, <<>>),
    Event = <<"Activation charges for ", ItemValue/binary>>,
    kz_transaction:set_event(Event, Transaction).

-spec dry_run(kz_services:services() | 'undefined') -> kz_json:object().
dry_run('undefined') -> kz_json:new();
dry_run(Services) ->
    lager:debug("updated services, checking for dry run"),
    kz_services:dry_run(Services).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec calc_service_updates(cb_context:context(), kz_term:ne_binary()) ->
                                  kz_services:services() | 'undefined'.
calc_service_updates(Context, <<"device">>) ->
    DeviceType = kzd_devices:device_type(cb_context:doc(Context)),
    Services = fetch_service(Context),
    kz_service_devices:reconcile(Services, DeviceType);
calc_service_updates(Context, <<"user">>) ->
    Services = fetch_service(Context),
    JObj = cb_context:doc(Context),
    UserType = kzd_user:priv_level(JObj),
    kz_service_users:reconcile(Services, UserType);
calc_service_updates(Context, <<"limits">>) ->
    Services = fetch_service(Context),
    ReqData = cb_context:req_data(Context),
    Updates =
        kz_json:from_list(
          [{<<"twoway_trunks">>, kz_json:get_integer_value(<<"twoway_trunks">>, ReqData, 0)}
          ,{<<"inbound_trunks">>, kz_json:get_integer_value(<<"inbound_trunks">>, ReqData, 0)}
          ,{<<"outbound_trunks">>, kz_json:get_integer_value(<<"outbound_trunks">>, ReqData, 0)}
          ]),
    kz_service_limits:reconcile(Services, Updates);
calc_service_updates(Context, <<"port_request">>) ->
    PortNumbers = kz_json:get_value(<<"numbers">>, cb_context:doc(Context)),
    PhoneNumbers =
        [create_port_number(Num, kz_json:values(Num, PortNumbers))
         || Num <- kz_json:get_keys(PortNumbers)
        ],
    kz_service_phone_numbers:reconcile(fetch_service(Context), PhoneNumbers);
calc_service_updates(Context, <<"app">>) ->
    [{<<"apps_store">>, [Id]} | _] = cb_context:req_nouns(Context),
    case kz_service_ui_apps:is_in_use(cb_context:req_data(Context)) of
        'false' -> 'undefined';
        'true' ->
            Services = fetch_service(Context),
            AppName = kz_json:get_value(<<"name">>, cb_context:fetch(Context, Id)),
            kz_service_ui_apps:reconcile(Services, AppName)
    end;
calc_service_updates(Context, <<"ips">>) ->
    Services = fetch_service(Context),
    kz_service_ips:reconcile(Services, <<"dedicated">>);
calc_service_updates(Context, <<"branding">>) ->
    Services = fetch_service(Context),
    kz_service_whitelabel:reconcile(Services, <<"whitelabel">>);
calc_service_updates(Context, <<"support">>) ->
    Services = fetch_service(Context),
    kz_service_ledgers:reconcile(Services, <<"support">>);
calc_service_updates(Context, <<"conference">>) ->
    JObj = cb_context:doc(Context),
    Services = fetch_service(Context),
    kz_service_conferences:reconcile(Services, JObj);
calc_service_updates(Context, <<"faxbox">>) ->
    JObj = cb_context:doc(Context),
    Services = fetch_service(Context),
    kz_service_faxes:reconcile(Services, JObj);
calc_service_updates(Context, <<"vmbox">>) ->
    JObj = cb_context:doc(Context),
    Services = fetch_service(Context),
    kz_service_voicemails:reconcile(Services, JObj);
calc_service_updates(_Context, _Type) ->
    lager:warning("unknown type ~p, cannot calculate service updates", [_Type]),
    'undefined'.

-spec calc_service_updates(cb_context:context(), kz_term:ne_binary(), kz_term:proplist()) ->
                                  kz_services:services() | 'undefined'.
calc_service_updates(Context, <<"ips">>, Props) ->
    Services = fetch_service(Context),
    kz_service_ips:reconcile(Services, Props);
calc_service_updates(Context, Type, []) ->
    calc_service_updates(Context, Type);
calc_service_updates(_Context, _Type, _Props) ->
    lager:warning("unknown type ~p, cannot execute dry run", [_Type]),
    'undefined'.

-spec create_port_number(kz_term:ne_binary(), kz_term:ne_binaries()) -> knm_phone_number:knm_phone_number().
create_port_number(Number, Features) ->
    JObj = kz_json:from_list([{<<"_id">>, Number}
                             ,{<<"features">>, Features}
                             ]),
    PN = knm_phone_number:from_json_with_options(JObj, []),
    knm_phone_number:set_feature(PN, ?FEATURE_PORT, kz_json:new()).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_service(cb_context:context()) -> kz_services:services().
fetch_service(Context) ->
    kz_services:fetch(cb_context:account_id(Context)).

-spec reconcile(cb_context:context()) -> cb_context:context().
reconcile(Context) ->
    case cb_context:resp_status(Context) =:= 'success'
        andalso cb_context:req_verb(Context) =/= ?HTTP_GET
    of
        'false' -> Context;
        'true' ->
            AccountId = cb_context:account_id(Context),
            lager:debug("maybe reconciling services for account ~s", [AccountId]),
            Services = kz_services:reconcile_only(AccountId),
            _ = kz_services:save_as_dirty(Services),
            Context
    end.

-spec base_audit_log(cb_context:context(), kz_services:services()) -> kz_json:object().
base_audit_log(Context, Services) ->
    AccountId = cb_context:account_id(Context),
    AccountJObj = cb_context:account_doc(Context),
    Tree = kzd_accounts:tree(AccountJObj) ++ [AccountId],

    lists:foldl(fun base_audit_log_fold/2
               ,kzd_audit_log:new()
               ,[{fun kzd_audit_log:set_tree/2, Tree}
                ,{fun kzd_audit_log:set_authenticating_user/2, base_auth_user_info(Context)}
                ,{fun kzd_audit_log:set_audit_account/3
                 ,AccountId
                 ,base_audit_account(Context, Services)
                 }
                ]).

-type audit_log_fun_2() :: {fun((kzd_audit_log:doc(), Term) -> kzd_audit_log:doc()), Term}.
-type audit_log_fun_3() :: {fun((kzd_audit_log:doc(), Term1, Term2) -> kzd_audit_log:doc()), Term1, Term2}.
-type audit_log_fun() ::  audit_log_fun_2() | audit_log_fun_3().

-spec base_audit_log_fold(audit_log_fun(), kzd_audit_log:doc()) -> kzd_audit_log:doc().
base_audit_log_fold({F, V}, Acc) -> F(Acc, V);
base_audit_log_fold({F, V1, V2}, Acc) -> F(Acc, V1, V2).

-spec base_audit_account(cb_context:context(), kz_services:services()) ->
                                kz_json:object().
base_audit_account(Context, Services) ->
    AccountName = kzd_accounts:name(cb_context:account_doc(Context)),
    Diff = kz_services:diff_quantities(Services),

    kz_json:from_list(
      props:filter_empty(
        [{<<"account_name">>, AccountName}
         ,{<<"diff_quantities">>, Diff}
        ]
       )).

-spec base_auth_user_info(cb_context:context()) -> kz_json:object().
base_auth_user_info(Context) ->
    AccountJObj = cb_context:auth_account_doc(Context),
    AuthDoc = cb_context:auth_doc(Context),
    kz_json:from_list(
      props:filter_empty(
        [{<<"account_id">>, kz_doc:account_id(AccountJObj)}
        ,{<<"account_name">>, kzd_accounts:name(AccountJObj)}
        ,{<<"created">>, kz_doc:created(AccountJObj)}
        ,{<<"realm">>, kzd_accounts:realm(AccountJObj)}
        ,{<<"language">>, kzd_accounts:language(AccountJObj)}
        ,{<<"timezone">>, kzd_accounts:timezone(AccountJObj)}
        ,{<<"auth_user_id">>, kz_json:get_value(<<"owner_id">>, AuthDoc)}
        ,{<<"original_auth_account_id">>, kz_json:get_value(<<"original_account_id">>, AuthDoc)}
        ,{<<"original_auth_user_id">>, kz_json:get_value(<<"original_owner_id">>, AuthDoc)}
        ]
      )
     ).

-spec save_an_audit_log(cb_context:context(), kz_services:services() | 'undefined') -> 'ok'.
save_an_audit_log(_Context, 'undefined') -> 'ok';
save_an_audit_log(Context, Services) ->
    BaseAuditLog = base_audit_log(Context, Services),
    lager:debug("attempting to save audit log for ~s (~s)"
                ,[cb_context:account_id(Context), kz_services:account_id(Services)]
               ),
    case cb_context:account_id(Context) =:= kz_services:account_id(Services) of
        'true' -> 'ok';
        'false' ->
            save_subaccount_audit_log(Context, BaseAuditLog)
    end,
    maybe_notify_reseller(Context, Services, BaseAuditLog),
    kzd_audit_log:save(Services, BaseAuditLog).

-spec save_subaccount_audit_log(cb_context:context(), kzd_audit_log:doc()) -> 'ok'.
save_subaccount_audit_log(Context, BaseAuditLog) ->
    MODb = cb_context:account_modb(Context),
    case kazoo_modb:save_doc(MODb, BaseAuditLog) of
        {ok, _} ->
            lager:debug("saved audit log for account ~s", [cb_context:account_id(Context)]);
        {error, _Reason} ->
            lager:debug("failed to save audit log (account ~s) : ~p", [cb_context:account_id(Context), _Reason])
    end.

-spec maybe_notify_reseller(cb_context:context(), kz_services:services(), kz_json:object()) -> 'ok'.
maybe_notify_reseller(Context, Services, AuditLog) ->
    ResellerId = kzd_services:reseller_id(kz_services:services_json(Services)),
    case ResellerId =:= kzd_audit_log:authenticating_user_account_id(AuditLog) of
        'true' -> 'ok';
        'false' ->
            Props = [{<<"Account-ID">>, cb_context:account_id(Context)}
                    ,{<<"Audit-Log">>, AuditLog}
                    ,{<<"Timestamp">>, kz_time:now_s()}
                     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],
            kapps_notify_publisher:cast(Props, fun kapi_notifications:publish_service_added/1)
  end.
