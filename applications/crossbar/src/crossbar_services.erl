%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(crossbar_services).

-export([maybe_dry_run/2, maybe_dry_run/3]).

-include("crossbar.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_dry_run(cb_context:context(), function()) -> any().
-spec maybe_dry_run(cb_context:context(), function(), ne_binary() | wh_proplist()) -> any().
maybe_dry_run(Context, Callback) ->
    Doc = cb_context:doc(Context),
    Type = wh_json:get_value(<<"pvt_type">>, Doc),
    maybe_dry_run(Context, Callback, Type).

maybe_dry_run(Context, Callback, Type) when is_binary(Type) ->
    maybe_dry_run_by_type(Context, Callback, Type, cb_context:accepting_charges(Context));
maybe_dry_run(Context, Callback, Props) ->
    maybe_dry_run_by_props(Context, Callback, Props, cb_context:accepting_charges(Context)).

-spec maybe_dry_run_by_props(cb_context:context(), function(), wh_proplist(), boolean()) ->
                                    any().
maybe_dry_run_by_props(Context, Callback, Props, 'true') ->
    Type = props:get_ne_binary_value(<<"type">>, Props),
    RespJObj = dry_run(Context, Type, props:delete(<<"type">>, Props)),
    lager:debug("accepting charges"),
    _ = accepting_charges(Context, RespJObj),
    Callback();
maybe_dry_run_by_props(Context, Callback, Props, 'false') ->
    Type = props:get_ne_binary_value(<<"type">>, Props),
    RespJObj = dry_run(Context, Type, props:delete(<<"type">>, Props)),
    lager:debug("not accepting charges"),
    case wh_json:is_empty(RespJObj) of
        'true' ->
            lager:debug("no charges"),
            Callback();
        'false' -> crossbar_util:response_402(RespJObj, Context)
    end.

-spec maybe_dry_run_by_type(cb_context:context(), function(), ne_binary(), boolean()) ->
                                   any().
maybe_dry_run_by_type(Context, Callback, Type, 'true') ->
    RespJObj = dry_run(Context, Type),
    lager:debug("accepting charges"),
    _ = accepting_charges(Context, RespJObj),
    Callback();
maybe_dry_run_by_type(Context, Callback, Type, 'false') ->
    RespJObj = dry_run(Context, Type),
    lager:debug("not accepting charges"),
    case wh_json:is_empty(RespJObj) of
        'true' ->
            lager:debug("no charges"),
            Callback();
        'false' -> crossbar_util:response_402(RespJObj, Context)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec accepting_charges(cb_context:context(), wh_json:object()) -> 'ok' | 'error'.
accepting_charges(Context, JObj) ->
    Services = fetch_service(Context),
    Items = extract_items(wh_json:delete_key(<<"activation_charges">>, JObj)),
    Transactions =
        lists:foldl(
          fun(Item, Acc) ->
                  create_transactions(Context, Item, Acc)
          end
          ,[]
          ,Items
         ),
    wh_services:commit_transactions(Services, Transactions).

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
    Amount = wh_json:get_integer_value(<<"activation_charges">>, Item, 0),
    create_transactions(Context, Item, Acc, Quantity, Amount).

create_transactions(_Context, _Item, Acc, _Quantity, 0) -> Acc;
create_transactions(Context, Item, Acc, Quantity, Amount) ->
    AccountId = cb_context:account_id(Context),
    Routines = [fun set_meta_data/3
                ,fun set_event/3
               ],
    Transaction =
        lists:foldl(
          fun(F, T) -> F(Context, Item, T) end
          ,wh_transaction:debit(AccountId, Amount)
          ,Routines
         ),
    create_transactions(Context, Item, [Transaction|Acc], Quantity-1).

-spec set_meta_data(cb_context:context()
                    ,wh_json:object()
                    ,wh_transaction:transaction()
                   ) -> wh_transaction:transaction().
set_meta_data(Context, _Item, Transaction) ->
    MetaData =
        wh_json:from_list(
          [{<<"auth_account_id">>, cb_context:auth_account_id(Context)}]
         ),
    wh_transaction:set_metadata(MetaData, Transaction).

-spec set_event(cb_context:context() ,wh_json:object()
                ,wh_transaction:transaction()) -> wh_transaction:transaction().
set_event(_Context, Item, Transaction) ->
    Category = wh_json:get_value(<<"category">>, Item, <<>>),
    ItemValue = wh_json:get_value(<<"item">>, Item, <<>>),
    Event = <<Category/binary, ".", ItemValue/binary>>,
    wh_transaction:set_event(Event, Transaction).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec dry_run(cb_context:context(), ne_binary()) -> wh_json:object().
-spec dry_run(cb_context:context(), ne_binary(), wh_proplist()) -> wh_json:object().
dry_run(Context, <<"device">>) ->
    lager:debug("dry run device"),
    Services = fetch_service(Context),
    JObj = cb_context:doc(Context),
    DeviceType = wh_json:get_value(<<"device_type">>, JObj),
    UpdatedServices = wh_service_devices:reconcile(Services, DeviceType),
    wh_services:dry_run(UpdatedServices);
dry_run(Context, <<"user">>) ->
    lager:debug("dry run user"),
    Services = fetch_service(Context),
    JObj = cb_context:doc(Context),
    UserType = wh_json:get_value(<<"priv_level">>, JObj),
    UpdatedServices = wh_service_users:reconcile(Services, UserType),
    wh_services:dry_run(UpdatedServices);
dry_run(Context, <<"limits">>) ->
    lager:debug("dry run limits"),
    Services = fetch_service(Context),
    ReqData = cb_context:req_data(Context),
    Updates =
        wh_json:from_list(
          [{<<"twoway_trunks">>, wh_json:get_integer_value(<<"twoway_trunks">>, ReqData, 0)}
           ,{<<"inbound_trunks">>, wh_json:get_integer_value(<<"inbound_trunks">>, ReqData, 0)}
           ,{<<"outbound_trunks">>, wh_json:get_integer_value(<<"outbound_trunks">>, ReqData, 0)}
          ]),
    UpdatedServices = wh_service_limits:reconcile(Services, Updates),
    wh_services:dry_run(UpdatedServices);
dry_run(Context, <<"port_request">>) ->
    lager:debug("dry run port_request"),
    Services = fetch_service(Context),
    JObj = cb_context:doc(Context),
    Numbers = wh_json:get_value(<<"numbers">>, JObj),
    PhoneNumbers =
        wh_json:foldl(
          fun port_request_foldl/3
          ,wh_json:new()
          ,Numbers
         ),
    UpdatedServices = wh_service_phone_numbers:reconcile(PhoneNumbers, Services),
    wh_services:dry_run(UpdatedServices);
dry_run(Context, <<"app">>) ->
    lager:debug("dry run app"),
    [{<<"apps_store">>, [Id]} | _] = cb_context:req_nouns(Context),
    case wh_service_ui_apps:is_in_use(cb_context:req_data(Context)) of
        'false' -> wh_json:new();
        'true' ->
            Services = fetch_service(Context),
            AppName = wh_json:get_value(<<"name">>, cb_context:fetch(Context, Id)),
            UpdatedServices = wh_service_ui_apps:reconcile(Services, AppName),
            wh_services:dry_run(UpdatedServices)
    end;
dry_run(Context, <<"ips">>) ->
    lager:debug("dry run dedicated"),
    Services = fetch_service(Context),
    UpdatedServices = wh_service_ips:reconcile(Services, <<"dedicated">>),
    wh_services:dry_run(UpdatedServices);
dry_run(_Context, _Type) ->
    lager:warning("unknown type ~p, cannot execute dry run", [_Type]),
    wh_json:new().

dry_run(Context, <<"ips">>, Props) ->
    lager:debug("dry run dedicated"),
    Services = fetch_service(Context),
    UpdatedServices = wh_service_ips:reconcile(Services, Props),
    wh_services:dry_run(UpdatedServices);
dry_run(_Context, _Type, _Props) ->
    lager:warning("unknown type ~p, cannot execute dry run", [_Type]),
    wh_json:new().

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
            lager:debug("auth account is not a reseller, loading service account ~s", [AccountId]),
            wh_services:fetch(AccountId);
        'true' ->
            lager:debug("auth account is a reseller, loading service from reseller ~s", [AuthAccountId]),
            wh_services:fetch(AuthAccountId)
    end.
