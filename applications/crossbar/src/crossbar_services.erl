%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(crossbar_services).

-export([maybe_dry_run/2]).

-include("crossbar.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_dry_run(cb_context:context(), function()) -> any().
maybe_dry_run(Context, Callback) ->
    DryRun = not(cb_context:accepting_charges(Context)),
    case DryRun of
        'false' ->
            lager:debug("accepting charges"),
            Callback();
        'true' ->
            lager:debug("not accepting charges"),
            Doc = cb_context:doc(Context),
            Type = wh_json:get_value(<<"pvt_type">>, Doc),
            RespJObj = dry_run(Context, Type),
            case wh_json:is_empty(RespJObj) of
                'true' ->
                    lager:debug("no charges"),
                    Callback();
                'false' -> crossbar_util:response_402(RespJObj, Context)
            end
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec dry_run(cb_context:context(), ne_binary()) -> wh_json:object().
dry_run(Context, <<"device">>) ->
    lager:error("dry run device"),
    Services = fetch_service(Context),
    JObj = cb_context:doc(Context),
    DeviceType = wh_json:get_value(<<"device_type">>, JObj),
    UpdatedServices = wh_service_devices:reconcile(Services, DeviceType),
    wh_services:dry_run(UpdatedServices);
dry_run(Context, <<"user">>) ->
    lager:error("dry run user"),
    Services = fetch_service(Context),
    JObj = cb_context:doc(Context),
    UserType = wh_json:get_value(<<"priv_level">>, JObj),
    UpdatedServices = wh_service_users:reconcile(Services, UserType),
    wh_services:dry_run(UpdatedServices);
dry_run(Context, <<"limits">>) ->
    lager:error("dry run limits"),
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
    lager:error("dry run port_request"),
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
    lager:error("dry run app"),
    [{<<"apps_store">>, [Id]} | _] = cb_context:req_nouns(Context),
    case wh_service_ui_apps:is_in_use(cb_context:req_data(Context)) of
        'false' -> wh_json:new();
        'true' ->
            Services = fetch_service(Context),
            AppName = wh_json:get_value(<<"name">>, cb_context:fetch(Context, Id)),
            UpdatedServices = wh_service_ui_apps:reconcile(Services, AppName),
            wh_services:dry_run(UpdatedServices)
    end;
dry_run(_Context, _Type) ->
    lager:error("unknown type ~p, cannot execute dry run", [_Type]),
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


