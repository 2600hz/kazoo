%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_limits_v2).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,billing/1
         ,validate/1
         ,post/1
        ]).

-include("crossbar.hrl").
-include_lib("whistle/src/wh_json.hrl").

-define(CB_LIST, <<"limits/crossbar_listing">>).
-define(PVT_TYPE, <<"limits">>).
-define(LIMITS_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".", (?PVT_TYPE)/binary>>).
-define(DEFAULT_ALLOWED_PVT_UPDATE, [<<"master">>, <<"reseller">>]).
-define(ALLOWED_PVT_UPDATE, whapps_config:get_non_empty(?LIMITS_CONFIG_CAT, <<"allowed_pvt_updates">>, ?DEFAULT_ALLOWED_PVT_UPDATE)).
-define(DEFAULT_ALLOWED_UPDATE, [<<"master">>, <<"master-client">>, <<"reseller">>]).
-define(ALLOWED_UPDATE, whapps_config:get_non_empty(?LIMITS_CONFIG_CAT, <<"allowed_updates">>, ?DEFAULT_ALLOWED_UPDATE)).
-define(LIMIT_PROPERTIES, [<<"allow_prepay">>, <<"allow_postpay">>, <<"max_postpay_amount">>
                          ,<<"twoway_trunks">>, <<"inbound_trunks">>, <<"outbound_trunks">>
                          ,<<"resource_consuming_calls">>, <<"calls">>, <<"burst_trunks">>
                          ,<<"enabled">>
                          ]).
-define(OVERRIDES_KEY, <<"overrides">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v2_resource.allowed_methods.limits">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v2_resource.resource_exists.limits">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v2_resource.billing">>, ?MODULE, 'billing'),
    _ = crossbar_bindings:bind(<<"v2_resource.validate.limits">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.post.limits">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"v2_resource.finish_request.*.limits">>, 'crossbar_services', 'reconcile').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Ensure we will be able to bill for devices
%% @end
%%--------------------------------------------------------------------
-spec billing(cb_context:context()) ->
                     cb_context:context().
billing(Context) ->
    process_billing(Context, cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec process_billing(cb_context:context(), req_nouns(), http_method()) ->
                             cb_context:context().
process_billing(Context, [{<<"limits">>, _}|_], ?HTTP_GET) ->
    Context;
process_billing(Context, [{<<"limits">>, _}|_], _Verb) ->
    AccountId = cb_context:account_id(Context),
    try wh_services:allow_updates(AccountId)
             andalso (allowed_updates(Context)
                      orelse allowed_pvt_updates(Context)
                     )
    of
        'true' -> Context;
        'false' ->
            Message = <<"Please contact your phone provider to add limits.">>,
            cb_context:add_system_error(
                'forbidden'
                ,wh_json:from_list([{<<"message">>, Message}])
                ,Context
            )
    catch
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end;
process_billing(Context, _Nouns, _Verb) -> Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_limits(Context, cb_context:req_verb(Context)).

-spec validate_limits(cb_context:context(), http_method()) -> cb_context:context().
validate_limits(Context, ?HTTP_GET) ->
    load_limit(Context);
validate_limits(Context, ?HTTP_POST) ->
    cb_context:validate_request_data(<<"limits">>, Context, fun on_successful_validation/1).

-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    case have_overrides_changed(Context) of
        'false' -> do_post(Context);
        'true' ->
            case allowed_pvt_updates(Context) of
                'true' -> handle_overrides(Context);
                'false' ->
                    Message = <<"You are not authorized to modify overrides, please resubmit without this property.">>,
                    cb_context:add_system_error(
                      'forbidden'
                      ,wh_json:from_list([{<<"message">>, Message}])
                      ,Context
                     )
            end
    end.

-spec have_overrides_changed(cb_context:contex()) -> boolean().
have_overrides_changed(Context) ->
    ReqData = cb_context:req_data(Context),
    case wh_json:get_ne_value(?OVERRIDES_KEY, ReqData) of
        'undefined' -> 'false';
        Overrides ->
            Doc = cb_context:doc(Context),
            lists:any(fun(Key) ->
                              wh_json:get_value(Key, Overrides)
                                  =/= wh_json:get_value(<<"pvt_", Key/binary>>, Doc)
                      end
                     ,?LIMIT_PROPERTIES
                     )
    end.

-spec handle_overrides(cb_context:context()) -> cb_context:context().
handle_overrides(Context) ->
    Overrides = wh_json:get_value(?OVERRIDES_KEY, cb_context:req_data(Context)),
    ReqData = lists:foldl(fun(Key, J) ->
                                  Value = wh_json:get_value(Key, Overrides),
                                  wh_json:set_value(<<"pvt_", Key/binary>>, Value, J)
                          end
                         ,cb_context:doc(Context)
                         ,wh_json:get_keys(Overrides)
                         ),
    do_post(cb_context:set_doc(Context, wh_json:delete_key(?OVERRIDES_KEY, ReqData))).

-spec do_post(cb_context:context()) -> cb_context:context().
do_post(Context) ->
    Callback =
        fun() ->
                crossbar_doc:save(Context)
        end,
    crossbar_services:maybe_dry_run(Context, Callback).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a Limit document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_limit(cb_context:context()) -> cb_context:context().
load_limit(Context) ->
    Routines = [fun maybe_handle_load_failure/1
               ,fun add_overrides/1
               ],
    cb_context:setters(crossbar_doc:load(?PVT_TYPE, Context), Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(cb_context:context()) -> cb_context:context().
on_successful_validation(Context) ->
    Routines = [fun maybe_handle_load_failure/1
                ,fun add_overrides/1
               ],
    cb_context:setters(crossbar_doc:load_merge(?PVT_TYPE, Context), Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_handle_load_failure(cb_context:context()) ->
                                       cb_context:context().
-spec maybe_handle_load_failure(cb_context:context(), pos_integer()) ->
                                       cb_context:context().
maybe_handle_load_failure(Context) ->
    maybe_handle_load_failure(Context, cb_context:resp_error_code(Context)).

maybe_handle_load_failure(Context, 404) ->
    Data = cb_context:req_data(Context),
    NewLimits = wh_json:from_list([{<<"pvt_type">>, ?PVT_TYPE}
                                   ,{<<"_id">>, ?PVT_TYPE}
                                  ]),
    JObj = wh_json_schema:add_defaults(wh_json:merge_jobjs(NewLimits, wh_json:public_fields(Data))
                                       ,<<"limits">>
                                      ),

    cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'success'}
                         ,{fun cb_context:set_resp_data/2, wh_json:public_fields(JObj)}
                         ,{fun cb_context:set_doc/2, crossbar_doc:update_pvt_parameters(JObj, Context)}
                        ]);
maybe_handle_load_failure(Context, _RespCode) -> Context.

-spec allowed_updates(cb_context:context()) -> boolean().
allowed_updates(Context) ->
    Type = get_authorized_type(Context),
    case lists:member(Type, ?ALLOWED_UPDATE) of
        'false' -> 'false';
        'true' ->
            lager:debug("allowing ~s to update limits"),
            'true'
    end.

-spec allowed_pvt_updates(cb_context:context()) -> boolean().
allowed_pvt_updates(Context) ->
    Type = get_authorized_type(Context),
    case lists:member(Type, ?ALLOWED_PVT_UPDATE) of
        'false' -> 'false';
        'true' ->
            lager:debug("allowing ~s to update limits overrides (pvt)"),
            'true'
    end.

-spec get_authorized_type(cb_context:context()) -> ne_binary().
get_authorized_type(Context) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    IsSystemAdmin = wh_util:is_system_admin(AuthAccountId),
    {'ok', MasterAccount} = whapps_util:get_master_account_id(),
    case wh_services:find_reseller_id(cb_context:account_id(Context)) of
        _Else when IsSystemAdmin -> <<"master">>;
        AuthAccountId -> <<"reseller">>;
        MasterAccount -> <<"master-client">>;
        _Else -> <<"reseller-client">>
    end.

-spec add_overrides(cb_context:context()) -> cb_context:context().
add_overrides(Context) ->
    Doc = cb_context:doc(Context),
    Resp = lists:foldl(fun(Key, JObj) ->
                               case wh_json:get_ne_value(<<"pvt_", Key/binary>>, Doc) of
                                   'undefined' -> JObj;
                                   Value -> wh_json:set_value([?OVERRIDES_KEY, Key], Value, JObj)
                               end
                       end
                      ,wh_json:set_value(?OVERRIDES_KEY, wh_json:new(), cb_context:resp_data(Context))
                      ,?LIMIT_PROPERTIES
                      ),
    cb_context:set_resp_data(Context, Resp).

