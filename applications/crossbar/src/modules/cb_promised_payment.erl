%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Dinkor Media Group (Sergey Korobkov)
%%%-------------------------------------------------------------------
-module(cb_promised_payment).

-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,validate/1
        ,post/1
        ,patch/1
        ]).

-include("crossbar.hrl").

-define(PVT_TYPE, <<"limits">>).
-define(PVT_OBJECT, <<"pvt_promised_payment">>).
-define(CHECK_FIELDS, [{<<"amount">>, <<"max_amount">>}
                      ,{<<"duration">>, <<"max_duration">>}
                      ,{<<"armed">>, 'ignore'}
                      ,{<<"enabled">>, 'forbid'}
                      ,{<<"start">>, 'forbid'}
                      ,{<<"max_duration">>, 'forbid'}
                      ,{<<"max_amount">>, 'forbid'}
                      ]).

%%%===================================================================
%%% API
%%%===================================================================
-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.promised_payment">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.promised_payment">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.promised_payment">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.promised_payment">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.promised_payment">>, ?MODULE, 'patch').

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
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH].

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
    validate_promised_payment(Context, cb_context:req_verb(Context)).

-spec validate_promised_payment(cb_context:context(), http_method()) -> cb_context:context().
validate_promised_payment(Context, ?HTTP_GET) ->
    load_promised_payment(Context);
validate_promised_payment(Context, ?HTTP_POST) ->
    cb_context:validate_request_data(<<"promised_payment">>, Context, fun on_successfull_validation/1);
validate_promised_payment(Context, ?HTTP_PATCH) ->
    PromisedPayment = fetch_promised_payments(Context),
    NewReqData = kz_json:merge_recursive(PromisedPayment, cb_context:req_data(Context)),
    Context1 = cb_context:set_req_data(Context, NewReqData),
    cb_context:validate_request_data(<<"promised_payment">>, Context1, fun on_successfull_validation/1).

-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    update(Context).

-spec patch(cb_context:context()) -> cb_context:context().
patch(Context) ->
    update(Context).

-spec update(cb_context:context()) -> cb_context:context().
update(Context) ->
    Doc = cb_context:doc(Context),
    ReqData = cb_context:req_data(Context),
    NewDoc = kz_json:set_value(?PVT_OBJECT, ReqData, Doc),
    Context1 = crossbar_doc:save(cb_context:set_doc(Context, NewDoc)),
    cb_context:set_resp_data(Context1, ReqData).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_promised_payment(cb_context:context()) -> cb_context:context().
load_promised_payment(Context) ->
    PromisedPayment = fetch_promised_payments(Context),
    Context1 = cb_context:set_resp_status(Context, 'success'),
    cb_context:set_resp_data(Context1, PromisedPayment).

-spec on_successfull_validation(cb_context:context()) -> cb_context:context().
on_successfull_validation(Context) ->
    Context1 = maybe_handle_load_failure(crossbar_doc:load(?PVT_TYPE, Context, ?TYPE_CHECK_OPTION(?PVT_TYPE))),
    PromisedPayment = fetch_promised_payments(Context),
    Doc = cb_context:doc(Context1),
    NewDoc = kz_json:set_value(?PVT_OBJECT, PromisedPayment, Doc),
    Context2 = cb_context:set_doc(Context, NewDoc),
    case is_admin_update(Context2) of
        'true' -> Context2;
        'false' -> update_by_user(Context2)
    end.

-spec fetch_promised_payments(cb_context:context()) -> kz_json:object().
fetch_promised_payments(Context) ->
    AccountId = cb_context:account_id(Context),
    Limits = j5_limits:fetch(AccountId),
    j5_limits:promised_payment(Limits).

-spec update_by_user(cb_context:context()) -> cb_context:context().
update_by_user(Context) ->
    IsEnabled = kz_json:is_true([?PVT_OBJECT, <<"enabled">>], cb_context:doc(Context)),
    case IsEnabled of
        'true' ->
            Routines = [fun check_values/2
                       ,fun maybe_change_arming/2
                       ],
            lists:foldl(fun(F, C) ->
                                case cb_context:resp_status(C) of
                                    'success' -> F(C, Context);
                                    _ -> C
                                end
                        end
                       ,Context
                       ,Routines
                       );
        'false' ->
            Msg = <<"promised payment function is not enabled, contact with your reseller to enable it">>,
            cb_context:add_validation_error(<<"enabled">>
                                           ,<<"forbidden">>
                                           ,Msg
                                           ,Context
                                           )
    end.

-spec check_values(cb_context:context(), cb_context:context()) -> cb_context:context().
check_values(Context, OrigContext) ->
    Doc = cb_context:doc(OrigContext),
    lists:foldl(fun({K, 'ignore'}, Ctx) ->
                        J = cb_context:req_data(Ctx),
                        V = cb_context:req_value(OrigContext, K),
                        cb_context:set_req_data(Ctx, kz_json:set_value(K, V, J));
                   ({K, 'forbid'}, Ctx) ->
                        V = cb_context:req_value(OrigContext, K),
                        VDoc = kz_json:get_value([?PVT_OBJECT, K], Doc),
                        case V =:= VDoc of
                            'true' ->
                                J = cb_context:req_data(Ctx),
                                cb_context:set_req_data(Ctx, kz_json:set_value(K, V, J));
                            'false' ->
                                Msg = <<"user does not allow to change it">>,
                                cb_context:add_validation_error(K
                                                               ,<<"forbidden">>
                                                               ,Msg
                                                               ,Ctx
                                                               )
                        end;
                   ({K, KDoc}, Ctx) ->
                        V = cb_context:req_value(OrigContext, K),
                        VDoc = kz_json:get_value([?PVT_OBJECT, KDoc], Doc),
                        case V > VDoc of
                            'false' ->
                                J = cb_context:req_data(Ctx),
                                cb_context:set_req_data(Ctx, kz_json:set_value(K, V, J));
                            'true' ->
                                ErrV =  kz_util:to_binary(VDoc),
                                Msg = <<"value should not exceed ", KDoc/binary, "(", ErrV/binary, ")">>,
                                cb_context:add_validation_error(K
                                                               ,<<"maximum">>
                                                               ,Msg
                                                               ,Ctx
                                                               )
                        end
                end
               ,cb_context:set_req_data(Context, kz_json:new())
               ,?CHECK_FIELDS
               ).

-spec maybe_change_arming(cb_context:context(), cb_context:context()) -> cb_context:context().
maybe_change_arming(Context, OrigContext) ->
    ReqValue = kz_util:is_true(cb_context:req_value(OrigContext, <<"armed">>)),
    DocValue = kz_json:is_true([?PVT_OBJECT, <<"armed">>], cb_context:doc(OrigContext), 'false'),
    case ReqValue =:= DocValue of
        'true' -> Context;
        'false' when ReqValue =:= 'true' -> try_arming(Context);
        'false' -> try_disarming(Context)
    end.

-spec try_arming(cb_context:context()) -> cb_context:context().
try_arming(Context) ->
    ReqData = cb_context:req_data(Context),
    MaxDuration = kz_json:get_value(<<"max_duration">>, ReqData),
    MaxAmount = kz_json:get_value(<<"max_amount">>, ReqData),
    JObj = kz_json:set_values([{<<"duration">>, kz_json:get_value(<<"duration">>, ReqData, MaxDuration)}
                              ,{<<"amount">>, kz_json:get_value(<<"amount">>, ReqData, MaxAmount)}
                              ,{<<"armed">>, 'true'}
                              ,{<<"start">>, kz_util:current_tstamp()}
                              ]
                             ,ReqData
                             ),
    cb_context:set_req_data(Context, kz_json:merge_recursive(ReqData, JObj)).

-spec try_disarming(cb_context:context()) -> cb_context:context().
try_disarming(Context) ->
    AccountId = cb_context:account_id(Context),
    Limits = j5_limits:fetch(AccountId),
    %% TODO: use some system-wyde function to check user balance
    Balance = case j5_limits:allow_postpay(Limits) of
                  'true' -> wht_util:current_balance(AccountId) + j5_limits:max_postpay(Limits);
                  'false' -> wht_util:current_balance(AccountId)
              end,
    case Balance > 0 of
        'true' ->
            JObj = kz_json:from_list([{<<"armed">>, 'false'}]),
            ReqData = cb_context:req_data(Context),
            NewReqData = kz_json:merge_recursive(ReqData, JObj),
            cb_context:set_req_data(Context, NewReqData);
        'false' ->
            cb_context:add_system_error('no_credit', Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_admin_update(cb_context:context()) -> boolean().
is_admin_update(Context) ->
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    IsSystemAdmin = kz_util:is_system_admin(AuthAccountId),
    {'ok', MasterAccount} = kapps_util:get_master_account_id(),
    case kz_services:find_reseller_id(AccountId) of
        AuthAccountId ->
            lager:debug("allowing reseller to update promised payment"),
            'true';
        MasterAccount ->
            lager:debug("allowing super-admin to update promised payment"),
            'true';
        _Else when IsSystemAdmin ->
            lager:debug("allowing system admin to update promised payment"),
            'true';
        _Else ->
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_handle_load_failure(cb_context:context()) -> cb_context:context().
-spec maybe_handle_load_failure(cb_context:context(), pos_integer()) -> cb_context:context().
maybe_handle_load_failure(Context) ->
    maybe_handle_load_failure(Context, cb_context:resp_error_code(Context)).

maybe_handle_load_failure(Context, 404) ->
    Data = cb_context:req_data(Context),
    NewLimits = kz_json:from_list([{<<"pvt_type">>, ?PVT_TYPE}
                                  ,{<<"_id">>, ?PVT_TYPE}
                                  ]),
    JObj = kz_json_schema:add_defaults(kz_json:merge_jobjs(NewLimits, kz_json:public_fields(Data))
                                      ,<<"limits">>
                                      ),

    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, kz_json:public_fields(JObj)}
                       ,{fun cb_context:set_doc/2, crossbar_doc:update_pvt_parameters(JObj, Context)}
                       ]);
maybe_handle_load_failure(Context, _RespCode) -> Context.
