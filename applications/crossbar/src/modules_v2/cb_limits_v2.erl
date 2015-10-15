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

-include("../crossbar.hrl").
-include_lib("whistle/src/wh_json.hrl").

-define(CB_LIST, <<"limits/crossbar_listing">>).
-define(PVT_TYPE, <<"limits">>).
-define(LIMITS_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".", (?PVT_TYPE)/binary>>).
-define(DEFAULT_LEAK_PVT_FIELDS, [<<"allow_prepay">>, <<"allow_postpay">>, <<"max_postpay_amount">>]).
-define(DEFAULT_RESELLER_PVT_FIELDS, [<<"allow_prepay">>, <<"allow_postpay">>, <<"max_postpay_amount">>]).

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
    try wh_services:allow_updates(AccountId) andalso is_allowed(Context) of
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

-spec is_allowed(cb_context:context()) -> boolean().
is_allowed(Context) ->
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    IsSystemAdmin = wh_util:is_system_admin(AuthAccountId),
    {'ok', MasterAccount} = whapps_util:get_master_account_id(),
    case wh_services:find_reseller_id(AccountId) of
        AuthAccountId ->
            lager:debug("allowing reseller to update limits"),
            'true';
        MasterAccount ->
            lager:debug("allowing direct account to update limits"),
            'true';
        _Else when IsSystemAdmin ->
            lager:debug("allowing system admin to update limits"),
            'true';
        _Else ->
            lager:debug("sub-accounts of non-master resellers must contact the reseller to change their limits"),
            'false'
    end.

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
    Callback =
        fun() ->
                Routines = [fun crossbar_doc:save/1
                            ,fun maybe_leak_pvt_fields/1
                            ,fun maybe_reseller_pvt_fields/1
                           ],
                cb_context:setters(Context, Routines)
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
                ,fun maybe_leak_pvt_fields/1
                ,fun maybe_reseller_pvt_fields/1
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
                ,fun maybe_set_pvt_fields/1
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

-spec maybe_leak_pvt_fields(cb_context:context()) -> cb_context:context().
maybe_leak_pvt_fields(Context) ->
    LeakFields = whapps_config:get_non_empty(?LIMITS_CONFIG_CAT, <<"leak_pvt_fields">>, ?DEFAULT_LEAK_PVT_FIELDS),
    leak_pvt_fields(Context, cb_context:doc(Context), LeakFields).

-spec maybe_reseller_pvt_fields(cb_context:context()) -> cb_context:context().
maybe_reseller_pvt_fields(Context) ->
    case is_allowed(Context) of
        'true' ->
            ResellerFields = whapps_config:get_non_empty(?LIMITS_CONFIG_CAT, <<"reseller_pvt_fields">>, ?DEFAULT_RESELLER_PVT_FIELDS),
            leak_pvt_fields(Context, cb_context:doc(Context), ResellerFields);
        'false' -> Context
    end.

-spec leak_pvt_fields(cb_context:context(), wh_json:object(), ne_binaries()) -> cb_context:context().
leak_pvt_fields(Context, 'undefined', _Fields) -> Context;
leak_pvt_fields(Context, _Doc, []) -> Context;
leak_pvt_fields(Context, Doc, Fields) when is_list(Fields) ->
    {_, LeakData} = lists:foldl(fun leak_field/2, {Doc, wh_json:new()}, Fields),
    RespData = cb_context:resp_data(Context),
    cb_context:set_resp_data(Context, wh_json:merge_recursive(RespData, LeakData));
leak_pvt_fields(Context, _Doc, _Fields) -> Context.

-spec leak_field(ne_binary(), {wh_json:object(), wh_json:object()}) -> {wh_json:object(), wh_json:object()}.
leak_field(<<"pvt_", Field/binary>>, {Doc, LeakData}) -> leak_field(Field, {Doc, LeakData});
leak_field(Field, {Doc, LeakData}) when is_binary(Field)->
    case wh_json:get_value(<<"pvt_", Field/binary>>, Doc) of
        'undefined' -> {Doc, LeakData};
        Value -> {Doc, wh_json:set_value(Field, Value, LeakData)}
    end;
leak_field(_Field, {Doc, LeakData}) -> {Doc, LeakData}.

-spec maybe_set_pvt_fields(cb_context:context()) -> cb_context:context().
maybe_set_pvt_fields(Context) ->
    ResellerFields = whapps_config:get_non_empty(?LIMITS_CONFIG_CAT, <<"reseller_pvt_fields">>, ?DEFAULT_RESELLER_PVT_FIELDS),
    maybe_leak_pvt_fields(set_pvt_fields(Context, cb_context:doc(Context), ResellerFields)).

-spec set_pvt_fields(cb_context:context(), wh_json:object(), ne_binaries()) -> cb_context:context().
set_pvt_fields(Context, _Doc, []) -> Context;
set_pvt_fields(Context, Doc, Fields) when is_list(Fields) ->
    NewDoc = lists:foldl(fun set_pvt_field/2, Doc, Fields),
    cb_context:set_doc(Context, NewDoc);
set_pvt_fields(Context, _Doc, _Fields) -> Context.

-spec set_pvt_field(ne_binary(), {wh_json:object(), wh_json:object()}) -> {wh_json:object(), wh_json:object()}.
set_pvt_field(<<"pvt_", Field/binary>>, Doc) -> set_pvt_field(Field, Doc);
set_pvt_field(Field, Doc) when is_binary(Field)->
    NewDoc = wh_json:delete_key(<<"pvt_", Field/binary>>, Doc),
    case wh_json:get_value(Field, NewDoc) of
        'undefined' -> NewDoc;
        Value ->
            wh_json:delete_key(Field,
                               wh_json:set_value(<<"pvt_", Field/binary>>, Value, NewDoc)
                              )
    end;
set_pvt_field(_Field, Doc) -> Doc.
