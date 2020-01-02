%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_limits).

-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,validate/1
        ,post/1
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-define(CB_LIST, <<"limits/crossbar_listing">>).
-define(PVT_TYPE, <<"limits">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.limits">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.limits">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.limits">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.limits">>, ?MODULE, 'post'),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_limits(Context, cb_context:req_verb(Context)).

-spec validate_limits(cb_context:context(), http_method()) -> cb_context:context().
validate_limits(Context, ?HTTP_GET) ->
    load_limit(Context);
validate_limits(Context, ?HTTP_POST) ->
    case is_allowed(Context) of
        'false' ->
            Message = <<"Please contact your phone provider to add limits.">>,
            cb_context:add_system_error('forbidden'
                                       ,kz_json:from_list([{<<"message">>, Message}])
                                       ,Context
                                       );
        'true' ->
            cb_context:validate_request_data(<<"limits">>
                                            ,cleanup_leaky_keys(Context)
                                            ,fun on_successful_validation/1
                                            )
    end.

-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    crossbar_doc:save(Context).

-spec is_allowed(cb_context:context()) -> boolean().
is_allowed(Context) ->
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    IsSystemAdmin = kzd_accounts:is_superduper_admin(AuthAccountId),
    {'ok', MasterAccount} = kapps_util:get_master_account_id(),
    case kz_services_reseller:get_id(AccountId) of
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

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Load a Limit document from the database
%% @end
%%------------------------------------------------------------------------------
-spec load_limit(cb_context:context()) -> cb_context:context().
load_limit(Context) ->
    leak_pvt_fields(crossbar_doc:load(?PVT_TYPE, Context, ?TYPE_CHECK_OPTION(?PVT_TYPE))).

-spec leak_pvt_fields(cb_context:context()) -> cb_context:context().
leak_pvt_fields(Context) ->
    leak_pvt_fields(Context, cb_context:resp_status(Context)).

-spec leak_pvt_fields(cb_context:context(), crossbar_status()) -> cb_context:context().
leak_pvt_fields(Context, 'success') ->
    Routines = [fun leak_pvt_allow_postpay/1
               ,fun leak_pvt_max_postpay_amount/1
               ],
    cb_context:setters(Context, Routines);
leak_pvt_fields(Context, _Status) ->
    maybe_handle_load_failure(Context).

-spec leak_pvt_allow_postpay(cb_context:context()) -> cb_context:context().
leak_pvt_allow_postpay(Context) ->
    cb_context:set_resp_data(Context
                            ,kz_json:set_value(<<"allow_postpay">>
                                              ,kz_json:is_true(<<"pvt_allow_postpay">>, cb_context:doc(Context), 'false')
                                              ,cb_context:resp_data(Context)
                                              )
                            ).

-spec leak_pvt_max_postpay_amount(cb_context:context()) -> cb_context:context().
leak_pvt_max_postpay_amount(Context) ->
    cb_context:set_resp_data(Context
                            ,kz_json:set_value(<<"max_postpay_amount">>
                                              ,abs(kz_json:get_number_value(<<"pvt_max_postpay_amount">>, cb_context:doc(Context), 0))
                                              ,cb_context:resp_data(Context)
                                              )
                            ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(cb_context:context()) -> cb_context:context().
on_successful_validation(Context) ->
    maybe_handle_load_failure(crossbar_doc:load_merge(?PVT_TYPE, Context, ?TYPE_CHECK_OPTION(?PVT_TYPE))).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec maybe_handle_load_failure(cb_context:context()) ->
          cb_context:context().
maybe_handle_load_failure(Context) ->
    maybe_handle_load_failure(Context, cb_context:resp_error_code(Context)).

-spec maybe_handle_load_failure(cb_context:context(), pos_integer()) ->
          cb_context:context().
maybe_handle_load_failure(Context, 404) ->
    Data = cb_context:req_data(Context),
    NewLimits = kz_json:from_list([{<<"pvt_type">>, ?PVT_TYPE}
                                  ,{<<"_id">>, ?PVT_TYPE}
                                  ]),
    JObj = kz_json_schema:add_defaults(kz_json:merge_jobjs(NewLimits, kz_doc:public_fields(Data))
                                      ,<<"limits">>
                                      ),

    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, kz_doc:public_fields(JObj)}
                       ,{fun cb_context:set_doc/2, crossbar_doc:update_pvt_parameters(JObj, Context)}
                       ]);
maybe_handle_load_failure(Context, _RespCode) -> Context.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cleanup_leaky_keys(cb_context:context()) -> cb_context:context().
cleanup_leaky_keys(Context) ->
    RemoveKeys = [<<"allow_postpay">>
                 ,<<"max_postpay_amount">>
                 ],
    ReqData = kz_json:delete_keys(RemoveKeys, cb_context:req_data(Context)),
    cb_context:set_req_data(Context, ReqData).
