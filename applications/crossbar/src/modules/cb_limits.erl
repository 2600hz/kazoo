%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_limits).

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

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.limits">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.limits">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.billing">>, ?MODULE, 'billing'),
    _ = crossbar_bindings:bind(<<"*.validate.limits">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.limits">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"*.finish_request.*.limits">>, 'cb_modules_util', 'reconcile_services').

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
    AuthAccountId = cb_context:auth_account_id(Context),
    try wh_services:allow_updates(AccountId)
             andalso authd_account_allowed_updates(AccountId, AuthAccountId)
    of
        'true' -> Context;
        'false' ->
            Message = <<"Please contact your phone provider to add limits.">>,
            cb_context:add_system_error('forbidden', [{'details', Message}], Context)
    catch
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end;
process_billing(Context, _Nouns, _Verb) -> Context.

-spec authd_account_allowed_updates(ne_binary(), ne_binary()) -> boolean().
authd_account_allowed_updates(AccountId, AuthAccountId) ->
    {'ok', MasterAccount} = whapps_util:get_master_account_id(),
    case wh_services:find_reseller_id(AccountId) of
        AuthAccountId ->
            lager:debug("allowing reseller to update limits"),
            'true';
        MasterAccount ->
            lager:debug("allowing direct account to update limits"),
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

validate_limits(Context, ?HTTP_GET) ->
    load_limit(Context);
validate_limits(Context, ?HTTP_POST) ->
    update_limits(Context).

-spec post(cb_context:context()) -> cb_context:context().
post(Context) -> crossbar_doc:save(Context).

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
    maybe_handle_load_failure(crossbar_doc:load(?PVT_TYPE, Context)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing device document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_limits(cb_context:context()) -> cb_context:context().
update_limits(Context) ->
    cb_context:validate_request_data(<<"limits">>, Context, fun on_successful_validation/1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(cb_context:context()) -> cb_context:context().
on_successful_validation(Context) ->
    maybe_handle_load_failure(crossbar_doc:load_merge(?PVT_TYPE, Context)).

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
