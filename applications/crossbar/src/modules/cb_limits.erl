%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
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
         ,reconcile_services/1
        ]).

-include("../crossbar.hrl").
-include_lib("whistle/src/wh_json.hrl").

-define(CB_LIST, <<"limits/crossbar_listing">>).
-define(PVT_TYPE, <<"limits">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.limits">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.limits">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.billing">>, ?MODULE, billing),
    _ = crossbar_bindings:bind(<<"*.validate.limits">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"*.execute.post.limits">>, ?MODULE, post),
    crossbar_bindings:bind(<<"*.finish_request.*.limits">>, ?MODULE, reconcile_services).    

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
resource_exists() ->
    true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Ensure we will be able to bill for devices
%% @end
%%--------------------------------------------------------------------
billing(#cb_context{req_nouns=[{<<"limits">>, _}|_], req_verb = ?HTTP_GET}=Context) ->
    Context;
billing(#cb_context{req_nouns=[{<<"limits">>, _}|_]
                    ,account_id=AccountId, auth_account_id=AuthAccountId}=Context) ->
    try wh_services:allow_updates(AccountId) 
             andalso authd_account_allowed_updates(AccountId, AuthAccountId) 
    of
        true -> Context;
        false ->
            Message = <<"Please contact your phone provider to add limits.">>,
            cb_context:add_system_error(forbidden, [{details, Message}], Context)
    catch
        throw:{Error, Reason} ->
            crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
    end;
billing(Context) -> Context.

authd_account_allowed_updates(AccountId, AuthAccountId) ->
    {ok, MasterAccount} = whapps_util:get_master_account_id(),
    case wh_services:find_reseller_id(AccountId) of
        AuthAccountId -> 
            lager:debug("allowing reseller to update limits", []),
            true;
        MasterAccount -> 
            lager:debug("allowing direct account to update limits", []),
            true;
        _Else ->
            lager:debug("sub-accounts of non-master resellers must contact the reseller to change their limits", []),
            false
    end.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Bill for devices
%% @end
%%--------------------------------------------------------------------
-spec reconcile_services(#cb_context{}) -> #cb_context{}.
reconcile_services(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    Context;
reconcile_services(#cb_context{account_id=AccountId}=Context) ->
    _ = wh_services:reconcile(AccountId, <<"limits">>),
    Context.
            
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(#cb_context{}) -> #cb_context{}.
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    load_limit(Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context) ->
    update_limits(Context).

-spec post(#cb_context{}) -> #cb_context{}.
post(Context) ->
    crossbar_doc:save(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a Limit document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_limit(#cb_context{}) -> #cb_context{}.
load_limit(Context) ->
    maybe_handle_load_failure(crossbar_doc:load(?PVT_TYPE, Context)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing device document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_limits(#cb_context{}) -> #cb_context{}.
update_limits(#cb_context{}=Context) ->
    cb_context:validate_request_data(<<"limits">>, Context, fun on_successful_validation/1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(#cb_context{}) -> #cb_context{}.
on_successful_validation(#cb_context{}=Context) ->
    maybe_handle_load_failure(crossbar_doc:load_merge(?PVT_TYPE, Context)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec maybe_handle_load_failure(#cb_context{}) -> #cb_context{}.
maybe_handle_load_failure(#cb_context{resp_error_code=404, req_data=Data}=Context) ->
    NewLimits = wh_json:from_list([{<<"pvt_type">>, ?PVT_TYPE}
                                   ,{<<"_id">>, ?PVT_TYPE}
                                  ]),
    J = wh_json:merge_jobjs(NewLimits, wh_json:public_fields(Data)),
    %% In this case we are using the validator to populate defaults
    {pass, JObj} = wh_json_validator:is_valid(J, <<"limits">>),
    Context#cb_context{resp_status=success
                       ,resp_data=wh_json:public_fields(JObj)
                       ,doc=crossbar_doc:update_pvt_parameters(JObj, Context)
                      };
maybe_handle_load_failure(Context) -> Context.
