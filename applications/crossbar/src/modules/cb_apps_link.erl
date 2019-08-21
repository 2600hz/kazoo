%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Listing of all expected v1 callbacks
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_apps_link).

-export([init/0
        ,authorize/1
        ,allowed_methods/1
        ,resource_exists/1
        ,validate/2
        ]).

-include("crossbar.hrl").

-define(DEFAULT_LANGUAGE, <<"en-US">>).
-define(AUTHORIZE, <<"authorize">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.apps_link">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.apps_link">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.apps_link">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.apps_link">>, ?MODULE, 'get').

%%------------------------------------------------------------------------------
%% @doc Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_nouns(cb_context:req_nouns(Context)).

-spec authorize_nouns(req_nouns()) -> boolean().
authorize_nouns([{<<"apps_link">>, _}]) -> 'true';
authorize_nouns(_Nouns) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?AUTHORIZE) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /apps_link => []
%%    /apps_link/foo => [<<"foo">>]
%%    /apps_link/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists(path_token()) -> 'true'.
resource_exists(?AUTHORIZE) -> 'true'.


%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /apps_link might load a list of skel objects
%% /apps_link/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?AUTHORIZE) ->
    JObj = kz_json:from_list(
             [{<<"auth_token">>, auth_info(Context)}
             ,{<<"account">>, account_info(Context)}
             ]),
    crossbar_util:response(JObj, Context).

-spec account_info(cb_context:context()) -> kz_json:object().
account_info(Context) ->
    AccountId = get_request_account(Context),
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    kz_json:from_list(
      [{<<"account_id">>, AccountId}
      ,{<<"account_name">>, kzd_accounts:fetch_name(AccountId)}
      ,{<<"language">>, crossbar_util:get_language(AccountId)}
      ,{<<"is_reseller">>, kz_services_reseller:is_reseller(AccountId)}
      ,{<<"reseller_id">>, kz_services_reseller:get_id(AccountId)}
      ,{<<"is_master">>, AccountId =:= MasterAccountId}
      ]).

-spec auth_info(cb_context:context()) -> kz_json:object().
auth_info(Context) ->
    JObj = cb_context:auth_doc(Context),
    AccountId = cb_context:auth_account_id(Context),
    OwnerId = kz_json:get_value(<<"owner_id">>, JObj),
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    kz_json:from_list(
      [{<<"account_id">>, AccountId}
      ,{<<"owner_id">>, OwnerId}
      ,{<<"account_name">>, kzd_accounts:fetch_name(AccountId)}
      ,{<<"method">>, kz_json:get_value(<<"method">>, JObj)}
      ,{<<"created">>, kz_doc:created(JObj)}
      ,{<<"language">>, crossbar_util:get_language(AccountId, OwnerId)}
      ,{<<"is_reseller">>, kz_services_reseller:is_reseller(AccountId)}
      ,{<<"reseller_id">>, kz_services_reseller:get_id(AccountId)}
      ,{<<"apps">>, crossbar_util:load_apps(AccountId, OwnerId)}
      ,{<<"is_master">>, AccountId =:= MasterAccountId}
      ]).

-spec get_request_account(cb_context:context()) -> kz_term:ne_binary().
get_request_account(Context) ->
    RequestNouns = cb_context:req_nouns(Context),
    case props:get_value(<<"accounts">>, RequestNouns) of
        'undefined' -> cb_context:auth_account_id(Context);
        [Else] -> Else
    end.
