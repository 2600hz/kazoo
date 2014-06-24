%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_apps_link).

-export([init/0
         ,authorize/1
         ,allowed_methods/1
         ,resource_exists/1
         ,validate/2
        ]).

-include("../crossbar.hrl").

-define(DEFAULT_LANGUAGE, <<"en-US">>).
-define(AUTHORIZE, <<"authorize">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.apps_link">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.apps_link">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.apps_link">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.apps_link">>, ?MODULE, 'get').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> 'false'.
authorize(#cb_context{req_nouns=[{<<"apps_link">>, _}]}) ->
    'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?AUTHORIZE) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /apps_link => []
%%    /apps_link/foo => [<<"foo">>]
%%    /apps_link/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(path_token()) -> 'true'.
resource_exists(?AUTHORIZE) -> 'true'.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /apps_link mights load a list of skel objects
%% /apps_link/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(#cb_context{auth_doc=Doc}=Context, ?AUTHORIZE) ->
    RequestNouns = cb_context:req_nouns(Context),
    RequestedAccountId = case props:get_value(<<"accounts">>, RequestNouns) of
                    'undefined' -> wh_json:get_value(<<"account_id">>, Doc, <<>>);
                    [Else] -> Else
                end,
    AccountId = wh_json:get_value(<<"account_id">>, Doc),
    JObj = wh_json:set_value(<<"account_id">>, RequestedAccountId, Doc),
    crossbar_util:response(crossbar_util:response_auth(JObj, AccountId), Context).

