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
-module(cb_call_inspector).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,validate/1
        ]).

-include("../crossbar.hrl").

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.call_inspector">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.call_inspector">>, ?MODULE, 'resource_exists'),
    crossbar_bindings:bind(<<"*.validate.call_inspector">>, ?MODULE, 'validate').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /skels => []
%%    /skels/foo => [<<"foo">>]
%%    /skels/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /skels mights load a list of skel objects
%% /skels/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    QueryString = cb_context:query_string(Context),
    case wh_json:get_ne_value(<<"call_id">>, QueryString) of
        'undefined' ->
            Message = <<"Requests must include a query string parameter 'call_id'">>,
            cb_context:add_validation_error(<<"call_id">>, <<"required">>, Message, Context);
        CallId ->
            inspect_call_id(CallId, Context)
    end.

-spec inspect_call_id(ne_binary(), cb_context:context()) -> cb_context:context().
inspect_call_id(CallId, Context) ->
    Req = [{<<"Call-ID">>, CallId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case wh_amqp_worker:call(Req
                            ,fun wapi_inspector:publish_lookup_req/1
                            ,fun wapi_inspector:lookup_resp_v/1
                            )
    of
        {'ok', JObj} ->
            crossbar_util:response(JObj, Context);
        {'timeout', _Resp} ->
            lager:debug("timeout: ~p", [_Resp]),
            crossbar_util:response_datastore_timeout(Context);
        {'error', _E} ->
            lager:debug("error: ~p", [_E]),
            crossbar_util:response_datastore_timeout(Context)
    end.
