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
         ,allowed_methods/1
         ,resource_exists/0
         ,resource_exists/1
         ,validate/1
         ,validate/2
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

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_) ->
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

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

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
    C = cb_cdrs:validate(Context),
    case cb_context:resp_status(C) of
        'success' -> maybe_filter_cdrs(C);
        Else -> Else
    end.

-spec maybe_filter_cdrs(cb_context:context()) -> cb_context:context().
maybe_filter_cdrs(Context) ->
    case cb_context:resp_data(Context) of
        [] -> Context;
        'undefined' -> crossbar_util:response([], Context);
        Cdrs -> filter_cdrs(Context, Cdrs)
    end.

-spec filter_cdrs(cb_context:context(), ne_binaries()) -> cb_context:context().
filter_cdrs(Context, Cdrs) ->
    Req = [{<<"Call-IDs">>,
            [wh_json:get_value(<<"call_id">>, Cdr)
             || Cdr <- Cdrs
            ]
           }
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case wh_amqp_worker:call(Req
                            ,fun wapi_inspector:publish_filter_req/1
                            ,fun wapi_inspector:filter_resp_v/1
                            )
    of
        {'ok', JObj} ->
            Ids = wh_json:get_value(<<"Call-IDs">>, JObj, []),
            crossbar_util:response(
              [Cdr
               || Cdr <- Cdrs
                  ,lists:member(wh_json:get_value(<<"call_id">>, Cdr), Ids)
              ]
              ,Context
             );
        {'timeout', _Resp} ->
            lager:debug("timeout: ~p", [_Resp]),
            crossbar_util:response_datastore_timeout(Context);
        {'error', _E} ->
            lager:debug("error: ~p", [_E]),
            crossbar_util:response_datastore_timeout(Context)
    end.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, CallId) ->
    case wh_util:is_empty(CallId) of
        'true' ->
            cb_context:add_system_error('not_found', Context);
        'false' ->
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
            Response =
                wh_json:from_list(
                  [{<<"messages">>, wh_json:get_value(<<"Chunks">>, JObj)}
                  ,{<<"analysis">>, wh_json:get_value(<<"Analysis">>, JObj)}
                  ]
                 ),
            crossbar_util:response(Response, Context);
        {'timeout', _Resp} ->
            lager:debug("timeout: ~p", [_Resp]),
            crossbar_util:response_datastore_timeout(Context);
        {'error', _E} ->
            lager:debug("error: ~p", [_E]),
            crossbar_util:response_datastore_timeout(Context)
    end.
