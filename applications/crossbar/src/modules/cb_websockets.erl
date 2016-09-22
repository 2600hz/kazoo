%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors:
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(cb_websockets).

-export([init/0
        ,authenticate/1
        ,authorize/1
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_amqp/include/kapi_conf.hrl").
-include_lib("kazoo_documents/include/doc_types.hrl").

-define(CB_LIST, <<"websockets/crossbar_listing">>).

-define(TO_JSON(Binding, Event),
        kz_json:from_list([{<<"binding">>, Binding}
                          ,{<<"event">>, Event}
                          ])).

-define(CALL, [?TO_JSON(<<"call.CHANNEL_CREATE.*">>, <<"CHANNEL_CREATE">>)
              ,?TO_JSON(<<"call.CHANNEL_ANSWER.*">>, <<"CHANNEL_ANSWER">>)
              ,?TO_JSON(<<"call.CHANNEL_DESTROY.*">>, <<"CHANNEL_DESTROY">>)
              ,?TO_JSON(<<"call.CHANNEL_BRIDGE.*">>, <<"CHANNEL_BRIDGE">>)
              ]).

-define(OBJECTS,
        [?TO_JSON(<<"object.", A/binary, ".", T/binary>>, <<A/binary, ".", T/binary>>)
         || A <- ?DOC_ACTIONS, T <- ?DOC_TYPES
        ]).

-define(AVAILABLE,
        kz_json:from_list([{<<"call">>, ?CALL}
                          ,{<<"fax">>, [?TO_JSON(<<"fax.status.*">>, <<"fax">>)]}
                          ,{<<"object">>, ?OBJECTS}
                          ])).

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
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.websockets">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.websockets">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.websockets">>, ?MODULE, 'validate').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> {'true', cb_context:context()} | 'false'.
authenticate(Context) ->
    authenticate_req(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_req(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(_SocketId) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /websockets => []
%%    /websockets/foo => [<<"foo">>]
%%    /websockets/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /websockets mights load a list of websocket objects
%% /websockets/123 might load the websocket object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_websockets(Context, cb_context:req_verb(Context)).
validate(Context, Id) ->
    validate_websocket(Context, Id, cb_context:req_verb(Context)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate_req(cb_context:context(), http_method(), req_nouns()) -> {'true', cb_context:context()} |'false'.
authenticate_req(Context, ?HTTP_GET, [{<<"websockets">>, []}]) ->
    lager:debug("authenticating request"),
    {'true', Context};
authenticate_req(_Context, _Verb, _Nouns) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize_req(http_method(), req_nouns()) -> boolean().
authorize_req(?HTTP_GET, [{<<"websockets">>, []}]) ->
    lager:debug("authorizing request"),
    'true';
authorize_req(_Verb, _Nouns) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load running web sockets
%% @end
%%--------------------------------------------------------------------
-spec validate_websockets(cb_context:context(), http_method()) -> cb_context:context().
validate_websockets(Context, ?HTTP_GET) ->
    case cb_context:req_nouns(Context) of
        [{<<"websockets">>, []}] -> summary_available(Context);
        _Nouns -> summary(Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a web socket info
%% @end
%%--------------------------------------------------------------------
-spec validate_websocket(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_websocket(Context, Id, ?HTTP_GET) ->
    read(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary_available(cb_context:context()) -> cb_context:context().
summary_available(Context) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                ,{fun cb_context:set_resp_data/2, ?AVAILABLE}
                                ]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    blackhole_req(Context, [{<<"Account-ID">>, cb_context:account_id(Context)}]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    blackhole_req(Context, [{<<"Socket-ID">>, Id}]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec blackhole_req(cb_context:context(), kz_proplist()) -> cb_context:context().
blackhole_req(Context, Props) ->
    Req = [{<<"Msg-ID">>, cb_context:req_id(Context)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kapps_util:amqp_pool_collect(Req ++ Props
                                     ,fun kapi_blackhole:publish_get_req/1
                                     ,{'blackhole', 'true'}
                                     )
    of
        {'error', _R} ->
            lager:error("could not reach blackhole sockets tracking: ~p", [_R]),
            crossbar_util:response('error', <<"could not reach blackhole sockets tracking">>, Context);
        {_OK, JObjs} ->
            blackhole_resp(Context, JObjs)

    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec blackhole_resp(cb_context:context(), kz_json:objects()) -> cb_context:context().
-spec blackhole_resp(cb_context:context(), kz_json:objects(), any()) -> cb_context:context().
blackhole_resp(Context, JObjs) ->
    blackhole_resp(Context, JObjs, []).

blackhole_resp(Context, [], RespData) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                ,{fun cb_context:set_resp_data/2, RespData}
                                ]);
blackhole_resp(Context, [JObj|JObjs], RespData) ->
    case kz_json:get_value(<<"Data">>, JObj) of
        'undefined' ->
            blackhole_resp(Context, JObjs, RespData);
        Data when is_list(Data) ->
            blackhole_resp(Context, JObjs, RespData ++ Data);
        Data ->
            blackhole_resp(Context, JObjs, [Data|RespData])
    end.
