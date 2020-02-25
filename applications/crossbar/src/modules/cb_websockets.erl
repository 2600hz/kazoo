%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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

-define(TO_JSON(Binding, Event)
       ,kz_json:from_list([{<<"binding">>, Binding}
                          ,{<<"event">>, Event}
                          ])
       ).

-define(AVAILABLE
       ,kapps_config:get_json(<<"blackhole">>, <<"bindings">>)
       ).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate.websockets">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize.websockets">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.websockets">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.websockets">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.websockets">>, ?MODULE, 'validate').

%%------------------------------------------------------------------------------
%% @doc Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%------------------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> {'true', cb_context:context()} | 'false'.
authenticate(Context) ->
    authenticate_req(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

%%------------------------------------------------------------------------------
%% @doc Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_req(cb_context:req_verb(Context), cb_context:req_nouns(Context)).

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_SocketId) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /websockets => []
%%    /websockets/foo => [<<"foo">>]
%%    /websockets/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /websockets might load a list of websocket objects
%% /websockets/123 might load the websocket object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_websockets(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate_websocket(Context, Id, cb_context:req_verb(Context)).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authenticate_req(cb_context:context(), http_method(), req_nouns()) -> {'true', cb_context:context()} |'false'.
authenticate_req(Context, ?HTTP_GET, [{<<"websockets">>, []}]) ->
    lager:debug("authenticating request"),
    {'true', Context};
authenticate_req(_Context, _Verb, _Nouns) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authorize_req(http_method(), req_nouns()) -> boolean().
authorize_req(?HTTP_GET, [{<<"websockets">>, []}]) ->
    lager:debug("authorizing request"),
    'true';
authorize_req(_Verb, _Nouns) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Load running web sockets
%% @end
%%------------------------------------------------------------------------------
-spec validate_websockets(cb_context:context(), http_method()) -> cb_context:context().
validate_websockets(Context, ?HTTP_GET) ->
    case cb_context:req_nouns(Context) of
        [{<<"websockets">>, []}] -> summary_available(Context);
        _Nouns -> summary(Context)
    end.

%%------------------------------------------------------------------------------
%% @doc Load a web socket info
%% @end
%%------------------------------------------------------------------------------
-spec validate_websocket(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_websocket(Context, Id, ?HTTP_GET) ->
    read(Id, Context).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary_available(cb_context:context()) -> cb_context:context().
summary_available(Context) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                ,{fun cb_context:set_resp_data/2, ?AVAILABLE}
                                ]).


%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    websockets_req(Context, [{<<"Auth-Account-ID">>, cb_context:auth_account_id(Context)}]).

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec read(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    ReadContext = websockets_req(Context, [{<<"Socket-ID">>, Id}]),
    read_resp(ReadContext, cb_context:resp_status(ReadContext)).

read_resp(Context, 'success') ->
    read_success_resp(Context, cb_context:resp_data(Context));
read_resp(Context, _Error) -> Context.

read_success_resp(Context, RespData) when is_list(RespData) ->
    cb_context:set_resp_data(Context, kz_json:merge(RespData));
read_success_resp(Context, _RespData) -> Context.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec websockets_req(cb_context:context(), kz_term:proplist()) -> cb_context:context().
websockets_req(Context, Props) ->
    Req = [{<<"Msg-ID">>, cb_context:req_id(Context)}
          ,{<<"Account-ID">>, cb_context:account_id(Context)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req ++ Props
                                    ,fun kapi_websockets:publish_get_req/1
                                    ,{'blackhole', 'true'}
                                    )
    of
        {'error', _R} ->
            lager:error("could not reach websockets tracking: ~p", [_R]),
            crossbar_util:response('error', <<"could not reach websockets tracking">>, Context);
        {_OK, JObjs} ->
            websockets_resp(Context, JObjs)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec websockets_resp(cb_context:context(), kz_json:objects()) -> cb_context:context().
websockets_resp(Context, JObjs) ->
    websockets_resp(Context, JObjs, []).

-spec websockets_resp(cb_context:context(), kz_json:objects(), any()) -> cb_context:context().
websockets_resp(Context, [], RespData) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                ,{fun cb_context:set_resp_data/2, RespData}
                                ]);
websockets_resp(Context, [JObj|JObjs], RespData) ->
    case kz_json:get_value(<<"Data">>, JObj) of
        'undefined' ->
            websockets_resp(Context, JObjs, RespData);
        Data when is_list(Data) ->
            websockets_resp(Context, JObjs, RespData ++ Data);
        Data ->
            websockets_resp(Context, JObjs, [Data|RespData])
    end.
