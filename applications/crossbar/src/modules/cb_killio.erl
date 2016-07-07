%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, 2600Hz
%%% @doc
%%% /call
%%%   methods: PUT, POST
%%%   create a call using the 'to' and 'from' passed in the payload
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_killio).

-export([init/0
        ,allowed_methods/1
        ,resource_exists/1
        ,validate/2
        ,put/2
        ,post/2
        ]).

-include("crossbar.hrl").

-define(PVT_TYPE, <<"skel">>).
-define(PVT_FUNS, [fun add_pvt_type/2]).
-define(CB_LIST, <<"killio/crossbar_listing">>).

-define(TOKEN_CALL, <<"call">>).

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.killio">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.killio">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.killio">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.killio">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.killio">>, ?MODULE, 'post').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?TOKEN_CALL) ->
    [?HTTP_PUT, ?HTTP_POST].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /killio => []
%%    /killio/foo => [<<"foo">>]
%%    /killio/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(path_token()) -> 'true'.
resource_exists(?TOKEN_CALL) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /killio mights load a list of skel objects
%% /killio/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), http_method(), path_token()) -> cb_context:context().
validate(Context, PathToken) ->
    validate(Context, cb_context:req_verb(Context), PathToken).

validate(Context, ?HTTP_PUT, ?TOKEN_CALL) -> setup_call(Context);
validate(Context, ?HTTP_POST, ?TOKEN_CALL) -> setup_call(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, ?TOKEN_CALL) ->
    start_call(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?TOKEN_CALL) ->
    start_call(Context).

setup_call(Context) ->
    cb_context:set_resp_status(Context, 'success').

start_call(Context) ->
    cb_context:set_resp_status(Context, 'success').
