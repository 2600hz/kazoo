%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz INC
%%% @doc
%%%
%%% Fetch and accumulate all stats about the system
%%%
%%% /system_stats/hangups [/{hangup_cause}] - stats about hangup causes
%%%   - you can also hit /accounts/{account_id}/system_stats for specific
%%%     account stats
%%%
%%% @end
%%% @contributors:
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_system_stats).

-export([init/0
         ,authorize/1, authorize/2, authorize/3
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,content_types_provided/1, content_types_provided/2, content_types_provided/3
         ,validate/1, validate/2, validate/3
        ]).

-include("../crossbar.hrl").

-define(PATH_TOKEN_HANGUPS, <<"hangups">>).

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.system_stats">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.system_stats">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.system_stats">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.system_stats">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> 'false'.
-spec authorize(cb_context:context(), path_token()) -> boolean().
-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(_Context) ->
    'false'.

authorize(Context, ?PATH_TOKEN_HANGUPS) ->
    maybe_authorize_by_account(Context, cb_context:account_id(Context));
authorize(_Context, _) ->
    'false'.

authorize(Context, ?PATH_TOKEN_HANGUPS, _) ->
    maybe_authorize_by_account(Context, cb_context:account_id(Context));
authorize(_Context, _, _) ->
    'false'.

-spec maybe_authorize_by_account(cb_context:context(), api_binary()) -> boolean().
maybe_authorize_by_account(Context, 'undefined') ->
    cb_modules_util:is_superduper_admin(Context);
maybe_authorize_by_account(_Context, _AccountId) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [].

allowed_methods(?PATH_TOKEN_HANGUPS) ->
    [?HTTP_GET];
allowed_methods(_) ->
    [].

allowed_methods(?PATH_TOKEN_HANGUPS, _HangupCause) ->
    [?HTTP_GET];
allowed_methods(_, _) ->
    [].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /system_stats => []
%%    /system_stats/foo => [<<"foo">>]
%%    /system_stats/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> boolean().
-spec resource_exists(path_token(), path_token()) -> boolean().
resource_exists() -> 'false'.

resource_exists(?PATH_TOKEN_HANGUPS) -> 'true';
resource_exists(_) -> 'false'.

resource_exists(?PATH_TOKEN_HANGUPS, _HangupCause) -> 'true';
resource_exists(_, _) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% What content-types will the module be using to respond (matched against
%% client's accept header)
%% Of the form {atom, [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context) ->
    Context.
content_types_provided(Context, ?PATH_TOKEN_HANGUPS) ->
    Context;
content_types_provided(Context, _) ->
    Context.

content_types_provided(Context, ?PATH_TOKEN_HANGUPS, _HangupCause) ->
    Context;
content_types_provided(Context, _, _) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /system_stats mights load a list of system_stat objects
%% /system_stats/123 might load the system_stat object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    Context.

validate(Context, ?PATH_TOKEN_HANGUPS) ->
    fetch_hangups(Context, <<"*">>).

validate(Context, ?PATH_TOKEN_HANGUPS, HangupCause) ->
    fetch_hangups(Context, HangupCause).

fetch_hangups(Context, HangupCause) ->
    lager:debug("req ~s", [HangupCause]),
    Req = props:filter_undefined(
            [{<<"Hangup-Cause">>, wh_util:to_upper_binary(HangupCause)}
             ,{<<"Account-ID">>, cb_context:account_id(Context)}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    case whapps_util:amqp_pool_collect(Req
                                       ,fun wapi_hangups:publish_query_req/1
                                       ,<<"hangups">>
                                       )
    of
        {'ok', Stats} ->
            crossbar_util:response(filter_stats(Stats), Context);
        {'error', _E} ->
            lager:debug("err: ~p", [_E]);
        {'timeout', Collected} ->
            lager:debug("partial response collected"),
            crossbar_util:response(filter_stats(Collected), Context)
    end.

-spec filter_stats(wh_json:objects()) -> wh_json:objects().
filter_stats(Stats) ->
    [ wh_json:set_value(
        <<"node">>
        ,wh_json:get_value(<<"Node">>, Stat)
        ,wh_api:remove_defaults(Stat)
       )
      || Stat <- Stats
    ].
