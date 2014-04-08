%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz INC
%%% @doc
%%%
%%% Fetch and accumulate all stats about the system
%%%
%%% /system_stats/hangups [/{hangup_cause}] - stats about hangup causes
%%%   - you can also hit /accounts/{account_id}/system_stats for specific
%%%     account stats
%%% /system_stats/compactor - stats about the compactor
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
         ,post/2
        ]).

-include("../crossbar.hrl").

-define(PATH_TOKEN_HANGUPS, <<"hangups">>).
-define(PATH_TOKEN_COMPACTOR, <<"compactor">>).

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
    _ = crossbar_bindings:bind(<<"*.execute.post.system_stats">>, ?MODULE, 'post'),
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
authorize(Context, ?PATH_TOKEN_COMPACTOR) ->
    cb_modules_util:is_superduper_admin(Context);
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
allowed_methods(?PATH_TOKEN_COMPACTOR) ->
    [?HTTP_GET, ?HTTP_POST];
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
-spec resource_exists() -> 'false'.
-spec resource_exists(path_token()) -> boolean().
-spec resource_exists(path_token(), path_token()) -> boolean().
resource_exists() -> 'false'.

resource_exists(?PATH_TOKEN_HANGUPS) -> 'true';
resource_exists(?PATH_TOKEN_COMPACTOR) -> 'true';
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
content_types_provided(Context, ?PATH_TOKEN_COMPACTOR) ->
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
    fetch_hangups(Context, <<"*">>);
validate(Context, ?PATH_TOKEN_COMPACTOR) ->
    validate_compactor(Context, cb_context:req_verb(Context)).

-spec validate_compactor(cb_context:context(), http_method()) -> cb_context:context().
validate_compactor(Context, ?HTTP_GET) ->
    case couch_compactor_fsm:status() of
        {'ok', 'not_running'} ->
            crossbar_util:response(<<"not running">>, Context);
        {'ok', 'ready'} ->
            crossbar_util:response(<<"ready">>, Context);
        {'ok', Props} ->
            JObj = compactor_props_to_json(Props),
            crossbar_util:response(JObj, Context)
    end;
validate_compactor(Context, ?HTTP_POST) ->
    case cb_context:req_value(Context, <<"command">>) of
        <<"compact">> ->
            validate_compactor_compact(Context);
        <<"compact_node">> ->
            validate_compactor_compact_node(Context);
        <<"compact_db">> ->
            validate_compactor_compact_db(Context);
        _ ->
            cb_context:add_validation_error(<<"command">>, <<"required">>, <<"invalid command">>, Context)
    end.

-spec validate_compactor_compact(cb_context:context()) -> cb_context:context().
validate_compactor_compact(Context) ->
    cb_context:set_resp_status(
      cb_context:store(Context
                       ,'compactor_fun'
                       ,{'compact', []}
                      )
      ,'success'
     ).

-spec validate_compactor_compact_node(cb_context:context()) -> cb_context:context().
validate_compactor_compact_node(Context) ->
    Node = cb_context:req_value(Context, <<"node">>),
    cb_context:set_resp_status(
      cb_context:store(Context
                       ,'compactor_fun'
                       ,{'compact_node', [Node]}
                      )
      ,'success'
     ).

-spec validate_compactor_compact_db(cb_context:context()) ->
                                           cb_context:context().
-spec validate_compactor_compact_db(cb_context:context(), api_binary(), api_binary()) ->
                                           cb_context:context().
validate_compactor_compact_db(Context) ->
    validate_compactor_compact_db(Context
                                  ,cb_context:req_value(Context, <<"db">>)
                                  ,cb_context:req_value(Context, <<"node">>)
                                 ).
validate_compactor_compact_db(Context, 'undefined', _Node) ->
    cb_context:add_validation_error(<<"db">>, <<"required">>, <<"db is required">>, Context);
validate_compactor_compact_db(Context, Db, 'undefined') ->
    cb_context:set_resp_status(
      cb_context:store(Context
                       ,'compactor_fun'
                       ,{'compact_db', [Db]}
                      )
      ,'success'
     );
validate_compactor_compact_db(Context, Db, Node) ->
    cb_context:set_resp_status(
      cb_context:store(Context
                       ,'compactor_fun'
                       ,{'compact_db', [Node, Db]}
                      )
      ,'success'
     ).

-spec validate(cb_context:context(), ne_binary(), path_token()) -> cb_context:context().
validate(Context, ?PATH_TOKEN_HANGUPS, HangupCause) ->
    fetch_hangups(Context, HangupCause).

-spec fetch_hangups(cb_context:context(), ne_binary()) -> cb_context:context().
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
            lager:debug("err: ~p", [_E]),
            Context;
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

-spec compactor_props_to_json(wh_proplist()) -> wh_json:objects().
compactor_props_to_json(Props) ->
        wh_json:from_list([compactor_prop_to_json(K, V) || {K, V} <- Props]).

-spec compactor_prop_to_json(atom(), term()) -> {ne_binary(), term()}.
compactor_prop_to_json('nodes_left', N) ->
    {<<"nodes_left">>, N};
compactor_prop_to_json('dbs_left', N) ->
    {<<"dbs_left">>, N};
compactor_prop_to_json('queued_jobs', 'none') ->
    {<<"queued_jobs">>, <<"none">>};
compactor_prop_to_json('queued_jobs', Jobs) ->
    {<<"queued_jobs">>, queued_jobs_to_json(Jobs)};
compactor_prop_to_json('wait_left', N) ->
    {<<"wait_left_ms">>, N};
compactor_prop_to_json('start_time', Start) ->
    {<<"start_timestamp">>, calendar:datetime_to_gregorian_seconds(
                              calendar:now_to_datetime(Start)
                             )};
compactor_prop_to_json('elapsed_s', Elapsed) ->
    {<<"elapsed_s">>, Elapsed};
compactor_prop_to_json(K, V) ->
    {wh_util:to_binary(K), wh_util:to_binary(V)}.

-spec queued_jobs_to_json(list()) -> wh_json:objects().
queued_jobs_to_json(Jobs) ->
    [queued_job_to_json(Job) || Job <- Jobs].

-spec queued_job_to_json(wh_proplist()) -> wh_json:object().
queued_job_to_json(Job) ->
    [Fun | Args] = tuple_to_list(props:get_value('job', Job)),
    wh_json:from_list([{<<"job">>, Fun}
                       ,{<<"args">>, Args}
                      ]).

-spec post(cb_context:context(), ne_binary()) -> cb_context:context().
post(Context, ?PATH_TOKEN_COMPACTOR) ->
    {Fun, Args} = cb_context:fetch(Context, 'compactor_fun'),
    lager:debug("applying couch_compactor_fsm:~s(~p)", [Fun, Args]),
    _Resp = apply('couch_compactor_fsm', Fun, Args),
    crossbar_util:response_202(<<"queued job">>, Context).
