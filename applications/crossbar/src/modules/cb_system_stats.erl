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

validate_compactor(Context, ?HTTP_GET) ->
    case couch_compactor_fsm:status() of
        {'ok', 'not_running'} ->
            crossbar_util:response(<<"not running">>, Context);
        {'ok', 'ready'} ->
            crossbar_util:response(<<"ready">>, Context);
        {'ok', Props} ->
            lager:debug("compactor props: ~p", [Props]),
            JObj = compactor_props_to_json(Props),
            lager:debug("jobj: ~p", [JObj]),
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
            cb_context:add_validation_error(<<"command">>, <<"required">>, <<"db is required">>, Context)
    end.

validate_compactor_compact(Context) ->
    cb_context:resp_status(
      cb_context:store(Context
                       ,'compactor_fun'
                       ,fun couch_compactor_fsm:compact/0
                      )
      ,'success'
     ).

validate_compactor_compact_node(Context) ->
    Node = cb_context:req_value(Context, <<"node">>),
    cb_context:resp_status(
      cb_context:store(Context
                       ,'compactor_fun'
                       ,fun() -> couch_compactor_fsm:compact_node(Node) end
                      )
      ,'success'
     ).

validate_compactor_compact_db(Context) ->
    case compact_db_fun_builder(Context) of
        'false' ->
            cb_context:add_validation_error(<<"db">>, <<"required">>, <<"db is required">>, Context);
        Fun ->
            cb_context:resp_status(
              cb_context:store(Context
                               ,'compactor_fun'
                               ,Fun
                              )
              ,'success'
             )
    end.

-spec compact_db_fun_builder(cb_context:context()) -> 'false' |
                                                      fun().
compact_db_fun_builder(Context) ->
    compact_db_fun_builder(cb_context:req_value(Context, <<"node">>)
                           ,cb_context:req_value(Context, <<"db">>)
                          ).
compact_db_fun_builder('undefined', 'undefined') ->
    'false';
compact_db_fun_builder('undefined', Db) ->
    fun() -> couch_compactor_fsm:compact_db(Db) end;
compact_db_fun_builder(Node, Db) ->
    fun() -> couch_compactor_fsm:compact_db(Node, Db) end.

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

-spec compactor_props_to_json(wh_proplist()) -> wh_json:objects().
compactor_props_to_json(Props) ->
        wh_json:from_list([compactor_prop_to_json(K, V) || {K, V} <- Props]).

-spec compactor_prop_to_json(atom(), term()) -> {ne_binary(), term()}.
compactor_prop_to_json('nodes_left', N) ->
    {<<"nodes_left">>, N};
compactor_prop_to_json('dbs_left', N) ->
    {<<"dbs_left">>, N};
compactor_prop_to_json('queued_jobs', Jobs) ->
    {<<"queued_jobs">>, queued_jobs_to_json(Jobs)};
compactor_prop_to_json('wait_left', N) ->
    {<<"wait_left">>, N};
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
