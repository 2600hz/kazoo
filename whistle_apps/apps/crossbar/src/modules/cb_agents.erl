%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% CRUD for call queues
%%% /agents
%%%   GET: list all known agents and their queues
%%%
%%% /agents/stats
%%%   GET: stats for all agents
%%% /agents/stats/realtime
%%%   GET: stats for all agents - realtime
%%%
%%% /agents/AID
%%%   GET: agent details
%%%
%%% /agents/AID/stats
%%%   GET: stats on the agent
%%% /agents/AID/stats/realtime
%%%   GET: stats on the agent - realtime
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_agents).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,validate/1, validate/2, validate/3, validate/4
        ]).

-include("include/crossbar.hrl").

-define(CB_LIST, <<"agents/crossbar_listing">>).
-define(STATS_PATH_TOKEN, <<"stats">>).
-define(REALTIME_PATH_TOKEN, <<"realtime">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init/0 :: () -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.agents">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.agents">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.agents">>, ?MODULE, validate).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods() | [].
-spec allowed_methods/1 :: (path_token()) -> http_methods() | [].
-spec allowed_methods/2 :: (path_token(), path_token()) -> http_methods() | [].
-spec allowed_methods/3 :: (path_token(), path_token(), path_token()) -> http_methods() | [].
allowed_methods() ->
    ['GET'].
allowed_methods(_) ->
    ['GET'].
allowed_methods(_Id, ?STATS_PATH_TOKEN) ->
    ['GET'];
allowed_methods(?STATS_PATH_TOKEN, ?REALTIME_PATH_TOKEN) ->
    ['GET'].
allowed_methods(_Id, ?STATS_PATH_TOKEN, ?REALTIME_PATH_TOKEN) ->
    ['GET'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /agents => []
%%    /agents/foo => [<<"foo">>]
%%    /agents/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
-spec resource_exists/2 :: (path_token(), path_token()) -> 'true'.
-spec resource_exists/3 :: (path_token(), path_token(), path_token()) -> 'true'.
resource_exists() -> true.
resource_exists(_) -> true.
resource_exists(_, ?STATS_PATH_TOKEN) -> true;
resource_exists(?STATS_PATH_TOKEN, ?REALTIME_PATH_TOKEN) -> true.
resource_exists(_, ?STATS_PATH_TOKEN, ?REALTIME_PATH_TOKEN) -> true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /agents mights load a list of agent objects
%% /agents/123 might load the agent object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec validate/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
-spec validate/4 :: (#cb_context{}, path_token(), path_token(), path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    summary(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, ?STATS_PATH_TOKEN) ->
    fetch_all_agent_stats(Context);
validate(#cb_context{req_verb = <<"get">>}=Context, Id) ->
    read(Id, Context).

validate(#cb_context{req_verb = <<"get">>}=Context, ?STATS_PATH_TOKEN, ?REALTIME_PATH_TOKEN) ->
    fetch_all_agent_stats(Context, ?REALTIME_PATH_TOKEN);
validate(#cb_context{req_verb = <<"get">>}=Context, Id, ?STATS_PATH_TOKEN) ->
    %% read agent stats with id=Id
    fetch_agent_stats(Id, Context).

validate(#cb_context{req_verb = <<"get">>}=Context, Id, ?STATS_PATH_TOKEN, ?REALTIME_PATH_TOKEN) ->
    fetch_agent_stats(Id, Context, ?REALTIME_PATH_TOKEN).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

fetch_all_agent_stats(Context) ->
    fetch_all_agent_stats(Context, history).
fetch_all_agent_stats(Context, history) ->
    {Today, _} = calendar:universal_time(),
    From = calendar:datetime_to_gregorian_seconds({Today, {0,0,0}}),

    crossbar_doc:load_view(<<"acdc_stats/stats_per_agent_by_time">>
                           ,[{startkey, [wh_util:current_tstamp(), <<"\ufff0">>]}
                             ,{endkey, [From, <<>>]}
                             ,descending
                            ]
                           ,Context
                           ,fun normalize_agent_results/2
                          );
fetch_all_agent_stats(#cb_context{account_id=AcctId}=Context, ?REALTIME_PATH_TOKEN) ->
    Req = [{<<"Account-ID">>, AcctId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_request(Req
                                       ,fun wapi_acdc_agent:publish_stats_req/1
                                       ,fun wapi_acdc_agent:stats_resp_v/1
                                       ,2000
                                      ) of
        {ok, Resp} ->
            lager:debug("stats req responded: ~p", [Resp]),
            Resp1 = strip_api_fields(wh_json:normalize(Resp)),
            Context#cb_context{resp_status=success
                               ,resp_data=total_up_stats(Resp1)
                               ,doc=Resp1
                              };
        {error, _E} ->
            lager:debug("stats req failed: ~p", [_E]),
            Context
    end.

total_up_stats(Stats) ->
    AgentsJObj = wh_json:get_value([<<"current_stats">>], Stats, wh_json:new()),
    {Total, AgentsJObj1} = wh_json:foldl(fun(AgentId, AgentStats, {Tot, ByAgent}) ->
                                                 L = length(wh_json:to_proplist(wh_json:get_value(<<"calls_handled">>, AgentStats, wh_json:new()))),
                                              {Tot + L, wh_json:set_value([AgentId, <<"calls_this_hour">>], L, ByAgent)}
                                         end, {0, AgentsJObj}, AgentsJObj),
    wh_json:set_values([{<<"current_stats">>, AgentsJObj1}
                        ,{<<"calls_this_hour">>, Total}
                       ], Stats).
        
fetch_agent_stats(Id, Context) ->
    {Today, _} = calendar:universal_time(),
    From = calendar:datetime_to_gregorian_seconds({Today, {0,0,0}}),

    crossbar_doc:load_view(<<"acdc_stats/stats_per_agent">>
                               ,[{startkey, [Id, wh_util:current_tstamp()]}
                                 ,{endkey, [Id, From]}
                                 ,descending
                                ]
                           ,Context
                           ,fun normalize_agent_results/2
                          ).
fetch_agent_stats(Id, #cb_context{account_id=AcctId}=Context, ?REALTIME_PATH_TOKEN) ->
    Req = [{<<"Account-ID">>, AcctId}
           ,{<<"Agent-ID">>, Id}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_request(Req
                                       ,fun wapi_acdc_agent:publish_stats_req/1
                                       ,fun wapi_acdc_agent:stats_resp_v/1
                                       ,2000
                                      ) of
        {ok, Resp} ->
            Resp1 = strip_api_fields(wh_json:normalize(Resp)),
            Context#cb_context{resp_status=success
                               ,resp_data=Resp1
                               ,doc=Resp1
                              };
        {error, _} -> Context
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary/1 :: (#cb_context{}) -> #cb_context{}.
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:set_value(<<"id">>
                       ,wh_json:get_value(<<"id">>, JObj)
                       , wh_json:get_value(<<"value">>, JObj)
                      )
     | Acc
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_agent_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_agent_results(JObj, Acc) ->
    [begin
         AID = agent_key(wh_json:get_value(<<"key">>, JObj)),
         wh_json:set_value(<<"agent_id">>
                           ,AID
                           ,wh_json:get_value(<<"value">>, JObj)
                          )
     end
     | Acc
    ].

agent_key([AID, TStamp]) when is_integer(TStamp) -> AID;
agent_key([TStamp, AID]) when is_integer(TStamp) -> AID.

strip_api_fields(JObj) ->
    Strip = [<<"event_name">>, <<"event_category">>
                 ,<<"app_name">>, <<"app_version">>
                 ,<<"node">>, <<"msg_id">>, <<"server_id">>
            ],
    wh_json:filter(fun({K,_}) -> not lists:member(K, Strip) end, JObj).
