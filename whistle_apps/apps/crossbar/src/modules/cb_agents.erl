%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% CRUD for call queues
%%% /agents
%%%   GET: list all known agents and their queues
%%%
%%% /agents/stats
%%%   GET: stats for all agents for the last hour
%%% /agents/AID
%%%   GET: agent details
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_agents).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,content_types_provided/2
         ,validate/1, validate/2
        ]).

-include("include/crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".queues">>).

-define(STAT_TIMESTAMP_PROCESSED, <<"finished_with_agent">>).
-define(STAT_TIMESTAMP_HANDLING, <<"connected_with_agent">>).
-define(STAT_TIMESTAMP_ABANDONED, <<"caller_abandoned_queue">>).
-define(STAT_TIMESTAMP_WAITING, <<"caller_entered_queue">>).
-define(STAT_AGENTS_MISSED, <<"missed">>).

-define(FORMAT_COMPRESSED, <<"compressed">>).
-define(FORMAT_VERBOSE, <<"verbose">>).

-define(STAT_TIMESTAMP_KEYS, [?STAT_TIMESTAMP_PROCESSED
                              ,?STAT_TIMESTAMP_HANDLING
                              ,?STAT_TIMESTAMP_ABANDONED
                              ,?STAT_TIMESTAMP_WAITING
                             ]).


-define(CB_LIST, <<"agents/crossbar_listing">>).
-define(STATS_PATH_TOKEN, <<"stats">>).

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
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_provided.agents">>, ?MODULE, content_types_provided),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.agents">>, ?MODULE, validate).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
allowed_methods() ->
    ['GET'].
allowed_methods(_) ->
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
resource_exists() -> true.
resource_exists(_) -> true.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided/2 :: (cb_context:context(), path_token()) -> cb_context:context().
content_types_provided(#cb_context{}=Context, ?STATS_PATH_TOKEN) ->
    case cb_context:req_value(Context, <<"format">>, ?FORMAT_COMPRESSED) of
        ?FORMAT_VERBOSE ->
            CTPs = [{to_json, [{<<"application">>, <<"json">>}]}
                    ,{to_csv, [{<<"application">>, <<"octet-stream">>}]}
                   ],
            cb_context:add_content_types_provided(Context, CTPs);
        ?FORMAT_COMPRESSED ->
            CTPs = [{to_json, [{<<"application">>, <<"json">>}]}],
            cb_context:add_content_types_provided(Context, CTPs)
    end.

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
-spec validate/1 :: (cb_context:context()) -> cb_context:context().
-spec validate/2 :: (cb_context:context(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    summary(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, ?STATS_PATH_TOKEN) ->
    fetch_all_agent_stats(Context);
validate(#cb_context{req_verb = <<"get">>}=Context, Id) ->
    read(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read/2 :: (path_token(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

-spec fetch_all_agent_stats/1 :: (cb_context:context()) -> cb_context:context().
fetch_all_agent_stats(Context) ->
    AcctId = cb_context:account_id(Context),

    MaxRange = whapps_config:get_integer(?MOD_CONFIG_CAT, <<"maximum_range">>, 3600),

    To = wh_util:to_integer(cb_context:req_value(Context, <<"end_range">>, wh_util:current_tstamp())),
    MaxFrom = To - MaxRange,

    From = case wh_util:to_integer(cb_context:req_value(Context, <<"start_range">>, MaxFrom)) of
               F when F > To -> MaxFrom; % created_from comes after created_to
               F when F < MaxFrom -> MaxFrom; % range is too large
               F -> F
           end,

    Opts = [{startkey, [To, AcctId]}
            ,{endkey, [From, AcctId]}
            ,include_docs
            ,descending
           ],

    case cb_context:req_value(Context, <<"format">>, ?FORMAT_COMPRESSED) of
        ?FORMAT_COMPRESSED ->
            Context1 = crossbar_doc:load_view(<<"agent_stats/call_log">>
                                              ,Opts
                                              ,cb_context:set_account_db(Context, acdc_stats:db_name(AcctId)) 
                                              ,fun normalize_agent_stats/2
                                             ),
            maybe_compress_stats(Context1, From, To);
        ?FORMAT_VERBOSE ->
            crossbar_doc:load_view(<<"agent_stats/call_log">>
                                   ,Opts
                                   ,cb_context:set_account_db(Context, acdc_stats:db_name(AcctId))
                                   ,fun normalize_agent_stats/2
                                  );
        _Format ->
            lager:debug("unrecognized stats format: ~s", [_Format]),
            cb_context:add_validation_error(<<"format">>, <<"enum">>, <<"enum:Value not found in enumerated list">>, Context)
    end.

maybe_compress_stats(Context, From, To) ->
    case cb_context:resp_status(Context) of
        'success' ->
            Compressed = compress_stats(cb_context:doc(Context)
                                        ,cb_context:account_id(Context)
                                        ,{wh_json:new(), wh_json:new(), wh_json:new()}
                                       ),
            crossbar_util:response(
              wh_json:set_values([{<<"start_range">>, From}
                                  ,{<<"end_range">>, To}
                                  ,{<<"current_timestamp">>, wh_util:current_tstamp()}
                                 ]
                                 ,Compressed)
              ,Context);
        _S ->
            lager:debug("failed to load stats"),
            Context
    end.

compress_stats(undefined, _, {Compressed, _, _}) -> Compressed;
compress_stats([], AcctId, {Compressed, Global, PerAgent}) -> accumulate_stats(AcctId, Compressed, Global, PerAgent);
compress_stats([Stat|Stats], AcctId, {_,_,_}=Res) ->
    compress_stats(Stats, AcctId, add_stat(Stat, Res)).

-spec accumulate_stats/4 :: (ne_binary(), wh_json:object(), wh_json:object(), wh_json:object()) ->
                                    wh_json:object().
accumulate_stats(AcctId, Compressed, Global, PerAgent) ->
    wh_json:from_list([{<<"totals">>, wh_json:foldl(fun fold_queue_totals/3, wh_json:new(), Global)}
                       ,{<<"agents">>, wh_json:merge_recursive(
                                         wh_json:foldl(fun(AgentId, Queues, Acc) ->
                                                               fold_agent_totals(AgentId, Queues, Acc, AcctId)
                                                       end, wh_json:new(), PerAgent)
                                         ,wh_json:map(fun accumulate_agent_stats/2, Compressed)
                                        )}
                      ]).

fold_queue_totals(_QueueId, Calls, Acc) ->
    wh_json:foldl(fun fold_call_totals/3, Acc, Calls).

fold_call_totals(_CallId, Stats, Acc) ->
    AccTalkTime = wh_json:get_value(<<"call_time">>, Acc, 0),
    AccTotalCalls = wh_json:get_value(<<"total_calls">>, Acc, 0),
    AccAgentsMissed = wh_json:get_value(<<"missed_calls">>, Acc, 0),

    Set = [{<<"total_calls">>, AccTotalCalls + 1}
           ,{<<"missed_calls">>, AccAgentsMissed + wh_json:get_integer_value(?STAT_AGENTS_MISSED, Stats, 0)}
          ],

    Set1 = case call_time(Stats) of
               undefined -> Set;
               TalkTime -> [{<<"call_time">>, AccTalkTime + TalkTime} | Set]
           end,
    wh_json:set_values(Set1, Acc).

fold_agent_totals(AgentId, Queues, Acc, AcctId) ->
    QueueTotals = lists:map(fun({QueueId, Calls}) ->
                                    {[AgentId, <<"queues">>, QueueId, <<"totals">>]
                                     ,wh_json:foldl(fun fold_call_totals/3, wh_json:new(), Calls)
                                    }
                            end, wh_json:to_proplist(Queues)
                           ),

    AgentTotals = wh_json:foldl(fun fold_queue_totals/3, wh_json:new(), Queues),

    wh_json:set_values([{[AgentId, <<"totals">>], AgentTotals}
                        ,{AgentId, agent_status(AcctId, AgentId)}
                        | QueueTotals
                       ], Acc).

agent_status(AcctId, AgentId) ->
    case acdc_util:agent_status(AcctId, AgentId, true) of
        Status when is_binary(Status) ->
            wh_json:set_value(<<"current_status">>, Status, wh_json:new());
        StatusJObj ->
            complex_agent_status(StatusJObj, wh_json:get_value(<<"status">>, StatusJObj))
    end.

complex_agent_status(StatusJObj, <<"wrapup">> = Status) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"current_status">>, Status}
         ,{<<"wait_time">>, wh_json:get_value(<<"wait_time">>, StatusJObj)}
         ,{<<"started">>, wh_json:get_value(<<"timestamp">>, StatusJObj)}
        ]));
complex_agent_status(StatusJObj, <<"paused">> = Status) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"current_status">>, Status}
         ,{<<"wait_time">>, wh_json:get_value(<<"wait_time">>, StatusJObj)}
         ,{<<"started">>, wh_json:get_value(<<"timestamp">>, StatusJObj)}
        ]));
complex_agent_status(StatusJObj, <<"logout">> = Status) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"current_status">>, Status}
         ,{<<"logged_out_timestamp">>, wh_json:get_value(<<"timestamp">>, StatusJObj)}
        ]));
complex_agent_status(_StatusJObj, Status) ->
    wh_json:set_value(<<"current_status">>, Status, wh_json:new()).

%% returns {AgentID: {queues: {QueueId: data}}}
accumulate_agent_stats(AgentId, Calls) ->
    {AgentId, wh_json:set_value(<<"queues">>
                                ,wh_json:map(fun accumulate_queue_stats/2, Calls)
                                ,wh_json:new()
                               )}.

accumulate_queue_stats(QueueId, Stats) ->
    AccStats = wh_json:foldl(fun fold_call_stats/3
                             ,wh_json:new()
                             ,wh_json:set_value(<<"call_time">>, call_time(Stats), Stats)
                            ),
    {QueueId, AccStats}.

fold_call_stats(_K, undefined, Acc) -> Acc;
fold_call_stats(K, V, Acc) ->
    case wh_json:is_json_object(V) of
        false -> wh_json:set_value(K, V, Acc);
        true ->
            CallIds = wh_json:get_value(<<"call_ids">>, Acc, []),
            wh_json:set_value(<<"call_ids">>, [K | CallIds], Acc)
    end.

call_time(Stats) ->
    case wh_json:get_integer_value(?STAT_TIMESTAMP_HANDLING, Stats) of
        undefined -> undefined;
        Conn -> call_time(Stats, Conn)
    end.
call_time(Stats, Conn) ->
    case wh_json:get_value(?STAT_TIMESTAMP_PROCESSED, Stats) of
        undefined -> undefined;
        Finished -> Finished - Conn
    end.

add_stat(Stat, {_Compressed, _Global, _PerAgent}=Res) ->
    add_stat(Stat, Res, wh_json:get_value(<<"status">>, Stat)).

add_stat(Stat, {Compressed, Global, PerAgent}, <<"handling">>) ->
    QID = wh_json:get_value(<<"queue_id">>, Stat),
    CID = wh_json:get_value(<<"call_id">>, Stat),
    AID = wh_json:get_value(<<"agent_id">>, Stat),

    TStamp = wh_json:get_value(<<"timestamp">>, Stat),

    {wh_json:set_values([{[AID, QID, CID, ?STAT_TIMESTAMP_HANDLING], TStamp}
                        ], Compressed)
     ,wh_json:set_value([QID, CID, ?STAT_TIMESTAMP_HANDLING], TStamp, Global)
     ,wh_json:set_value([AID, QID, CID, ?STAT_TIMESTAMP_HANDLING], TStamp, PerAgent)
    };
add_stat(Stat, {Compressed, Global, PerAgent}, <<"processed">>) ->
    QID = wh_json:get_value(<<"queue_id">>, Stat),
    CID = wh_json:get_value(<<"call_id">>, Stat),
    AID = wh_json:get_value(<<"agent_id">>, Stat),

    TStamp = wh_json:get_value(<<"timestamp">>, Stat),

    {wh_json:set_values([{[AID, QID, CID, ?STAT_TIMESTAMP_PROCESSED], TStamp}
                        ], Compressed)
     ,wh_json:set_value([QID, CID, ?STAT_TIMESTAMP_PROCESSED], TStamp, Global)
     ,wh_json:set_value([AID, QID, CID, ?STAT_TIMESTAMP_PROCESSED], TStamp, PerAgent)
    };
add_stat(Stat, {Compressed, Global, PerAgent}, ?STAT_AGENTS_MISSED) ->
    QID = wh_json:get_value(<<"queue_id">>, Stat),
    CID = wh_json:get_value(<<"call_id">>, Stat),
    AID = wh_json:get_value(<<"agent_id">>, Stat),

    TStamp = wh_json:get_value(<<"timestamp">>, Stat),

    K = [AID, QID, CID, <<"agents_tried">>],
    AgentsTried = wh_json:set_value(wh_util:to_binary(TStamp), AID, wh_json:get_value(K, Compressed, wh_json:new())),

    NumAgentsTried = num_agents_tried(AgentsTried, AID),

    {wh_json:set_values([{K, AgentsTried}
                        ], Compressed)
     ,case wh_json:get_value([QID, CID, ?STAT_AGENTS_MISSED], Global, 0) of
          N when N < NumAgentsTried ->
              wh_json:set_value([QID, CID, ?STAT_AGENTS_MISSED], NumAgentsTried, Global);
          _ -> Global
      end
     ,case wh_json:get_value([AID, QID, CID, ?STAT_AGENTS_MISSED], PerAgent, 0) of
          N when N < NumAgentsTried ->
              wh_json:set_value([AID, QID, CID, ?STAT_AGENTS_MISSED], NumAgentsTried, PerAgent);
          _ -> PerAgent
      end
    };
add_stat(_Stat, {_Compressed, _Global, _PerAgent}=Res, _T) ->
    lager:debug("unhandled stat type: ~p", [_T]),
    Res.

-spec num_agents_tried/2 :: (wh_json:object(), ne_binary()) -> non_neg_integer().    
num_agents_tried(AgentsTried, AID) ->
    {Vs, _} = wh_json:get_values(AgentsTried),
    lists:foldl(fun(A, Acc) when A =:= AID -> Acc + 1;
                   (_, Acc) -> Acc
                end, 0, Vs).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary/1 :: (cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:set_value(<<"id">>
                       ,wh_json:get_value(<<"id">>, JObj)
                       ,wh_json:get_value(<<"value">>, JObj)
                      )
     | Acc
    ].

normalize_agent_stats(JObj, Acc) ->
    [wh_doc:public_fields(
       wh_json:delete_key(<<"type">>, wh_json:get_value(<<"doc">>, JObj))
      )
     | Acc].
