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
%%% /agents/statuses
%%%   GET: statuses for each agents
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
         ,content_types_provided/1, content_types_provided/2
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

-define(QUEUE_STATUS_HANDLING, <<"handling">>).
-define(QUEUE_STATUS_PROCESSED, <<"processed">>).

-define(AGENT_STATUS_READY, <<"ready">>).
-define(AGENT_STATUS_BUSY, <<"busy">>).
-define(AGENT_STATUS_HANDLING, <<"handling">>).
-define(AGENT_STATUS_WRAPUP, <<"wrapup">>).
-define(AGENT_STATUS_PAUSED, <<"paused">>).
-define(AGENT_STATUS_LOGIN, <<"login">>).
-define(AGENT_STATUS_LOGOUT, <<"logout">>).

-define(CB_LIST, <<"agents/crossbar_listing">>).
-define(STATS_PATH_TOKEN, <<"stats">>).
-define(STATUS_PATH_TOKEN, <<"status">>).

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
allowed_methods() -> ['GET'].
allowed_methods(_) -> ['GET'].

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
-spec content_types_provided/1 :: (cb_context:context()) -> cb_context:context().
-spec content_types_provided/2 :: (cb_context:context(), path_token()) -> cb_context:context().
content_types_provided(#cb_context{}=Context) -> Context.
content_types_provided(#cb_context{}=Context, ?STATUS_PATH_TOKEN) -> Context;
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

validate(#cb_context{req_verb = <<"get">>}=Context, ?STATUS_PATH_TOKEN) ->
    fetch_all_agent_statuses(Context);
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
read(Id, Context) -> crossbar_doc:load(Id, Context).

fetch_all_agent_statuses(Context) ->
    AcctId = cb_context:account_id(Context),
    Context.

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

    Opts = [{startkey, [To]}
            ,{endkey, [From]}
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
            AcctId = cb_context:account_id(Context),
            lager:debug("failed to load stats from ~s", [acdc_stats:db_name(AcctId)]),

            case couch_mgr:db_exists(acdc_stats:db_name(AcctId)) of
                'true' -> Context;
                'false' ->
                    spawn(fun() ->
                                  lager:debug("db ~s doesn't exist, let's init it", [AcctId]),
                                  acdc_stats:init_db(AcctId)
                          end),
                    crossbar_util:response_db_fatal(Context)
            end
    end.

compress_stats(undefined, {Compressed, _, _}) -> Compressed;
compress_stats([], {Compressed, Global, PerAgent}) -> accumulate_stats(Compressed, Global, PerAgent);
compress_stats([Stat|Stats], {_,_,_}=Res) ->
    compress_stats(Stats, add_stat(Stat, Res)).

-spec accumulate_stats/3 :: (wh_json:object(), wh_json:object(), wh_json:object()) ->
                                    wh_json:object().
accumulate_stats(Compressed, Global, PerAgent) ->
    wh_json:from_list([{<<"totals">>, wh_json:foldl(fun fold_queue_totals/3, wh_json:new(), Global)}
                       ,{<<"agents">>, wh_json:merge_recursive(
                                         wh_json:foldl(fun(AgentId, AgentData, Acc) ->
                                                               fold_agent_totals(AgentId, AgentData, Acc)
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

fold_agent_totals(AgentId, AgentData, Acc) ->
    Queues = wh_json:get_value(<<"queues">>, AgentData, wh_json:new()),

    QueueTotals = lists:map(fun({QueueId, Calls}) ->
                                    {[AgentId, <<"queues">>, QueueId, <<"totals">>]
                                     ,wh_json:foldl(fun fold_call_totals/3, wh_json:new(), Calls)
                                    }
                            end, wh_json:to_proplist(Queues)
                           ),

    AgentTotals = wh_json:foldl(fun fold_queue_totals/3, wh_json:new(), Queues),

    wh_json:set_values([{[AgentId, <<"totals">>], AgentTotals}
                        | QueueTotals
                       ], Acc).

%% returns {AgentID: {queues: {QueueId: data}}}
accumulate_agent_stats(AgentId, Calls) ->
    {AgentId, wh_json:set_value(<<"queues">>
                                ,wh_json:map(fun accumulate_queue_stats/2, wh_json:get_value(<<"queues">>, Calls, wh_json:new()))
                                ,Calls
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

add_stat(Stat, {Compressed, Global, PerAgent}, ?QUEUE_STATUS_HANDLING = Status) ->
    AID = wh_json:get_value(<<"agent_id">>, Stat),
    TStamp = wh_json:get_integer_value(<<"timestamp">>, Stat, 0),

    case wh_json:get_value(<<"queue_id">>, Stat) of
        undefined ->
            {maybe_add_current_status(Compressed, Stat, Status, AID, TStamp)
             ,Global
             ,PerAgent
            };
        QID ->
            CID = wh_json:get_value(<<"call_id">>, Stat),

            {wh_json:set_value([AID, <<"queues">>, QID, CID, ?STAT_TIMESTAMP_HANDLING]
                               ,TStamp
                               ,maybe_add_current_status(Compressed, Stat, Status, AID, TStamp)
                              )
             ,wh_json:set_value([QID, CID, ?STAT_TIMESTAMP_HANDLING], TStamp, Global)
             ,wh_json:set_value([AID, <<"queues">>, QID, CID, ?STAT_TIMESTAMP_HANDLING], TStamp, PerAgent)
            }
    end;
add_stat(Stat, {Compressed, Global, PerAgent}, ?QUEUE_STATUS_PROCESSED = Status) ->
    QID = wh_json:get_value(<<"queue_id">>, Stat),
    CID = wh_json:get_value(<<"call_id">>, Stat),
    AID = wh_json:get_value(<<"agent_id">>, Stat),

    TStamp = wh_json:get_integer_value(<<"timestamp">>, Stat, 0),

    {wh_json:set_value([AID, <<"queues">>, QID, CID, ?STAT_TIMESTAMP_PROCESSED]
                       ,TStamp
                       ,maybe_add_current_status(Compressed, Stat, Status, AID, TStamp)
                      )
     ,wh_json:set_value([QID, CID, ?STAT_TIMESTAMP_PROCESSED], TStamp, Global)
     ,wh_json:set_value([AID, <<"queues">>, QID, CID, ?STAT_TIMESTAMP_PROCESSED], TStamp, PerAgent)
    };
add_stat(Stat, {Compressed, Global, PerAgent}, ?STAT_AGENTS_MISSED = Status) ->
    QID = wh_json:get_value(<<"queue_id">>, Stat),
    CID = wh_json:get_value(<<"call_id">>, Stat),
    AID = wh_json:get_value(<<"agent_id">>, Stat),

    TStamp = wh_json:get_integer_value(<<"timestamp">>, Stat, 0),

    K = [AID, <<"queues">>, QID, CID, <<"agents_tried">>],
    AgentsTried = wh_json:set_value(wh_util:to_binary(TStamp), AID, wh_json:get_value(K, Compressed, wh_json:new())),

    NumAgentsTried = num_agents_tried(AgentsTried, AID),

    {wh_json:set_values([{K, AgentsTried}]
                        ,maybe_add_current_status(Compressed, Stat, Status, AID, TStamp)
                       )
     ,case wh_json:get_value([QID, CID, ?STAT_AGENTS_MISSED], Global, 0) of
          N when N < NumAgentsTried ->
              wh_json:set_value([QID, CID, ?STAT_AGENTS_MISSED], NumAgentsTried, Global);
          _ -> Global
      end
     ,case wh_json:get_value([AID, QID, CID, ?STAT_AGENTS_MISSED], PerAgent, 0) of
          N when N < NumAgentsTried ->
              wh_json:set_value([AID, <<"queues">>, QID, CID, ?STAT_AGENTS_MISSED], NumAgentsTried, PerAgent);
          _ -> PerAgent
      end
    };
add_stat(Stat, {Compressed, Global, PerAgent}, Status) when
      Status =:= ?AGENT_STATUS_READY;
      Status =:= ?AGENT_STATUS_LOGIN;
      Status =:= ?AGENT_STATUS_BUSY;
      Status =:= ?AGENT_STATUS_LOGOUT;
      Status =:= ?AGENT_STATUS_PAUSED;
      Status =:= ?AGENT_STATUS_WRAPUP
      ->
    AID = wh_json:get_value(<<"agent_id">>, Stat),
    TStamp = wh_json:get_integer_value(<<"timestamp">>, Stat, 0),

    {maybe_add_current_status(Compressed, Stat, Status, AID, TStamp)
     ,Global
     ,PerAgent
    };
add_stat(_Stat, {_Compressed, _Global, _PerAgent}=Res, _T) ->
    lager:debug("unhandled stat type: ~p: ~p", [_T, _Stat]),
    Res.

-spec maybe_add_current_status/5 :: (wh_json:object(), wh_json:object(), ne_binary(), ne_binary(), wh_now()) ->
                                            wh_json:object().
maybe_add_current_status(Compressed, Stat, Status, AID, TStamp) ->
    case wh_json:get_integer_value([AID, <<"current">>, <<"status_timestamp">>], Compressed) of
        T when T =:= undefined orelse T < TStamp ->
            case complex_agent_status(Stat, Status, AID) of
                undefined -> Compressed;
                [_|_]=StatusData -> wh_json:set_values(StatusData
                                                       ,wh_json:delete_key([AID, <<"current">>], Compressed)
                                                      )
            end;
        _ -> Compressed
    end.

-spec complex_agent_status/3 :: (wh_json:object(), ne_binary(), ne_binary()) -> wh_proplist().
complex_agent_status(Stat, ?AGENT_STATUS_BUSY = Status, AID) ->
    [{[AID, <<"current">>, <<"status_started">>], wh_json:get_integer_value(<<"timestamp">>, Stat)}
     ,{[AID, <<"current">>, <<"status">>], Status}
     ,{[AID, <<"current">>, <<"outbound_callid">>], wh_json:get_value(<<"call_id">>, Stat)}
    ];
complex_agent_status(Stat, ?AGENT_STATUS_WRAPUP = Status, AID) ->
    props:filter_undefined(
      [{[AID, <<"current">>, <<"status">>], Status}
       ,{[AID, <<"current">>, <<"wait_time">>], wh_json:get_value(<<"wait_time">>, Stat)}
       ,{[AID, <<"current">>, <<"status_started">>], wh_json:get_value(<<"timestamp">>, Stat)}
      ]);
complex_agent_status(StatusJObj, ?AGENT_STATUS_PAUSED = Status, AID) ->
    props:filter_undefined(
      [{[AID, <<"current">>, <<"status">>], Status}
       ,{[AID, <<"current">>, <<"wait_time">>], wh_json:get_value(<<"wait_time">>, StatusJObj)}
       ,{[AID, <<"current">>, <<"status_started">>], wh_json:get_value(<<"timestamp">>, StatusJObj)}
      ]);
complex_agent_status(Stat, ?AGENT_STATUS_LOGOUT = Status, AID) ->
    props:filter_undefined(
      [{[AID, <<"current">>, <<"status">>], Status}
       ,{[AID, <<"current">>, <<"status_started">>], wh_json:get_value(<<"timestamp">>, Stat)}
      ]);
complex_agent_status(_Stat, ?AGENT_STATUS_LOGIN = _Status, AID) ->
    props:filter_undefined(
      [{[AID, <<"current">>, <<"status">>], ?AGENT_STATUS_READY}]);
complex_agent_status(Stat, ?AGENT_STATUS_HANDLING = Status, AID) ->
    props:filter_undefined(
      [{[AID, <<"current">>, <<"status">>], Status}
       ,{[AID, <<"current">>, <<"status_started">>], wh_json:get_value(<<"timestamp">>, Stat)}
      ]);
complex_agent_status(Stat, ?STAT_AGENTS_MISSED, AID) ->
    complex_agent_status(Stat, ?AGENT_STATUS_READY, AID);
complex_agent_status(Stat, ?QUEUE_STATUS_HANDLING, AID) ->
    complex_agent_status(Stat, ?AGENT_STATUS_HANDLING, AID);
complex_agent_status(_Stat, ?QUEUE_STATUS_PROCESSED, _AID) ->
    undefined;
complex_agent_status(_Stat, Status, AID) ->
    [{[AID, <<"current">>, <<"status">>], Status}].

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
