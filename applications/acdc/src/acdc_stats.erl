%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Collector of stats
%%% @author James Aimonetti
%%% @author Sponsored by GTNetwork LLC, Implemented by SIPLABS LLC
%%% @author Daniel Finke
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_stats).
-behaviour(gen_listener).

%% Public API
-export([call_waiting/6
        ,call_abandoned/4
        ,call_handled/4
        ,call_missed/5
        ,call_processed/5

        ,find_call/1
        ,call_stat_to_json/1
        ]).

%% ETS config
-export([call_table_id/0
        ,call_key_pos/0
        ,call_table_opts/0

        ,init_db/1
        ,archive_call_data/2
        ]).

%% AMQP Callbacks
-export([handle_call_stat/2
        ,handle_call_query/2
        ,handle_average_wait_time_req/2
        ]).

%% gen_listener functions
-export([start_link/0
        ,init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("acdc.hrl").
-include("acdc_stats.hrl").

-define(SERVER, ?MODULE).

%% Public API
-spec call_waiting(kz_term:api_binary()
                  ,kz_term:api_binary()
                  ,kz_term:api_binary()
                  ,kz_term:api_binary()
                  ,kz_term:api_binary()
                  ,kz_term:api_binary()
                  ) -> 'ok'.
call_waiting(AccountId, QueueId, CallId, CallerIdName, CallerIdNumber, CallerPriority) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Caller-ID-Name">>, CallerIdName}
             ,{<<"Caller-ID-Number">>, CallerIdNumber}
             ,{<<"Entered-Timestamp">>, kz_time:now_s()}
             ,{<<"Caller-Priority">>, CallerPriority}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    call_state_change(AccountId, 'waiting', Prop),
    'ok' = kz_amqp_worker:cast(Prop, fun kapi_acdc_stats:publish_call_waiting/1).

-spec call_abandoned(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), atom()) -> 'ok'.
call_abandoned(AccountId, QueueId, CallId, Reason) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Abandon-Reason">>, Reason}
             ,{<<"Abandon-Timestamp">>, kz_time:now_s()}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    call_state_change(AccountId, 'abandoned', Prop),
    'ok' = kz_amqp_worker:cast(Prop, fun kapi_acdc_stats:publish_call_abandoned/1).

-spec call_handled(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
call_handled(AccountId, QueueId, CallId, AgentId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Handled-Timestamp">>, kz_time:now_s()}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    call_state_change(AccountId, 'handled', Prop),
    'ok' = kz_amqp_worker:cast(Prop, fun kapi_acdc_stats:publish_call_handled/1).

-spec call_missed(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
call_missed(AccountId, QueueId, AgentId, CallId, ErrReason) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Miss-Reason">>, ErrReason}
             ,{<<"Miss-Timestamp">>, kz_time:now_s()}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    call_state_change(AccountId, 'missed', Prop),
    'ok' = kz_amqp_worker:cast(Prop, fun kapi_acdc_stats:publish_call_missed/1).

-spec call_processed(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), atom()) -> 'ok'.
call_processed(AccountId, QueueId, AgentId, CallId, Initiator) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Processed-Timestamp">>, kz_time:now_s()}
             ,{<<"Hung-Up-By">>, Initiator}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    call_state_change(AccountId, 'processed', Prop),
    'ok' = kz_amqp_worker:cast(Prop, fun kapi_acdc_stats:publish_call_processed/1).

%% ETS config

-spec call_table_id() -> atom().
call_table_id() -> 'acdc_stats_call'.

-spec call_key_pos() -> pos_integer().
call_key_pos() -> #call_stat.id.

-spec call_table_opts() -> kz_term:proplist().
call_table_opts() ->
    ['protected', 'named_table'
    ,{'keypos', call_key_pos()}
    ].

-define(BINDINGS, [{'self', []}
                  ,{?MODULE, []}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_call_stat'}
                     ,[{<<"acdc_call_stat">>, <<"waiting">>}
                      ,{<<"acdc_call_stat">>, <<"missed">>}
                      ,{<<"acdc_call_stat">>, <<"abandoned">>}
                      ,{<<"acdc_call_stat">>, <<"handled">>}
                      ,{<<"acdc_call_stat">>, <<"processed">>}
                      ,{<<"acdc_call_stat">>, <<"flush">>}
                      ]
                     }
                    ,{{'acdc_agent_stats', 'handle_status_stat'}
                     ,[{<<"acdc_status_stat">>, <<"ready">>}
                      ,{<<"acdc_status_stat">>, <<"logged_in">>}
                      ,{<<"acdc_status_stat">>, <<"logged_out">>}
                      ,{<<"acdc_status_stat">>, <<"pending_logged_out">>}
                      ,{<<"acdc_status_stat">>, <<"connecting">>}
                      ,{<<"acdc_status_stat">>, <<"connected">>}
                      ,{<<"acdc_status_stat">>, <<"wrapup">>}
                      ,{<<"acdc_status_stat">>, <<"paused">>}
                      ,{<<"acdc_status_stat">>, <<"outbound">>}
                      ]
                     }
                    ,{{?MODULE, 'handle_call_query'}
                     ,[{<<"acdc_stat">>, <<"current_calls_req">>}]
                     }
                    ,{{?MODULE, 'handle_average_wait_time_req'}
                     ,[{<<"acdc_stat">>, <<"average_wait_time_req">>}]
                     }
                    ,{{'acdc_agent_stats', 'handle_status_query'}
                     ,[{<<"acdc_stat">>, <<"status_req">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ],
                            []).

-spec handle_call_stat(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_call_stat(JObj, Props) ->
    case kz_json:get_value(<<"Event-Name">>, JObj) of
        <<"waiting">> -> handle_waiting_stat(JObj, Props);
        <<"missed">> -> handle_missed_stat(JObj, Props);
        <<"abandoned">> -> handle_abandoned_stat(JObj, Props);
        <<"handled">> -> handle_handled_stat(JObj, Props);
        <<"processed">> -> handle_processed_stat(JObj, Props);
        <<"flush">> -> flush_call_stat(JObj, Props);
        _Name ->
            lager:debug("recv unknown call stat type ~s: ~p", [_Name, JObj])
    end.

-spec handle_call_query(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_call_query(JObj, _Prop) ->
    'true' = kapi_acdc_stats:current_calls_req_v(JObj),
    RespQ = kz_json:get_value(<<"Server-ID">>, JObj),
    MsgId = kz_json:get_value(<<"Msg-ID">>, JObj),
    Limit = acdc_stats_util:get_query_limit(JObj),

    case call_build_match_spec(JObj) of
        {'ok', Match} -> query_calls(RespQ, MsgId, Match, Limit);
        {'error', Errors} -> publish_query_errors(RespQ, MsgId, Errors)
    end.

%%------------------------------------------------------------------------------
%% @doc Handle requests for the average wait time of a queue based on stats
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_average_wait_time_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_average_wait_time_req(JObj, _Prop) ->
    'true' = kapi_acdc_stats:average_wait_time_req_v(JObj),
    Match = average_wait_time_build_match_spec(JObj),
    query_average_wait_time(Match, JObj).

-spec find_call(kz_term:ne_binary()) -> kz_term:api_object().
find_call(CallId) ->
    MS = [{#call_stat{call_id=CallId
                     ,_ = '_'
                     }
          ,[]
          ,['$_']
          }],
    case ets:select(call_table_id(), MS) of
        [] -> 'undefined';
        [Stat] -> call_stat_to_json(Stat);
        Stats -> call_stat_to_json(get_recent_stat_for_call(Stats))
    end.

-spec get_recent_stat_for_call(call_stats()) -> call_stat().
get_recent_stat_for_call(Stats) ->
    Sorted = lists:sort(fun sort_by_entered_timestamp/2, Stats),
    lists:nth(1, Sorted).

-spec sort_by_entered_timestamp(call_stat(), call_stat()) -> boolean().
sort_by_entered_timestamp(#call_stat{entered_timestamp=ATimestamp}, #call_stat{entered_timestamp=BTimestamp}) ->
    ATimestamp > BTimestamp.

-record(state, {archive_ref :: reference()
               ,cleanup_ref :: reference()
               }).
-type state() :: #state{}.

-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_log:put_callid(<<"acdc.stats">>),
    kz_datamgr:suppress_change_notice(),
    lager:debug("started new acdc stats collector"),

    {'ok', #state{archive_ref=start_archive_timer()
                 ,cleanup_ref=start_cleanup_timer()
                 }}.

-spec start_archive_timer() -> reference().
start_archive_timer() ->
    erlang:send_after(?ARCHIVE_PERIOD, self(), ?ARCHIVE_MSG).

-spec start_cleanup_timer() -> reference().
start_cleanup_timer() ->
    erlang:send_after(?CLEANUP_PERIOD, self(), ?CLEANUP_MSG).

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Req, _From, State) ->
    {'reply', 'ok', State}.

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'create_call', #call_stat{id=_Id}=Stat}, State) ->
    lager:debug("creating new call stat ~s", [_Id]),
    ets:insert_new(call_table_id(), Stat),
    {'noreply', State};
handle_cast({'create_status', #status_stat{id=_Id, status=_Status}=Stat}, State) ->
    lager:debug("creating new status stat ~s: ~s", [_Id, _Status]),
    case ets:insert_new(acdc_agent_stats:status_table_id(), Stat) of
        'true' -> {'noreply', State};
        'false' ->
            lager:debug("stat ~s already exists, updating", [_Id]),
            ets:insert(acdc_agent_stats:status_table_id(), Stat),
            {'noreply', State}
    end;
handle_cast({'update_call', Id, Updates}, State) ->
    lager:debug("updating call stat ~s: ~p", [Id, Updates]),
    ets:update_element(call_table_id(), Id, Updates),
    {'noreply', State};
handle_cast({'flush_call', Id}, State) ->
    lager:debug("flushing call stat ~s", [Id]),
    ets:delete(call_table_id(), Id),
    {'noreply', State};
handle_cast({'remove_call', [{M, P, _}]}, State) ->
    Match = [{M, P, ['true']}],
    N = ets:select_delete(call_table_id(), Match),
    N > 1
        andalso lager:debug("removed calls: ~p", [N]),
    {'noreply', State};

handle_cast({'update_status', Id, Updates}, State) ->
    lager:debug("updating status stat ~s: ~p", [Id, Updates]),
    ets:update_element(acdc_agent_stats:status_table_id(), Id, Updates),
    {'noreply', State};
handle_cast({'remove_status', [{M, P, _}]}, State) ->
    Match = [{M, P, ['true']}],
    N = ets:select_delete(acdc_agent_stats:status_table_id(), Match),
    N > 1
        andalso lager:debug("removed statuses: ~p", [N]),
    {'noreply', State};

handle_cast({'gen_listener',{'created_queue',_Q}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Req, State) ->
    lager:debug("unhandled cast: ~p", [_Req]),
    {'noreply', State}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'ETS-TRANSFER', _TblId, _From, _Data}, State) ->
    lager:debug("ETS control for ~p transferred to me for writing", [_TblId]),
    {'noreply', State};
handle_info(?ARCHIVE_MSG, State) ->
    _ = archive_data(),
    {'noreply', State#state{archive_ref=start_archive_timer()}};
handle_info(?CLEANUP_MSG, State) ->
    _ = cleanup_data(self()),
    {'noreply', State#state{cleanup_ref=start_cleanup_timer()}};
handle_info(_Msg, State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', State}.

-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _) ->
    force_archive_data(),
    lager:debug("acdc stats terminating: ~p", [_Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

publish_query_errors(RespQ, MsgId, Errors) ->
    API = [{<<"Error-Reason">>, Errors}
          ,{<<"Msg-ID">>, MsgId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("responding with errors to req ~s: ~p", [MsgId, Errors]),
    kapi_acdc_stats:publish_current_calls_err(RespQ, API).

call_build_match_spec(JObj) ->
    case kz_json:get_value(<<"Account-ID">>, JObj) of
        'undefined' ->
            {'error', kz_json:from_list([{<<"Account-ID">>, <<"missing but required">>}])};
        AccountId ->
            AccountMatch = {#call_stat{account_id='$1', _='_'}
                           ,[{'=:=', '$1', {'const', AccountId}}]
                           },
            call_build_match_spec(JObj, AccountMatch)
    end.

-spec call_build_match_spec(kz_json:object(), {call_stat(), list()}) ->
          {'ok', ets:match_spec()} |
          {'error', kz_json:object()}.
call_build_match_spec(JObj, AccountMatch) ->
    case kz_json:foldl(fun call_match_builder_fold/3, AccountMatch, JObj) of
        {'error', _Errs}=Errors -> Errors;
        {CallStat, Constraints} -> {'ok', [{CallStat, Constraints, ['$_']}]}
    end.

call_match_builder_fold(_, _, {'error', _Err}=E) -> E;
call_match_builder_fold(<<"Queue-ID">>, QueueId, {CallStat, Contstraints}) ->
    {CallStat#call_stat{queue_id='$2'}
    ,[{'=:=', '$2', {'const', QueueId}} | Contstraints]
    };
call_match_builder_fold(<<"Agent-ID">>, AgentId, {CallStat, Contstraints}) ->
    {CallStat#call_stat{agent_id='$3'}
    ,[{'=:=', '$3', {'const', AgentId}} | Contstraints]
    };
call_match_builder_fold(<<"Status">>, Status, {CallStat, Contstraints}) ->
    case is_valid_call_status(Status) of
        {'true', Normalized} ->
            {CallStat#call_stat{status='$4'}
            ,[{'=:=', '$4', {'const', Normalized}} | Contstraints]
            };
        'false' ->
            {'error', kz_json:from_list([{<<"Status">>, <<"unknown status supplied">>}])}
    end;
call_match_builder_fold(<<"Start-Range">>, Start, {CallStat, Contstraints}) ->
    Now = kz_time:now_s(),
    Past = Now - ?CLEANUP_WINDOW,

    try kz_term:to_integer(Start) of
        N when N < Past ->
            {'error', kz_json:from_list([{<<"Start-Range">>, <<"supplied value is too far in the past">>}
                                        ,{<<"Window-Size">>, ?CLEANUP_WINDOW}
                                        ,{<<"Current-Timestamp">>, Now}
                                        ,{<<"Past-Timestamp">>, Past}
                                        ])};
        N when N > Now ->
            {'error', kz_json:from_list([{<<"Start-Range">>, <<"supplied value is in the future">>}
                                        ,{<<"Current-Timestamp">>, Now}
                                        ])};
        N ->
            {CallStat#call_stat{entered_timestamp='$5'}
            ,[{'>=', '$5', N} | Contstraints]
            }
    catch
        _:_ ->
            {'error', kz_json:from_list([{<<"Start-Range">>, <<"supplied value is not an integer">>}])}
    end;
call_match_builder_fold(<<"End-Range">>, End, {CallStat, Contstraints}) ->
    Now = kz_time:now_s(),
    Past = Now - ?CLEANUP_WINDOW,

    try kz_term:to_integer(End) of
        N when N < Past ->
            {'error', kz_json:from_list([{<<"End-Range">>, <<"supplied value is too far in the past">>}
                                        ,{<<"Window-Size">>, ?CLEANUP_WINDOW}
                                        ,{<<"Current-Timestamp">>, Now}
                                        ])};
        N when N > Now ->
            {'error', kz_json:from_list([{<<"End-Range">>, <<"supplied value is in the future">>}
                                        ,{<<"Current-Timestamp">>, Now}
                                        ])};
        N ->
            {CallStat#call_stat{entered_timestamp='$5'}
            ,[{'=<', '$5', N} | Contstraints]
            }
    catch
        _:_ ->
            {'error', kz_json:from_list([{<<"End-Range">>, <<"supplied value is not an integer">>}])}
    end;
call_match_builder_fold(_, _, Acc) -> Acc.

-spec average_wait_time_build_match_spec(kz_json:object()) -> ets:match_spec().
average_wait_time_build_match_spec(JObj) ->
    AccountId = kz_json:get_ne_binary_value(<<"Account-ID">>, JObj),
    QueueId = kz_json:get_ne_binary_value(<<"Queue-ID">>, JObj),

    Match = [{#call_stat{account_id=AccountId
                        ,queue_id=QueueId
                        ,entered_timestamp='$1'
                        ,abandoned_timestamp='$2'
                        ,handled_timestamp='$3'
                        ,status='$4'
                        ,_='_'
                        }
             ,[{'orelse'
               ,{'=:=', '$4', {'const', <<"handled">>}}
               ,{'=:=', '$4', {'const', <<"processed">>}}
               }]
             ,[['$1', '$2', '$3']]
             }],

    Window = kz_json:get_integer_value(<<"Window">>, JObj),

    average_wait_time_build_match_spec(Match, Window).

-spec average_wait_time_build_match_spec(ets:match_spec(), kz_term:api_integer()) ->
          ets:match_spec().
average_wait_time_build_match_spec(Match, 'undefined') ->
    Match;
average_wait_time_build_match_spec([{CallStat, Conditions, Results}], Window) ->
    Start = kz_time:current_tstamp() - Window,
    [{CallStat, [{'>=', '$1', {'const', Start}} | Conditions], Results}].

is_valid_call_status(S) ->
    Status = kz_term:to_lower_binary(S),
    case lists:member(Status, ?VALID_STATUSES) of
        'true' -> {'true', Status};
        'false' -> 'false'
    end.

-spec query_calls(kz_term:ne_binary(), kz_term:ne_binary(), ets:match_spec(), pos_integer() | 'no_limit') -> 'ok'.
query_calls(RespQ, MsgId, Match, _Limit) ->
    case ets:select(call_table_id(), Match) of
        [] ->
            lager:debug("no stats found, sorry ~s", [RespQ]),
            Resp = [{<<"Query-Time">>, kz_time:now_s()}
                   ,{<<"Msg-ID">>, MsgId}
                    | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            kapi_acdc_stats:publish_current_calls_resp(RespQ, Resp);
        Stats ->
            Dict = dict:from_list([{<<"waiting">>, []}
                                  ,{<<"handled">>, []}
                                  ,{<<"abandoned">>, []}
                                  ,{<<"processed">>, []}
                                  ]),

            QueryResult = lists:foldl(fun query_call_fold/2, Dict, Stats),
            Resp = [{<<"Waiting">>, dict:fetch(<<"waiting">>, QueryResult)}
                   ,{<<"Handled">>, dict:fetch(<<"handled">>, QueryResult)}
                   ,{<<"Abandoned">>, dict:fetch(<<"abandoned">>, QueryResult)}
                   ,{<<"Processed">>, dict:fetch(<<"processed">>, QueryResult)}
                   ,{<<"Query-Time">>, kz_time:now_s()}
                   ,{<<"Msg-ID">>, MsgId}
                    | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],

            kapi_acdc_stats:publish_current_calls_resp(RespQ, Resp)
    end.

%%------------------------------------------------------------------------------
%% @doc Calculate and reply with the average wait time on a queue
%%
%% @end
%%------------------------------------------------------------------------------
-spec query_average_wait_time(ets:match_spec(), kz_json:object()) -> 'ok'.
query_average_wait_time(Match, JObj) ->
    AverageWaitTime = average_wait_time_fold(ets:select(call_table_id(), Match)),

    RespQ = kz_json:get_value(<<"Server-ID">>, JObj),
    Resp = [{<<"Average-Wait-Time">>, AverageWaitTime}
           ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_acdc_stats:publish_average_wait_time_resp(RespQ, Resp).

%%------------------------------------------------------------------------------
%% @doc Calculate the average wait time given a list of finished call stat
%% timestamps
%%
%% @end
%%------------------------------------------------------------------------------
-spec average_wait_time_fold(list()) -> non_neg_integer().
average_wait_time_fold(Stats) ->
    {CallCount, TotalWaitTime} = lists:foldl(fun average_wait_time_fold/2
                                            ,{0, 0}
                                            ,Stats
                                            ),
    case CallCount of
        0 -> 0;
        _ -> TotalWaitTime div CallCount
    end.

-spec average_wait_time_fold([non_neg_integer()], {non_neg_integer(), non_neg_integer()}) ->
          {non_neg_integer(), non_neg_integer()}.
average_wait_time_fold([EnteredT, AbandonedT, HandledT], {CallCount, TotalWaitTime}) ->
    WaitTime = wait_time(EnteredT, AbandonedT, HandledT),
    {CallCount + 1, TotalWaitTime + WaitTime}.

-spec archive_data() -> 'ok'.
archive_data() ->
    Self = self(),
    _ = kz_process:spawn(fun archive_call_data/2, [Self, 'false']),
    _ = kz_process:spawn(fun acdc_agent_stats:archive_status_data/2, [Self, 'false']),
    'ok'.

force_archive_data() ->
    Self = self(),
    _ = kz_process:spawn(fun archive_call_data/2, [Self, 'true']),
    _ = kz_process:spawn(fun acdc_agent_stats:archive_status_data/2, [Self, 'true']),
    'ok'.

cleanup_data(Srv) ->
    Past = kz_time:now_s() - ?CLEANUP_WINDOW,
    PastConstraint = {'=<', '$1', Past},

    TypeConstraints = [{'=/=', '$2', {'const', <<"waiting">>}}
                      ,{'=/=', '$2', {'const', <<"handled">>}}
                      ],

    CallMatch = [{#call_stat{entered_timestamp='$1', status='$2', _='_'}
                 ,[PastConstraint | TypeConstraints]
                 ,['$_']
                 }],
    gen_listener:cast(Srv, {'remove_call', CallMatch}),

    StatusMatch = [{#status_stat{key=#status_stat_key{timestamp='$1'}, _='_'}
                   ,[{'=<', '$1', Past}]
                   ,['$_']
                   }],
    gen_listener:cast(Srv, {'remove_status', StatusMatch}),

    case ets:select(call_table_id()
                   ,[{#call_stat{entered_timestamp='$1', status= <<"waiting">>, _='_'}
                     ,[PastConstraint]
                     ,['$_']
                     }
                    ,{#call_stat{entered_timestamp='$1', status= <<"handled">>, _='_'}
                     ,[PastConstraint]
                     ,['$_']
                     }
                    ])
    of
        [] -> 'ok';
        Unfinished -> cleanup_unfinished(Unfinished)
    end.

cleanup_unfinished(Unfinished) ->
    lager:debug("unfinished stats: ~p", [Unfinished]).

-spec archive_call_data(pid(), boolean()) -> 'ok'.
archive_call_data(Srv, 'true') ->
    kz_log:put_callid(<<"acdc_stats.force_call_archiver">>),

    Match = [{#call_stat{status='$1'
                        ,is_archived='$2'
                        ,_='_'
                        }
             ,[{'=/=', '$1', {'const', <<"waiting">>}}
              ,{'=/=', '$1', {'const', <<"handled">>}}
              ,{'=:=', '$2', 'false'}
              ]
             ,['$_']
             }],
    maybe_archive_call_data(Srv, Match);
archive_call_data(Srv, 'false') ->
    kz_log:put_callid(<<"acdc_stats.call_archiver">>),

    Past = kz_time:now_s() - ?ARCHIVE_WINDOW,
    Match = [{#call_stat{entered_timestamp='$1'
                        ,status='$2'
                        ,is_archived='$3'
                        , _='_'
                        }
             ,[{'=<', '$1', Past}
              ,{'=/=', '$2', {'const', <<"waiting">>}}
              ,{'=/=', '$2', {'const', <<"handled">>}}
              ,{'=:=', '$3', 'false'}
              ]
             ,['$_']
             }],
    maybe_archive_call_data(Srv, Match).

maybe_archive_call_data(Srv, Match) ->
    case ets:select(call_table_id(), Match) of
        [] -> 'ok';
        Stats ->
            kz_datamgr:suppress_change_notice(),
            ToSave = lists:foldl(fun archive_call_fold/2, dict:new(), Stats),
            _ = [kz_datamgr:save_docs(acdc_stats_util:db_name(Account), Docs)
                 || {Account, Docs} <- dict:to_list(ToSave)
                ],
            _ = [gen_listener:cast(Srv, {'update_call', Id, [{#call_stat.is_archived, 'true'}]})
                 || #call_stat{id=Id} <- Stats
                ],
            'ok'
    end.

-spec query_call_fold(call_stat(), dict:dict()) -> dict:dict().
query_call_fold(#call_stat{status=Status}=Stat, Acc) ->
    Doc = call_stat_to_doc(Stat),
    dict:update(Status, fun(L) -> [Doc | L] end, [Doc], Acc).

-spec archive_call_fold(call_stat(), dict:dict()) -> dict:dict().
archive_call_fold(#call_stat{account_id=AccountId}=Stat, Acc) ->
    Doc = call_stat_to_doc(Stat),
    dict:update(AccountId, fun(L) -> [Doc | L] end, [Doc], Acc).

-spec call_stat_to_doc(call_stat()) -> kz_json:object().
call_stat_to_doc(#call_stat{id=Id
                           ,call_id=CallId
                           ,account_id=AccountId
                           ,queue_id=QueueId
                           ,agent_id=AgentId
                           ,entered_timestamp=EnteredT
                           ,abandoned_timestamp=AbandonedT
                           ,handled_timestamp=HandledT
                           ,processed_timestamp=ProcessedT
                           ,hung_up_by=HungUpBy
                           ,abandoned_reason=AbandonedR
                           ,misses=Misses
                           ,status=Status
                           ,caller_id_name=CallerIdName
                           ,caller_id_number=CallerIdNumber
                           ,caller_priority=CallerPriority
                           }) ->
    kz_doc:update_pvt_parameters(kz_json:from_list(
                                   [{<<"_id">>, Id}
                                   ,{<<"call_id">>, CallId}
                                   ,{<<"queue_id">>, QueueId}
                                   ,{<<"agent_id">>, AgentId}
                                   ,{<<"entered_timestamp">>, EnteredT}
                                   ,{<<"abandoned_timestamp">>, AbandonedT}
                                   ,{<<"handled_timestamp">>, HandledT}
                                   ,{<<"processed_timestamp">>, ProcessedT}
                                   ,{<<"hung_up_by">>, HungUpBy}
                                   ,{<<"abandoned_reason">>, AbandonedR}
                                   ,{<<"misses">>, misses_to_docs(Misses)}
                                   ,{<<"status">>, Status}
                                   ,{<<"caller_id_name">>, CallerIdName}
                                   ,{<<"caller_id_number">>, CallerIdNumber}
                                   ,{<<"caller_priority">>, CallerPriority}
                                   ,{<<"wait_time">>, wait_time(EnteredT, AbandonedT, HandledT)}
                                   ,{<<"talk_time">>, talk_time(HandledT, ProcessedT)}
                                   ])
                                ,acdc_stats_util:db_name(AccountId)
                                ,[{'account_id', AccountId}
                                 ,{'type', <<"call_stat">>}
                                 ]
                                ).

-spec call_stat_to_json(call_stat()) -> kz_json:object().
call_stat_to_json(#call_stat{id=Id
                            ,call_id=CallId
                            ,account_id=AccountId
                            ,queue_id=QueueId
                            ,agent_id=AgentId
                            ,entered_timestamp=EnteredT
                            ,abandoned_timestamp=AbandonedT
                            ,handled_timestamp=HandledT
                            ,processed_timestamp=ProcessedT
                            ,hung_up_by=HungUpBy
                            ,abandoned_reason=AbandonedR
                            ,misses=Misses
                            ,status=Status
                            ,caller_id_name=CallerIdName
                            ,caller_id_number=CallerIdNumber
                            }) ->
    kz_json:from_list(
      [{<<"Id">>, Id}
      ,{<<"Call-ID">>, CallId}
      ,{<<"Queue-ID">>, QueueId}
      ,{<<"Agent-ID">>, AgentId}
      ,{<<"Account-ID">>, AccountId}
      ,{<<"Entered-Timestamp">>, EnteredT}
      ,{<<"Abandoned-Timestamp">>, AbandonedT}
      ,{<<"Handled-Timestamp">>, HandledT}
      ,{<<"Processed-Timestamp">>, ProcessedT}
      ,{<<"Hung-Up-By">>, HungUpBy}
      ,{<<"Abandoned-Reason">>, AbandonedR}
      ,{<<"Misses">>, misses_to_docs(Misses)}
      ,{<<"Status">>, Status}
      ,{<<"Caller-ID-Name">>, CallerIdName}
      ,{<<"Caller-ID-Number">>, CallerIdNumber}
      ,{<<"Wait-Time">>, wait_time(EnteredT, AbandonedT, HandledT)}
      ,{<<"Talk-Time">>, talk_time(HandledT, ProcessedT)}
      ]).

wait_time(E, _, H) when is_integer(E), is_integer(H) -> H - E;
wait_time(E, A, _) when is_integer(E), is_integer(A) -> A - E;
wait_time(_, _, _) -> 'undefined'.

talk_time(H, P) when is_integer(H), is_integer(P) -> P - H;
talk_time(_, _) -> 'undefined'.

-spec misses_to_docs(agent_misses()) -> kz_json:objects().
misses_to_docs(Misses) -> [miss_to_doc(Miss) || Miss <- Misses].

-spec miss_to_doc(agent_miss()) -> kz_json:object().
miss_to_doc(#agent_miss{agent_id=AgentId
                       ,miss_reason=Reason
                       ,miss_timestamp=T
                       }) ->
    kz_json:from_list([{<<"agent_id">>, AgentId}
                      ,{<<"reason">>, Reason}
                      ,{<<"timestamp">>, T}
                      ]).

-spec init_db(kz_term:ne_binary()) -> 'ok'.
init_db(AccountId) ->
    DbName = acdc_stats_util:db_name(AccountId),
    maybe_created_db(DbName, kazoo_modb:maybe_create(DbName)).

-spec maybe_created_db(kz_term:ne_binary(), boolean()) -> 'ok'.
maybe_created_db(DbName, 'false') ->
    case kz_datamgr:db_exists(DbName) of
        'true' ->
            lager:debug("database ~s already created, refreshing view", [DbName]),
            _ = kapps_maintenance:refresh(DbName),
            'ok';
        'false' ->
            lager:debug("modb ~s was not created", [DbName])
    end;
maybe_created_db(DbName, 'true') ->
    lager:debug("created db ~s, adding views", [DbName]),
    _ = kapps_maintenance:refresh(DbName),
    'ok'.

-spec call_stat_id(kz_json:object()) -> kz_term:ne_binary().
call_stat_id(JObj) ->
    call_stat_id(kz_json:get_value(<<"Call-ID">>, JObj)
                ,kz_json:get_value(<<"Queue-ID">>, JObj)
                ).

-spec call_stat_id(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
call_stat_id(CallId, QueueId) -> <<CallId/binary, "::", QueueId/binary>>.

-spec handle_waiting_stat(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_waiting_stat(JObj, Props) ->
    'true' = kapi_acdc_stats:call_waiting_v(JObj),

    Id = call_stat_id(JObj),
    case find_call_stat(Id) of
        'undefined' -> create_call_stat(Id, JObj, Props);
        _Stat ->
            Updates = props:filter_undefined(
                        [{#call_stat.caller_id_name, kz_json:get_value(<<"Caller-ID-Name">>, JObj)}
                        ,{#call_stat.caller_id_number, kz_json:get_value(<<"Caller-ID-Number">>, JObj)}
                        ]),
            update_call_stat(Id, Updates, Props)
    end.

-spec handle_missed_stat(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_missed_stat(JObj, Props) ->
    'true' = kapi_acdc_stats:call_missed_v(JObj),

    Id = call_stat_id(JObj),
    case find_call_stat(Id) of
        'undefined' -> lager:debug("can't update stat ~s with missed data, missing", [Id]);
        #call_stat{misses=Misses} ->
            Updates = [{#call_stat.misses, [create_miss(JObj) | Misses]}],
            update_call_stat(Id, Updates, Props)
    end.

-spec create_miss(kz_json:object()) -> agent_miss().
create_miss(JObj) ->
    #agent_miss{agent_id = kz_json:get_value(<<"Agent-ID">>, JObj)
               ,miss_reason = kz_json:get_value(<<"Miss-Reason">>, JObj)
               ,miss_timestamp = kz_json:get_value(<<"Miss-Timestamp">>, JObj)
               }.

-spec handle_abandoned_stat(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_abandoned_stat(JObj, Props) ->
    'true' = kapi_acdc_stats:call_abandoned_v(JObj),

    Id = call_stat_id(JObj),
    Updates = props:filter_undefined(
                [{#call_stat.abandoned_reason, kz_json:get_value(<<"Abandon-Reason">>, JObj)}
                ,{#call_stat.abandoned_timestamp, kz_json:get_value(<<"Abandon-Timestamp">>, JObj)}
                ,{#call_stat.status, <<"abandoned">>}
                ]),
    update_call_stat(Id, Updates, Props).

-spec handle_handled_stat(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_handled_stat(JObj, Props) ->
    'true' = kapi_acdc_stats:call_handled_v(JObj),

    Id = call_stat_id(JObj),
    Updates = props:filter_undefined(
                [{#call_stat.agent_id, kz_json:get_value(<<"Agent-ID">>, JObj)}
                ,{#call_stat.handled_timestamp, kz_json:get_value(<<"Handled-Timestamp">>, JObj)}
                ,{#call_stat.status, <<"handled">>}
                ]),

    Stat = find_call_stat(Id),
    case handled_stat_should_update(Stat) of
        'true' -> update_call_stat(Id, Updates, Props);
        'false' -> 'ok'
    end.

-spec handled_stat_should_update(call_stat()) -> boolean().
                                                % Handle stats where processed came in before handled (hungup quickly after pickup)
handled_stat_should_update(#call_stat{status= <<"processed">>}) -> 'false';
handled_stat_should_update(_) -> 'true'.

-spec handle_processed_stat(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_processed_stat(JObj, Props) ->
    'true' = kapi_acdc_stats:call_processed_v(JObj),

    Id = call_stat_id(JObj),
    Updates = props:filter_undefined(
                [{#call_stat.agent_id, kz_json:get_value(<<"Agent-ID">>, JObj)}
                ,{#call_stat.processed_timestamp, kz_json:get_value(<<"Processed-Timestamp">>, JObj)}
                ,{#call_stat.hung_up_by, kz_json:get_value(<<"Hung-Up-By">>, JObj)}
                ,{#call_stat.status, <<"processed">>}
                ]),

    Stat = find_call_stat(Id),
    Updates1 = processed_stat_maybe_fix_update(Stat, Updates),

    update_call_stat(Id, Updates1, Props).

-spec processed_stat_maybe_fix_update(call_stat(), kz_term:proplist()) -> kz_term:proplist().
processed_stat_maybe_fix_update(#call_stat{handled_timestamp='undefined'}, Updates) ->
                                                % Handle stats where processed came in before handled (hungup quickly after pickup)
    ProcessedTimestamp = props:get_integer_value(#call_stat.processed_timestamp, Updates),
    [{#call_stat.handled_timestamp, ProcessedTimestamp} | Updates];
processed_stat_maybe_fix_update(_, Updates) -> Updates.

-spec flush_call_stat(kz_json:object(), kz_term:proplist()) -> 'ok'.
flush_call_stat(JObj, Props) ->
    'true' = kapi_acdc_stats:call_flush_v(JObj),

    Id = call_stat_id(JObj),

    lager:debug("flushing ~s: ~p", [Id, JObj]),

    gen_listener:cast(props:get_value('server', Props)
                     ,{'flush_call', Id}
                     ).

-spec find_call_stat(kz_term:ne_binary()) -> 'undefined' | call_stat().
find_call_stat(Id) ->
    case ets:lookup(call_table_id(), Id) of
        [] -> 'undefined';
        [Stat] -> Stat
    end.

-spec create_call_stat(kz_term:ne_binary(), kz_json:object(), kz_term:proplist()) -> 'ok'.
create_call_stat(Id, JObj, Props) ->
    gen_listener:cast(props:get_value('server', Props)
                     ,{'create_call', #call_stat{id = Id
                                                ,call_id = kz_json:get_value(<<"Call-ID">>, JObj)
                                                ,account_id = kz_json:get_value(<<"Account-ID">>, JObj)
                                                ,queue_id = kz_json:get_value(<<"Queue-ID">>, JObj)
                                                ,entered_timestamp = kz_json:get_value(<<"Entered-Timestamp">>, JObj)
                                                ,misses = []
                                                ,status = <<"waiting">>
                                                ,caller_id_name = kz_json:get_value(<<"Caller-ID-Name">>, JObj)
                                                ,caller_id_number = kz_json:get_value(<<"Caller-ID-Number">>, JObj)
                                                ,caller_priority = kz_json:get_integer_value(<<"Caller-Priority">>, JObj)
                                                }
                      }).

-type updates() :: [{pos_integer(), any()}].
-spec update_call_stat(kz_term:ne_binary(), updates(), kz_term:proplist()) -> 'ok'.
update_call_stat(Id, Updates, Props) ->
    gen_listener:cast(props:get_value('server', Props), {'update_call', Id, Updates}).

call_state_change(AccountId, Status, Prop) ->
    Body = kz_json:normalize(kz_json:from_list([{<<"Event">>, <<"call_status_change">>}
                                               ,{<<"Status">>, kz_term:to_binary(Status)}
                                                | Prop
                                               ])),
    kz_edr:event(?APP_NAME, ?APP_VERSION, 'ok', 'info', Body, AccountId).
