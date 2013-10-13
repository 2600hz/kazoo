%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%% Collector of stats
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_stats).

-behaviour(gen_listener).

%% Public API
-export([call_waiting/5
         ,call_abandoned/4
         ,call_handled/4
         ,call_missed/5
         ,call_processed/4

         ,agent_ready/2
         ,agent_logged_in/2
         ,agent_logged_out/2
         ,agent_connecting/3, agent_connecting/5
         ,agent_connected/3, agent_connected/5
         ,agent_wrapup/3
         ,agent_paused/3
         ,agent_outbound/3
        ]).

%% ETS config
-export([call_table_id/0
         ,call_key_pos/0
         ,call_table_opts/0

         ,status_table_id/0
         ,status_key_pos/0
         ,status_table_opts/0

         ,init_db/1
         ,db_name/1
         ,archive_call_data/2
         ,archive_status_data/2
        ]).

%% AMQP Callbacks
-export([handle_call_stat/2
         ,handle_call_query/2

         ,handle_status_stat/2
         ,handle_status_query/2
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

-define(ARCHIVE_MSG, 'time_to_archive').
-define(CLEANUP_MSG, 'time_to_cleanup').

-define(VALID_STATUSES, [<<"waiting">>, <<"handled">>, <<"abandoned">>, <<"processed">>]).

-define(MAX_RESULT_SET, whapps_config:get_integer(?CONFIG_CAT, <<"max_result_set">>, 25)).

-record(agent_miss, {
          agent_id :: api_binary()
          ,miss_reason :: api_binary()
          ,miss_timestamp = wh_util:current_tstamp() :: pos_integer()
         }).
-type agent_miss() :: #agent_miss{}.
-type agent_misses() :: [agent_miss(),...] | [].

-record(call_stat, {
          id :: api_binary() | '_' %% call_id::queue_id
          ,call_id :: api_binary() | '_'
          ,acct_id :: api_binary() | '$1' | '_'
          ,queue_id :: api_binary() | '$2' | '_'

          ,agent_id :: api_binary() | '$3' | '_' % the handling agent

          ,entered_timestamp = wh_util:current_tstamp() :: pos_integer() | '$1' | '$5' | '_'
          ,abandoned_timestamp :: api_integer() | '_'
          ,handled_timestamp :: api_integer() | '_'
          ,processed_timestamp :: api_integer() | '_'

          ,abandoned_reason :: api_binary() | '_'

          ,misses = [] :: agent_misses() | '_'

          ,status :: api_binary() | '$2' | '$4' | '_'
          ,caller_id_name :: api_binary() | '_'
          ,caller_id_number :: api_binary() | '_'
          ,is_archived = 'false' :: boolean() | '$3' | '_'
         }).
-type call_stat() :: #call_stat{}.

-define(STATUS_STATUSES, [<<"logged_in">>, <<"logged_out">>, <<"ready">>
                          ,<<"connecting">>, <<"connected">>
                          ,<<"wrapup">>, <<"paused">>, <<"outbound">>
                         ]).
-record(status_stat, {
          id :: api_binary() | '_'
          ,agent_id :: api_binary() | '$2' | '_'
          ,acct_id :: api_binary() | '$1' | '_'
          ,status :: api_binary() | '$4' | '_'
          ,timestamp :: api_pos_integer() | '$1' | '$3' | '$5' | '_'

          ,wait_time :: api_integer() | '_'
          ,pause_time :: api_integer() | '_'
          ,callid :: api_binary() | '_'
          ,caller_id_name :: api_binary() | '_'
          ,caller_id_number :: api_binary() | '_'
          ,is_archived = 'false' :: boolean() | '$1' | '$2' | '_'
         }).
-type status_stat() :: #status_stat{}.

%% Public API
call_waiting(AcctId, QueueId, CallId, CallerIdName, CallerIdNumber) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
              ,{<<"Queue-ID">>, QueueId}
              ,{<<"Call-ID">>, CallId}
              ,{<<"Caller-ID-Name">>, CallerIdName}
              ,{<<"Caller-ID-Number">>, CallerIdNumber}
              ,{<<"Entered-Timestamp">>, wh_util:current_tstamp()}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    whapps_util:amqp_pool_send(Prop, fun wapi_acdc_stats:publish_call_waiting/1).

call_abandoned(AcctId, QueueId, CallId, Reason) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
              ,{<<"Queue-ID">>, QueueId}
              ,{<<"Call-ID">>, CallId}
              ,{<<"Abandon-Reason">>, Reason}
              ,{<<"Abandon-Timestamp">>, wh_util:current_tstamp()}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    whapps_util:amqp_pool_send(Prop, fun wapi_acdc_stats:publish_call_abandoned/1).

call_handled(AcctId, QueueId, CallId, AgentId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
              ,{<<"Queue-ID">>, QueueId}
              ,{<<"Call-ID">>, CallId}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Handled-Timestamp">>, wh_util:current_tstamp()}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    whapps_util:amqp_pool_send(Prop, fun wapi_acdc_stats:publish_call_handled/1).

call_missed(AcctId, QueueId, AgentId, CallId, ErrReason) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
              ,{<<"Queue-ID">>, QueueId}
              ,{<<"Call-ID">>, CallId}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Miss-Reason">>, ErrReason}
              ,{<<"Miss-Timestamp">>, wh_util:current_tstamp()}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    whapps_util:amqp_pool_send(Prop, fun wapi_acdc_stats:publish_call_missed/1).

call_processed(AcctId, QueueId, AgentId, CallId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
              ,{<<"Queue-ID">>, QueueId}
              ,{<<"Call-ID">>, CallId}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Processed-Timestamp">>, wh_util:current_tstamp()}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    whapps_util:amqp_pool_send(Prop, fun wapi_acdc_stats:publish_call_processed/1).

agent_ready(AcctId, AgentId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Timestamp">>, wh_util:current_tstamp()}
              ,{<<"Status">>, <<"ready">>}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    whapps_util:amqp_pool_send(Prop, fun wapi_acdc_stats:publish_status_ready/1).

agent_logged_in(AcctId, AgentId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Timestamp">>, wh_util:current_tstamp()}
              ,{<<"Status">>, <<"logged_in">>}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    whapps_util:amqp_pool_send(Prop, fun wapi_acdc_stats:publish_status_logged_in/1).

agent_logged_out(AcctId, AgentId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Timestamp">>, wh_util:current_tstamp()}
              ,{<<"Status">>, <<"logged_out">>}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    whapps_util:amqp_pool_send(Prop, fun wapi_acdc_stats:publish_status_logged_out/1).

agent_connecting(AcctId, AgentId, CallId) ->
    agent_connecting(AcctId, AgentId, CallId, 'undefined', 'undefined').
agent_connecting(AcctId, AgentId, CallId, CallerIDName, CallerIDNumber) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Timestamp">>, wh_util:current_tstamp()}
              ,{<<"Status">>, <<"connecting">>}
              ,{<<"Call-ID">>, CallId}
              ,{<<"Caller-ID-Name">>, CallerIDName}
              ,{<<"Caller-ID-Number">>, CallerIDNumber}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    whapps_util:amqp_pool_send(Prop, fun wapi_acdc_stats:publish_status_connecting/1).

agent_connected(AcctId, AgentId, CallId) ->
    agent_connected(AcctId, AgentId, CallId, 'undefined', 'undefined').
agent_connected(AcctId, AgentId, CallId, CallerIDName, CallerIDNumber) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Timestamp">>, wh_util:current_tstamp()}
              ,{<<"Status">>, <<"connected">>}
              ,{<<"Call-ID">>, CallId}
              ,{<<"Caller-ID-Name">>, CallerIDName}
              ,{<<"Caller-ID-Number">>, CallerIDNumber}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    whapps_util:amqp_pool_send(Prop, fun wapi_acdc_stats:publish_status_connected/1).

agent_wrapup(AcctId, AgentId, WaitTime) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Timestamp">>, wh_util:current_tstamp()}
              ,{<<"Status">>, <<"wrapup">>}
              ,{<<"Wait-Time">>, WaitTime}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    whapps_util:amqp_pool_send(Prop, fun wapi_acdc_stats:publish_status_wrapup/1).

agent_paused(AcctId, AgentId, 'undefined') ->
    lager:debug("undefined pause time for ~s(~s)", [AgentId, AcctId]);
agent_paused(AcctId, AgentId, PauseTime) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Timestamp">>, wh_util:current_tstamp()}
              ,{<<"Status">>, <<"paused">>}
              ,{<<"Pause-Time">>, PauseTime}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    whapps_util:amqp_pool_send(Prop, fun wapi_acdc_stats:publish_status_paused/1).

agent_outbound(AcctId, AgentId, CallId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AcctId}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Timestamp">>, wh_util:current_tstamp()}
              ,{<<"Status">>, <<"outbound">>}
              ,{<<"Call-ID">>, CallId}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    whapps_util:amqp_pool_send(Prop, fun wapi_acdc_stats:publish_status_outbound/1).

%% ETS config
call_table_id() -> 'acdc_stats_call'.
call_key_pos() -> #call_stat.id.
call_table_opts() ->
    ['protected', 'named_table'
     ,{'keypos', call_key_pos()}
    ].

status_table_id() -> 'acdc_stats_status'.
status_key_pos() -> #status_stat.id.
status_table_opts() ->
    ['protected', 'named_table'
     ,{'keypos', status_key_pos()}
    ].

-define(BINDINGS, [{'self', []}
                   ,{'acdc_stats', []}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_call_stat'}
                      ,[{<<"acdc_call_stat">>, <<"waiting">>}
                        ,{<<"acdc_call_stat">>, <<"missed">>}
                        ,{<<"acdc_call_stat">>, <<"abandoned">>}
                        ,{<<"acdc_call_stat">>, <<"handled">>}
                        ,{<<"acdc_call_stat">>, <<"processed">>}
                       ]
                     }
                     ,{{?MODULE, 'handle_status_stat'}
                       ,[{<<"acdc_status_stat">>, <<"ready">>}
                         ,{<<"acdc_status_stat">>, <<"logged_in">>}
                         ,{<<"acdc_status_stat">>, <<"logged_out">>}
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
                     ,{{?MODULE, 'handle_status_query'}
                       ,[{<<"acdc_stat">>, <<"status_req">>}]
                      }
                    ]).
-define(QUEUE_NAME, <<>>).

start_link() ->
    gen_listener:start_link(?MODULE
                            ,[{'bindings', ?BINDINGS}
                              ,{'responders', ?RESPONDERS}
                              ,{'queue_name', ?QUEUE_NAME}
                             ],
                            []).

-spec handle_call_stat(wh_json:object(), wh_proplist()) -> 'ok'.
handle_call_stat(JObj, Props) ->
    case wh_json:get_value(<<"Event-Name">>, JObj) of
        <<"waiting">> -> handle_waiting_stat(JObj, Props);
        <<"missed">> -> handle_missed_stat(JObj, Props);
        <<"abandoned">> -> handle_abandoned_stat(JObj, Props);
        <<"handled">> -> handle_handled_stat(JObj, Props);
        <<"processed">> -> handle_processed_stat(JObj, Props);
        _Name ->
            lager:debug("recv unknown call stat type ~s: ~p", [_Name, JObj])
    end.

-spec handle_status_stat(wh_json:object(), wh_proplist()) -> 'ok'.
handle_status_stat(JObj, Props) ->
    'true' = case (EventName = wh_json:get_value(<<"Event-Name">>, JObj)) of
                 <<"ready">> -> wapi_acdc_stats:status_ready_v(JObj);
                 <<"logged_in">> -> wapi_acdc_stats:status_logged_in_v(JObj);
                 <<"logged_out">> -> wapi_acdc_stats:status_logged_out_v(JObj);
                 <<"connecting">> -> wapi_acdc_stats:status_connecting_v(JObj);
                 <<"connected">> -> wapi_acdc_stats:status_connected_v(JObj);
                 <<"wrapup">> -> wapi_acdc_stats:status_wrapup_v(JObj);
                 <<"paused">> -> wapi_acdc_stats:status_paused_v(JObj);
                 <<"outbound">> -> wapi_acdc_stats:status_outbound_v(JObj);
                 _Name ->
                     lager:debug("recv unknown status stat type ~s: ~p", [_Name, JObj]),
                     'false'
             end,

    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    Timestamp = wh_json:get_integer_value(<<"Timestamp">>, JObj),

    gen_listener:cast(props:get_value('server', Props)
                      ,{'create_status', #status_stat{id=status_stat_id(AgentId, Timestamp, EventName)
                                                      ,agent_id=AgentId
                                                      ,acct_id=wh_json:get_value(<<"Account-ID">>, JObj)
                                                      ,status=EventName
                                                      ,timestamp=Timestamp
                                                      ,callid=wh_json:get_value(<<"Call-ID">>, JObj)
                                                      ,wait_time=wait_time(EventName, JObj)
                                                      ,pause_time=pause_time(EventName, JObj)
                                                      ,caller_id_name=caller_id_name(EventName, JObj)
                                                      ,caller_id_number=caller_id_number(EventName, JObj)
                                                     }}).

-spec wait_time(ne_binary(), wh_json:object()) -> api_integer().
wait_time(<<"paused">>, _) -> 'undefined';
wait_time(_, JObj) -> wh_json:get_integer_value(<<"Wait-Time">>, JObj).

-spec pause_time(ne_binary(), wh_json:object()) -> api_integer().
pause_time(<<"paused">>, JObj) ->
    case wh_json:get_integer_value(<<"Pause-Time">>, JObj) of
        'undefined' -> wh_json:get_integer_value(<<"Wait-Time">>, JObj);
        PT -> PT
    end;
pause_time(_, _JObj) -> 'undefined'.

caller_id_name(_, JObj) ->
    wh_json:get_value(<<"Caller-ID-Name">>, JObj).
caller_id_number(_, JObj) ->
    wh_json:get_value(<<"Caller-ID-Number">>, JObj).

-spec handle_call_query(wh_json:object(), wh_proplist()) -> 'ok'.
handle_call_query(JObj, _Prop) ->
    'true' = wapi_acdc_stats:current_calls_req_v(JObj),
    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
    MsgId = wh_json:get_value(<<"Msg-ID">>, JObj),
    Limit = get_query_limit(JObj),

    case call_build_match_spec(JObj) of
        {'ok', Match} -> query_calls(RespQ, MsgId, Match, Limit);
        {'error', Errors} -> publish_query_errors(RespQ, MsgId, Errors)
    end.

-spec handle_status_query(wh_json:object(), wh_proplist()) -> 'ok'.
handle_status_query(JObj, _Prop) ->
    'true' = wapi_acdc_stats:status_req_v(JObj),
    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
    MsgId = wh_json:get_value(<<"Msg-ID">>, JObj),
    Limit = get_query_limit(JObj),

    case status_build_match_spec(JObj) of
        {'ok', Match} -> query_statuses(RespQ, MsgId, Match, Limit);
        {'error', Errors} -> publish_query_errors(RespQ, MsgId, Errors)
    end.

get_query_limit(JObj) ->
    Max = ?MAX_RESULT_SET,
    case wh_json:get_integer_value(<<"Limit">>, JObj) of
        'undefined' -> Max;
        N when N > Max -> Max;
        N when N < 1 -> 1;
        N -> N
    end.

-record(state, {
          archive_ref :: reference()
          ,cleanup_ref :: reference()
          ,call_table_id :: ets:table_id()
          ,status_table_id :: ets:table_id()
         }).

init([]) ->
    put('callid', <<"acdc.stats">>),
    couch_mgr:suppress_change_notice(),
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

handle_call(_Req, _From, State) ->
    {'reply', 'ok', State}.

handle_cast({'create_call', #call_stat{id=_Id}=Stat}, State) ->
    lager:debug("creating new call stat ~s", [_Id]),
    ets:insert_new(call_table_id(), Stat),
    {'noreply', State};
handle_cast({'create_status', #status_stat{id=_Id, status=_Status}=Stat}, State) ->
    lager:debug("creating new status stat ~s: ~s", [_Id, _Status]),
    case ets:insert_new(status_table_id(), Stat) of
        'true' -> {'noreply', State};
        'false' ->
            lager:debug("stat ~s already exists, updating", [_Id]),
            ets:insert(status_table_id(), Stat),
            {'noreply', State}
    end;
handle_cast({'update_call', Id, Updates}, State) ->
    lager:debug("updating call stat ~s: ~p", [Id, Updates]),
    ets:update_element(call_table_id(), Id, Updates),
    {'noreply', State};
handle_cast({'remove_call', [{M, P, _}]}, State) ->
    Match = [{M, P, ['true']}],
    lager:debug("removing call stats from table"),
    N = ets:select_delete(call_table_id(), Match),
    lager:debug("removed calls (or not): ~p", [N]),
    {'noreply', State};

handle_cast({'update_status', Id, Updates}, State) ->
    lager:debug("updating status stat ~s: ~p", [Id, Updates]),
    ets:update_element(status_table_id(), Id, Updates),
    {'noreply', State};
handle_cast({'remove_status', [{M, P, _}]}, State) ->
    Match = [{M, P, ['true']}],
    lager:debug("removing status stats from table"),
    N = ets:select_delete(status_table_id(), Match),
    lager:debug("removed statuses (or not): ~p", [N]),
    {'noreply', State};

handle_cast({'wh_amqp_channel',{'new_channel',_IsNew}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'created_queue',_Q}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Req, State) ->
    lager:debug("unhandled cast: ~p", [_Req]),
    {'noreply', State}.

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
    lager:debug("unhandling message: ~p", [_Msg]),
    {'noreply', State}.

handle_event(_JObj, _State) ->
    {'reply', []}.

terminate(_Reason, _) ->
    force_archive_data(),
    lager:debug("acdc stats terminating: ~p", [_Reason]).

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

publish_query_errors(RespQ, MsgId, Errors) ->
    API = [{<<"Error-Reason">>, Errors}
           ,{<<"Msg-ID">>, MsgId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("responding with errors to req ~s: ~p", [MsgId, Errors]),
    wapi_acdc_stats:publish_current_calls_err(RespQ, API).

call_build_match_spec(JObj) ->
    case wh_json:get_value(<<"Account-ID">>, JObj) of
        'undefined' ->
            {'error', wh_json:from_list([{<<"Account-ID">>, <<"missing but required">>}])};
        AccountId ->
            AcctMatch = {#call_stat{acct_id='$1', _='_'}
                         ,[{'=:=', '$1', {'const', AccountId}}]
                        },
            call_build_match_spec(JObj, AcctMatch)
    end.

-spec call_build_match_spec(wh_json:object(), {call_stat(), list()}) ->
                                   {'ok', ets:match_spec()} |
                                   {'error', wh_json:object()}.
call_build_match_spec(JObj, AcctMatch) ->
    case wh_json:foldl(fun call_match_builder_fold/3, AcctMatch, JObj) of
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
            {'error', wh_json:from_list([{<<"Status">>, <<"unknown status supplied">>}])}
    end;
call_match_builder_fold(<<"Start-Range">>, Start, {CallStat, Contstraints}) ->
    Now = wh_util:current_tstamp(),
    Past = Now - ?CLEANUP_WINDOW,

    try wh_util:to_integer(Start) of
        N when N < Past ->
            {'error', wh_json:from_list([{<<"Start-Range">>, <<"supplied value is too far in the past">>}
                                         ,{<<"Window-Size">>, ?CLEANUP_WINDOW}
                                         ,{<<"Current-Timestamp">>, Now}
                                         ,{<<"Past-Timestamp">>, Past}
                                        ])};
        N when N > Now ->
            {'error', wh_json:from_list([{<<"Start-Range">>, <<"supplied value is in the future">>}
                                         ,{<<"Current-Timestamp">>, Now}
                                        ])};
        N ->
            {CallStat#call_stat{entered_timestamp='$5'}
             ,[{'>=', '$5', N} | Contstraints]
            }
    catch
        _:_ ->
            {'error', wh_json:from_list([{<<"Start-Range">>, <<"supplied value is not an integer">>}])}
    end;
call_match_builder_fold(<<"End-Range">>, End, {CallStat, Contstraints}) ->
    Now = wh_util:current_tstamp(),
    Past = Now - ?CLEANUP_WINDOW,

    try wh_util:to_integer(End) of
        N when N < Past ->
            {'error', wh_json:from_list([{<<"End-Range">>, <<"supplied value is too far in the past">>}
                                         ,{<<"Window-Size">>, ?CLEANUP_WINDOW}
                                         ,{<<"Current-Timestamp">>, Now}
                                        ])};
        N when N > Now ->
            {'error', wh_json:from_list([{<<"End-Range">>, <<"supplied value is in the future">>}
                                         ,{<<"Current-Timestamp">>, Now}
                                        ])};
        N ->
            {CallStat#call_stat{entered_timestamp='$5'}
             ,[{'=<', '$5', N} | Contstraints]
            }
    catch
        _:_ ->
            {'error', wh_json:from_list([{<<"End-Range">>, <<"supplied value is not an integer">>}])}
    end;
call_match_builder_fold(_, _, Acc) -> Acc.

is_valid_call_status(S) ->
    Status = wh_util:to_lower_binary(S),
    case lists:member(Status, ?VALID_STATUSES) of
        'true' -> {'true', Status};
        'false' -> 'false'
    end.

-spec query_calls(ne_binary(), ne_binary(), ets:match_spec(), pos_integer()) -> 'ok'.
query_calls(RespQ, MsgId, Match, _Limit) ->
    case ets:select(call_table_id(), Match) of
        [] ->
            lager:debug("no stats found, sorry ~s", [RespQ]),
            Resp = [{<<"Query-Time">>, wh_util:current_tstamp()}
                    ,{<<"Msg-ID">>, MsgId}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_acdc_stats:publish_current_calls_resp(RespQ, Resp);
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
                    ,{<<"Query-Time">>, wh_util:current_tstamp()}
                    ,{<<"Msg-ID">>, MsgId}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],

            wapi_acdc_stats:publish_current_calls_resp(RespQ, Resp)
    end.

status_build_match_spec(JObj) ->
    case wh_json:get_value(<<"Account-ID">>, JObj) of
        'undefined' ->
            {'error', wh_json:from_list([{<<"Account-ID">>, <<"missing but required">>}])};
        AccountId ->
            AcctMatch = {#status_stat{acct_id='$1', _='_'}
                         ,[{'=:=', '$1', {'const', AccountId}}]
                        },
            status_build_match_spec(JObj, AcctMatch)
    end.

-spec status_build_match_spec(wh_json:object(), {status_stat(), list()}) ->
                                     {'ok', ets:match_spec()} |
                                     {'error', wh_json:object()}.
status_build_match_spec(JObj, AcctMatch) ->
    case wh_json:foldl(fun status_match_builder_fold/3, AcctMatch, JObj) of
        {'error', _Errs}=Errors -> Errors;
        {StatusStat, Constraints} -> {'ok', [{StatusStat, Constraints, ['$_']}]}
    end.

status_match_builder_fold(_, _, {'error', _Err}=E) -> E;
status_match_builder_fold(<<"Agent-ID">>, AgentId, {StatusStat, Contstraints}) ->
    {StatusStat#status_stat{agent_id='$2'}
     ,[{'=:=', '$2', {'const', AgentId}} | Contstraints]
    };
status_match_builder_fold(<<"Start-Range">>, Start, {StatusStat, Contstraints}) ->
    Now = wh_util:current_tstamp(),
    Past = Now - ?CLEANUP_WINDOW,

    try wh_util:to_integer(Start) of
        N when N < Past ->
            {'error', wh_json:from_list([{<<"Start-Range">>, <<"supplied value is too far in the past">>}
                                         ,{<<"Window-Size">>, ?CLEANUP_WINDOW}
                                         ,{<<"Current-Timestamp">>, Now}
                                         ,{<<"Past-Timestamp">>, Past}
                                         ,{<<"Start-Range">>, N}
                                        ])};
        N when N > Now ->
            {'error', wh_json:from_list([{<<"Start-Range">>, <<"supplied value is in the future">>}
                                         ,{<<"Current-Timestamp">>, Now}
                                        ])};
        N ->
            {StatusStat#status_stat{timestamp='$3'}
             ,[{'>=', '$3', N} | Contstraints]
            }
    catch
        _:_ ->
            {'error', wh_json:from_list([{<<"Start-Range">>, <<"supplied value is not an integer">>}])}
    end;
status_match_builder_fold(<<"End-Range">>, End, {StatusStat, Contstraints}) ->
    Now = wh_util:current_tstamp(),
    Past = Now - ?CLEANUP_WINDOW,

    try wh_util:to_integer(End) of
        N when N < Past ->
            {'error', wh_json:from_list([{<<"End-Range">>, <<"supplied value is too far in the past">>}
                                         ,{<<"Window-Size">>, ?CLEANUP_WINDOW}
                                         ,{<<"Current-Timestamp">>, Now}
                                        ])};
        N when N > Now ->
            {'error', wh_json:from_list([{<<"End-Range">>, <<"supplied value is in the future">>}
                                         ,{<<"Current-Timestamp">>, Now}
                                        ])};
        N ->
            {StatusStat#status_stat{timestamp='$3'}
             ,[{'=<', '$3', N} | Contstraints]
            }
    catch
        _:_ ->
            {'error', wh_json:from_list([{<<"End-Range">>, <<"supplied value is not an integer">>}])}
    end;
status_match_builder_fold(<<"Status">>, Status, {StatusStat, Contstraints}) ->
    {StatusStat#status_stat{status='$4'}
     ,[{'=:=', '$4', {'const', Status}} | Contstraints]
    };
status_match_builder_fold(_, _, Acc) -> Acc.

-spec query_statuses(ne_binary(), ne_binary(), ets:match_spec(), pos_integer()) -> 'ok'.
query_statuses(RespQ, MsgId, Match, Limit) ->
    case ets:select(status_table_id(), Match) of
        [] ->
            lager:debug("no stats found, sorry ~s", [RespQ]),
            Resp = [{<<"Error-Reason">>, <<"No agents found">>}
                    ,{<<"Msg-ID">>, MsgId}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_acdc_stats:publish_status_err(RespQ, Resp);
        Stats ->
            QueryResults = lists:foldl(fun query_status_fold/2, wh_json:new(), Stats),
            LimitList = lists:duplicate(Limit, 0),
            TrimmedResults = wh_json:map(fun(A, B) ->
                                                 trim_query_statuses(A, B, LimitList)
                                         end, QueryResults),

            Resp = [{<<"Agents">>, TrimmedResults}
                    ,{<<"Msg-ID">>, MsgId}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_acdc_stats:publish_status_resp(RespQ, Resp)
    end.

trim_query_statuses(A, Statuses, Limit) ->
    {_, Trimmed} = wh_json:foldl(fun trim_query_statuses_fold/3
                                 ,{Limit
                                   ,wh_json:new()
                                  }, Statuses),
    {A, Trimmed}.

trim_query_statuses_fold(TBin, Datum, {Ks, Data}=Acc) ->
    T = wh_util:to_integer(TBin),
    case lists:min(Ks) of
        N when N < T ->
            {[T | lists:delete(N, Ks)]
             ,wh_json:set_value(TBin
                                ,wh_doc:public_fields(Datum)
                                ,wh_json:delete_key(N, Data)
                               )};
        _ -> Acc
    end.

-spec archive_data() -> 'ok'.
archive_data() ->
    Self = self(),
    _ = spawn(?MODULE, 'archive_call_data', [Self, 'false']),
    _ = spawn(?MODULE, 'archive_status_data', [Self, 'false']),
    'ok'.

force_archive_data() ->
    Self = self(),
    _ = spawn(?MODULE, 'archive_call_data', [Self, 'true']),
    _ = spawn(?MODULE, 'archive_status_data', [Self, 'true']),
    'ok'.

cleanup_data(Srv) ->
    Past = wh_util:current_tstamp() - ?CLEANUP_WINDOW,
    PastConstraint = {'=<', '$1', Past},

    TypeConstraints = [{'=/=', '$2', {'const', <<"waiting">>}}
                       ,{'=/=', '$2', {'const', <<"handled">>}}
                      ],

    CallMatch = [{#call_stat{entered_timestamp='$1', status='$2', _='_'}
                  ,[PastConstraint | TypeConstraints]
                  ,['$_']
                 }],
    gen_listener:cast(Srv, {'remove_call', CallMatch}),

    StatusMatch = [{#status_stat{timestamp='$1', _='_'}
                    ,[{'=<', '$1', Past}]
                    ,['$_']
                   }],
    gen_listener:cast(Srv, {'remove_status', StatusMatch}),

    case ets:select(?MODULE:call_table_id()
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

archive_call_data(Srv, 'true') ->
    put('callid', <<"acdc_stats.force_call_archiver">>),

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
    put('callid', <<"acdc_stats.call_archiver">>),

    Past = wh_util:current_tstamp() - ?ARCHIVE_WINDOW,
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
            couch_mgr:suppress_change_notice(),
            ToSave = lists:foldl(fun archive_call_fold/2, dict:new(), Stats),
            [couch_mgr:save_docs(db_name(Acct), Docs)
             || {Acct, Docs} <- dict:to_list(ToSave)
            ],
            [gen_listener:cast(Srv, {'update_call', Id, [{#call_stat.is_archived, 'true'}]})
             || #call_stat{id=Id} <- Stats
            ]
    end.

-spec query_call_fold(call_stat(), dict()) -> dict().
query_call_fold(#call_stat{status=Status}=Stat, Acc) ->
    Doc = call_stat_to_doc(Stat),
    dict:update(Status, fun(L) -> [Doc | L] end, [Doc], Acc).

-spec query_status_fold(status_stat(), wh_json:object()) -> wh_json:object().
query_status_fold(#status_stat{agent_id=AgentId
                               ,timestamp=T
                              }=Stat, Acc) ->
    Doc = status_stat_to_doc(Stat),
    wh_json:set_value([AgentId, wh_util:to_binary(T)], Doc, Acc).

-spec archive_call_fold(call_stat(), dict()) -> dict().
archive_call_fold(#call_stat{acct_id=AcctId}=Stat, Acc) ->
    Doc = call_stat_to_doc(Stat),
    dict:update(AcctId, fun(L) -> [Doc | L] end, [Doc], Acc).

-spec call_stat_to_doc(call_stat()) -> wh_json:object().
call_stat_to_doc(#call_stat{id=Id
                            ,call_id=CallId
                            ,acct_id=AcctId
                            ,queue_id=QueueId
                            ,agent_id=AgentId
                            ,entered_timestamp=EnteredT
                            ,abandoned_timestamp=AbandonedT
                            ,handled_timestamp=HandledT
                            ,processed_timestamp=ProcessedT
                            ,abandoned_reason=AbandonedR
                            ,misses=Misses
                            ,status=Status
                            ,caller_id_name=CallerIdName
                            ,caller_id_number=CallerIdNumber
                           }) ->
    wh_doc:update_pvt_parameters(
      wh_json:from_list(
        props:filter_undefined(
          [{<<"_id">>, Id}
           ,{<<"call_id">>, CallId}
           ,{<<"queue_id">>, QueueId}
           ,{<<"agent_id">>, AgentId}
           ,{<<"entered_timestamp">>, EnteredT}
           ,{<<"abandoned_timestamp">>, AbandonedT}
           ,{<<"handled_timestamp">>, HandledT}
           ,{<<"processed_timestamp">>, ProcessedT}
           ,{<<"abandoned_reason">>, AbandonedR}
           ,{<<"misses">>, misses_to_docs(Misses)}
           ,{<<"status">>, Status}
           ,{<<"caller_id_name">>, CallerIdName}
           ,{<<"caller_id_number">>, CallerIdNumber}
           ,{<<"wait_time">>, wait_time(EnteredT, AbandonedT, HandledT)}
           ,{<<"talk_time">>, talk_time(HandledT, ProcessedT)}
          ]))
      ,db_name(AcctId)
      ,[{'account_id', AcctId}
        ,{'type', <<"call_stat">>}
       ]).

archive_status_data(Srv, 'true') ->
    put('callid', <<"acdc_stats.force_status_archiver">>),

    Match = [{#status_stat{is_archived='$1'
                           ,_='_'
                          }
              ,[{'=:=', '$1', 'false'}]
              ,['$_']
             }],
    maybe_archive_status_data(Srv, Match);
archive_status_data(Srv, 'false') ->
    put('callid', <<"acdc_stats.status_archiver">>),

    Past = wh_util:current_tstamp() - ?ARCHIVE_WINDOW,
    Match = [{#status_stat{timestamp='$1'
                           ,is_archived='$2'
                           ,_='_'
                          }
              ,[{'=<', '$1', Past}
                ,{'=:=', '$2', 'false'}
               ]
              ,['$_']
             }],
    maybe_archive_status_data(Srv, Match).

maybe_archive_status_data(Srv, Match) ->
    case ets:select(status_table_id(), Match) of
        [] -> 'ok';
        Stats ->
            couch_mgr:suppress_change_notice(),
            ToSave = lists:foldl(fun archive_status_fold/2, dict:new(), Stats),
            [couch_mgr:save_docs(db_name(Acct), Docs)
             || {Acct, Docs} <- dict:to_list(ToSave)
            ],
            [gen_listener:cast(Srv, {'update_status', Id, [{#status_stat.is_archived, 'true'}]})
             || #status_stat{id=Id} <- Stats
            ]
    end.

-spec archive_status_fold(status_stat(), dict()) -> dict().
archive_status_fold(#status_stat{acct_id=AcctId}=Stat, Acc) ->
    Doc = status_stat_to_doc(Stat),
    dict:update(AcctId, fun(L) -> [Doc | L] end, [Doc], Acc).

-spec status_stat_to_doc(status_stat()) -> wh_json:object().
status_stat_to_doc(#status_stat{id=Id
                                ,agent_id=AgentId
                                ,acct_id=AcctId
                                ,status=Status
                                ,timestamp=Timestamp
                                ,wait_time=WT
                                ,pause_time=PT
                                ,callid=CallId
                                ,caller_id_name=CIDName
                                ,caller_id_number=CIDNum
                               }) ->
    Prop = [{<<"_id">>, Id}
            ,{<<"call_id">>, CallId}
            ,{<<"agent_id">>, AgentId}
            ,{<<"timestamp">>, Timestamp}
            ,{<<"status">>, Status}
            ,{<<"wait_time">>, WT}
            ,{<<"pause_time">>, PT}
            ,{<<"caller_id_name">>, CIDName}
            ,{<<"caller_id_number">>, CIDNum}
           ],
    wh_doc:update_pvt_parameters(
      wh_json:from_list(props:filter_undefined(Prop))
      ,db_name(AcctId)
      ,[{'account_id', AcctId}
        ,{'type', <<"status_stat">>}
       ]).

wait_time(E, _, H) when is_integer(E), is_integer(H) -> H - E;
wait_time(E, A, _) when is_integer(E), is_integer(A) -> A - E;
wait_time(_, _, _) -> 'undefined'.

talk_time(H, P) when is_integer(H), is_integer(P) -> P - H;
talk_time(_, _) -> 'undefined'.

-spec misses_to_docs(agent_misses()) -> wh_json:objects().
-spec miss_to_doc(agent_miss()) -> wh_json:object().
misses_to_docs(Misses) -> [miss_to_doc(Miss) || Miss <- Misses].
miss_to_doc(#agent_miss{agent_id=AgentId
                        ,miss_reason=Reason
                        ,miss_timestamp=T
                        }) ->
    wh_json:from_list([{<<"agent_id">>, AgentId}
                       ,{<<"reason">>, Reason}
                       ,{<<"timestamp">>, T}
                      ]).

-spec init_db(ne_binary()) -> 'ok'.
init_db(AcctId) ->
    DbName = db_name(AcctId),
    lager:debug("created db ~s: ~s", [DbName, couch_mgr:db_create(DbName)]),
    lager:debug("revised docs in ~s: ~p", [AcctId, couch_mgr:revise_views_from_folder(DbName, 'acdc')]).

-spec db_name(ne_binary()) -> ne_binary().
db_name(Acct) ->
    <<A:2/binary, B:2/binary, Rest/binary>> = wh_util:format_account_id(Acct, 'raw'),
    <<"acdc%2F",A/binary,"%2F",B/binary,"%2F", Rest/binary>>.

-spec call_stat_id(wh_json:object()) -> ne_binary().
-spec call_stat_id(ne_binary(), ne_binary()) -> ne_binary().
call_stat_id(JObj) ->
    call_stat_id(wh_json:get_value(<<"Call-ID">>, JObj)
            ,wh_json:get_value(<<"Queue-ID">>, JObj)
           ).
call_stat_id(CallId, QueueId) -> <<CallId/binary, "::", QueueId/binary>>.

status_stat_id(AgentId, Timestamp, _EventName) ->
    <<AgentId/binary, "::", (wh_util:to_binary(Timestamp))/binary>>.

handle_waiting_stat(JObj, Props) ->
    'true' = wapi_acdc_stats:call_waiting_v(JObj),

    Id = call_stat_id(JObj),
    case find_call_stat(Id) of
        'undefined' -> create_call_stat(Id, JObj, Props);
        _Stat ->
            Updates = props:filter_undefined(
                        [{#call_stat.caller_id_name, wh_json:get_value(<<"Caller-ID-Name">>, JObj)}
                         ,{#call_stat.caller_id_number, wh_json:get_value(<<"Caller-ID-Number">>, JObj)}
                        ]),
            update_call_stat(Id, Updates, Props)
    end.

handle_missed_stat(JObj, Props) ->
    'true' = wapi_acdc_stats:call_missed_v(JObj),

    Id = call_stat_id(JObj),
    case find_call_stat(Id) of
        'undefined' -> lager:debug("can't update stat ~s with missed data, missing", [Id]);
        #call_stat{misses=Misses} ->
            Updates = [{#call_stat.misses, [create_miss(JObj) | Misses]}],
            update_call_stat(Id, Updates, Props)
    end.

-spec create_miss(wh_json:object()) -> agent_miss().
create_miss(JObj) ->
    #agent_miss{
       agent_id = wh_json:get_value(<<"Agent-ID">>, JObj)
       ,miss_reason = wh_json:get_value(<<"Miss-Reason">>, JObj)
       ,miss_timestamp = wh_json:get_value(<<"Miss-Timestamp">>, JObj)
      }.

handle_abandoned_stat(JObj, Props) ->
    'true' = wapi_acdc_stats:call_abandoned_v(JObj),

    Id = call_stat_id(JObj),
    Updates = props:filter_undefined(
                [{#call_stat.abandoned_reason, wh_json:get_value(<<"Abandon-Reason">>, JObj)}
                 ,{#call_stat.abandoned_timestamp, wh_json:get_value(<<"Abandon-Timestamp">>, JObj)}
                 ,{#call_stat.status, <<"abandoned">>}
                ]),
    update_call_stat(Id, Updates, Props).

handle_handled_stat(JObj, Props) ->
    'true' = wapi_acdc_stats:call_handled_v(JObj),

    Id = call_stat_id(JObj),
    Updates = props:filter_undefined(
                [{#call_stat.agent_id, wh_json:get_value(<<"Agent-ID">>, JObj)}
                 ,{#call_stat.handled_timestamp, wh_json:get_value(<<"Handled-Timestamp">>, JObj)}
                 ,{#call_stat.status, <<"handled">>}
                ]),
    update_call_stat(Id, Updates, Props).

handle_processed_stat(JObj, Props) ->
    'true' = wapi_acdc_stats:call_processed_v(JObj),

    Id = call_stat_id(JObj),
    Updates = props:filter_undefined(
                [{#call_stat.agent_id, wh_json:get_value(<<"Agent-ID">>, JObj)}
                 ,{#call_stat.processed_timestamp, wh_json:get_value(<<"Processed-Timestamp">>, JObj)}
                 ,{#call_stat.status, <<"processed">>}
                ]),
    update_call_stat(Id, Updates, Props).

-spec find_call_stat(ne_binary()) -> 'undefined' | call_stat().
find_call_stat(Id) ->
    case ets:lookup(call_table_id(), Id) of
        [] -> 'undefined';
        [Stat] -> Stat
    end.

-spec create_call_stat(ne_binary(), wh_json:object(), wh_proplist()) -> 'ok'.
create_call_stat(Id, JObj, Props) ->
    gen_listener:cast(props:get_value('server', Props)
                      ,{'create_call', #call_stat{
                                          id = Id
                                          ,call_id = wh_json:get_value(<<"Call-ID">>, JObj)
                                          ,acct_id = wh_json:get_value(<<"Account-ID">>, JObj)
                                          ,queue_id = wh_json:get_value(<<"Queue-ID">>, JObj)
                                          ,entered_timestamp = wh_json:get_value(<<"Entered-Timestamp">>, JObj)
                                          ,misses = []
                                          ,status = <<"waiting">>
                                          ,caller_id_name = wh_json:get_value(<<"Caller-ID-Name">>, JObj)
                                          ,caller_id_number = wh_json:get_value(<<"Caller-ID-Number">>, JObj)
                                         }
                       }).

-spec update_call_stat(ne_binary(), wh_proplist(), wh_proplist()) -> 'ok'.
update_call_stat(Id, Updates, Props) ->
    gen_listener:cast(props:get_value('server', Props)
                      ,{'update_call', Id, Updates}
                     ).
