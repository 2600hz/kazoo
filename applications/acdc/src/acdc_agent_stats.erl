%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc Collector of stats for agents
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_agent_stats).

-export([agent_ready/2
        ,agent_logged_in/2
        ,agent_logged_out/2
        ,agent_pending_logged_out/2
        ,agent_connecting/3, agent_connecting/6
        ,agent_connected/3, agent_connected/6
        ,agent_wrapup/3
        ,agent_paused/3
        ,agent_outbound/3

        ,handle_status_stat/2
        ,handle_status_query/2

        ,status_stat_id/3

        ,status_table_id/0
        ,status_key_pos/0
        ,status_table_opts/0

        ,archive_status_data/2
        ]).

-include("acdc.hrl").
-include("acdc_stats.hrl").

-spec status_table_id() -> atom().
status_table_id() -> 'acdc_stats_status'.

-spec status_key_pos() -> pos_integer().
status_key_pos() -> #status_stat.id.

-spec status_table_opts() -> kz_term:proplist().
status_table_opts() ->
    ['protected', 'named_table'
    ,{'keypos', status_key_pos()}
    ].

-spec agent_ready(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
agent_ready(AccountId, AgentId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Timestamp">>, kz_time:now_s()}
             ,{<<"Status">>, <<"ready">>}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    log_status_change(AccountId, Prop),
    kz_amqp_worker:cast(Prop
                       ,fun kapi_acdc_stats:publish_status_ready/1
                       ).

-spec agent_logged_in(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
agent_logged_in(AccountId, AgentId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Timestamp">>, kz_time:now_s()}
             ,{<<"Status">>, <<"logged_in">>}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    log_status_change(AccountId, Prop),
    kz_amqp_worker:cast(Prop
                       ,fun kapi_acdc_stats:publish_status_logged_in/1
                       ).

-spec agent_logged_out(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
agent_logged_out(AccountId, AgentId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Timestamp">>, kz_time:now_s()}
             ,{<<"Status">>, <<"logged_out">>}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    log_status_change(AccountId, Prop),
    kz_amqp_worker:cast(Prop
                       ,fun kapi_acdc_stats:publish_status_logged_out/1
                       ).

-spec agent_pending_logged_out(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                      'ok'.
agent_pending_logged_out(AccountId, AgentId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Timestamp">>, kz_time:now_s()}
             ,{<<"Status">>, <<"pending_logged_out">>}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    log_status_change(AccountId, Prop),
    kz_amqp_worker:cast(Prop
                       ,fun kapi_acdc_stats:publish_status_pending_logged_out/1
                       ).

-spec agent_connecting(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                              'ok'.
agent_connecting(AccountId, AgentId, CallId) ->
    agent_connecting(AccountId, AgentId, CallId, 'undefined', 'undefined', 'undefined').
-spec agent_connecting(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()) ->
                              'ok'.
agent_connecting(AccountId, AgentId, CallId, CallerIDName, CallerIDNumber, QueueId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Timestamp">>, kz_time:now_s()}
             ,{<<"Status">>, <<"connecting">>}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Caller-ID-Name">>, CallerIDName}
             ,{<<"Caller-ID-Number">>, CallerIDNumber}
             ,{<<"Queue-ID">>, QueueId}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    log_status_change(AccountId, Prop),
    kz_amqp_worker:cast(Prop
                       ,fun kapi_acdc_stats:publish_status_connecting/1
                       ).

-spec agent_connected(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                             'ok'.
agent_connected(AccountId, AgentId, CallId) ->
    agent_connected(AccountId, AgentId, CallId, 'undefined', 'undefined', 'undefined').
-spec agent_connected(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()) ->
                             'ok'.
agent_connected(AccountId, AgentId, CallId, CallerIDName, CallerIDNumber, QueueId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Timestamp">>, kz_time:now_s()}
             ,{<<"Status">>, <<"connected">>}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Caller-ID-Name">>, CallerIDName}
             ,{<<"Caller-ID-Number">>, CallerIDNumber}
             ,{<<"Queue-ID">>, QueueId}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    log_status_change(AccountId, Prop),
    kz_amqp_worker:cast(Prop
                       ,fun kapi_acdc_stats:publish_status_connected/1
                       ).

-spec agent_wrapup(kz_term:ne_binary(), kz_term:ne_binary(), integer()) -> 'ok'.
agent_wrapup(AccountId, AgentId, WaitTime) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Timestamp">>, kz_time:now_s()}
             ,{<<"Status">>, <<"wrapup">>}
             ,{<<"Wait-Time">>, WaitTime}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    log_status_change(AccountId, Prop),
    kz_amqp_worker:cast(Prop
                       ,fun kapi_acdc_stats:publish_status_wrapup/1
                       ).

-spec agent_paused(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_integer()) -> 'ok'.
agent_paused(AccountId, AgentId, 'undefined') ->
    lager:debug("undefined pause time for ~s(~s)", [AgentId, AccountId]);
agent_paused(AccountId, AgentId, PauseTime) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Timestamp">>, kz_time:now_s()}
             ,{<<"Status">>, <<"paused">>}
             ,{<<"Pause-Time">>, PauseTime}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    log_status_change(AccountId, Prop),
    kz_amqp_worker:cast(Prop
                       ,fun kapi_acdc_stats:publish_status_paused/1
                       ).

-spec agent_outbound(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
agent_outbound(AccountId, AgentId, CallId) ->
    Prop = props:filter_undefined(
             [{<<"Account-ID">>, AccountId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Timestamp">>, kz_time:now_s()}
             ,{<<"Status">>, <<"outbound">>}
             ,{<<"Call-ID">>, CallId}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    log_status_change(AccountId, Prop),
    kz_amqp_worker:cast(Prop
                       ,fun kapi_acdc_stats:publish_status_outbound/1
                       ).

-spec handle_status_stat(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_status_stat(JObj, Props) ->
    'true' = case (EventName = kz_json:get_value(<<"Event-Name">>, JObj)) of
                 <<"ready">> -> kapi_acdc_stats:status_ready_v(JObj);
                 <<"logged_in">> -> kapi_acdc_stats:status_logged_in_v(JObj);
                 <<"logged_out">> -> kapi_acdc_stats:status_logged_out_v(JObj);
                 <<"pending_logged_out">> -> kapi_acdc_stats:status_pending_logged_out_v(JObj);
                 <<"connecting">> -> kapi_acdc_stats:status_connecting_v(JObj);
                 <<"connected">> -> kapi_acdc_stats:status_connected_v(JObj);
                 <<"wrapup">> -> kapi_acdc_stats:status_wrapup_v(JObj);
                 <<"paused">> -> kapi_acdc_stats:status_paused_v(JObj);
                 <<"outbound">> -> kapi_acdc_stats:status_outbound_v(JObj);
                 _Name ->
                     lager:debug("recv unknown status stat type ~s: ~p", [_Name, JObj]),
                     'false'
             end,

    AgentId = kz_json:get_value(<<"Agent-ID">>, JObj),
    Timestamp = kz_json:get_integer_value(<<"Timestamp">>, JObj),

    gen_listener:cast(props:get_value('server', Props)
                     ,{'create_status'
                      ,#status_stat{id=status_stat_id(AgentId, Timestamp, EventName)
                                   ,agent_id=AgentId
                                   ,account_id=kz_json:get_value(<<"Account-ID">>, JObj)
                                   ,status=EventName
                                   ,timestamp=Timestamp
                                   ,callid=kz_json:get_value(<<"Call-ID">>, JObj)
                                   ,wait_time=acdc_stats_util:wait_time(EventName, JObj)
                                   ,pause_time=acdc_stats_util:pause_time(EventName, JObj)
                                   ,caller_id_name=acdc_stats_util:caller_id_name(EventName, JObj)
                                   ,caller_id_number=acdc_stats_util:caller_id_number(EventName, JObj)
                                   ,queue_id=acdc_stats_util:queue_id(EventName, JObj)
                                   }
                      }
                     ).

-spec status_stat_id(kz_term:ne_binary(), pos_integer(), any()) -> kz_term:ne_binary().
status_stat_id(AgentId, Timestamp, _EventName) ->
    <<AgentId/binary, "::", (kz_term:to_binary(Timestamp))/binary>>.

-spec handle_status_query(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_status_query(JObj, _Prop) ->
    'true' = kapi_acdc_stats:status_req_v(JObj),
    RespQ = kz_json:get_value(<<"Server-ID">>, JObj),
    MsgId = kz_json:get_value(<<"Msg-ID">>, JObj),
    Limit = acdc_stats_util:get_query_limit(JObj),

    case status_build_match_spec(JObj) of
        {'ok', Match} -> query_statuses(RespQ, MsgId, Match, Limit);
        {'error', Errors} -> publish_query_errors(RespQ, MsgId, Errors)
    end.

-spec publish_query_errors(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
publish_query_errors(RespQ, MsgId, Errors) ->
    API = [{<<"Error-Reason">>, Errors}
          ,{<<"Msg-ID">>, MsgId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("responding with errors to req ~s: ~p", [MsgId, Errors]),
    kapi_acdc_stats:publish_status_err(RespQ, API).

status_build_match_spec(JObj) ->
    case kz_json:get_value(<<"Account-ID">>, JObj) of
        'undefined' ->
            {'error', kz_json:from_list([{<<"Account-ID">>, <<"missing but required">>}])};
        AccountId ->
            AcctMatch = {#status_stat{account_id='$1', _='_'}
                        ,[{'=:=', '$1', {'const', AccountId}}]
                        },
            status_build_match_spec(JObj, AcctMatch)
    end.

-spec status_build_match_spec(kz_json:object(), {status_stat(), list()}) ->
                                     {'ok', ets:match_spec()} |
                                     {'error', kz_json:object()}.
status_build_match_spec(JObj, AcctMatch) ->
    case kz_json:foldl(fun status_match_builder_fold/3, AcctMatch, JObj) of
        {'error', _Errs}=Errors -> Errors;
        {StatusStat, Constraints} -> {'ok', [{StatusStat, Constraints, ['$_']}]}
    end.

status_match_builder_fold(_, _, {'error', _Err}=E) -> E;
status_match_builder_fold(<<"Agent-ID">>, AgentId, {StatusStat, Contstraints}) ->
    {StatusStat#status_stat{agent_id='$2'}
    ,[{'=:=', '$2', {'const', AgentId}} | Contstraints]
    };
status_match_builder_fold(<<"Start-Range">>, Start, {StatusStat, Contstraints}) ->
    Now = kz_time:now_s(),
    Past = Now - ?CLEANUP_WINDOW,

    try kz_term:to_integer(Start) of
        N when N < Past ->
            {'error', kz_json:from_list([{<<"Start-Range">>, <<"supplied value is too far in the past">>}
                                        ,{<<"Window-Size">>, ?CLEANUP_WINDOW}
                                        ,{<<"Current-Timestamp">>, Now}
                                        ,{<<"Past-Timestamp">>, Past}
                                        ,{<<"Start-Range">>, N}
                                        ])};
        N when N > Now ->
            {'error', kz_json:from_list([{<<"Start-Range">>, <<"supplied value is in the future">>}
                                        ,{<<"Current-Timestamp">>, Now}
                                        ])};
        N ->
            {StatusStat#status_stat{timestamp='$3'}
            ,[{'>=', '$3', N} | Contstraints]
            }
    catch
        _:_ ->
            {'error', kz_json:from_list([{<<"Start-Range">>, <<"supplied value is not an integer">>}])}
    end;
status_match_builder_fold(<<"End-Range">>, End, {StatusStat, Contstraints}) ->
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
            {StatusStat#status_stat{timestamp='$3'}
            ,[{'=<', '$3', N} | Contstraints]
            }
    catch
        _:_ ->
            {'error', kz_json:from_list([{<<"End-Range">>, <<"supplied value is not an integer">>}])}
    end;
status_match_builder_fold(<<"Status">>, Status, {StatusStat, Contstraints}) ->
    {StatusStat#status_stat{status='$4'}
    ,[{'=:=', '$4', {'const', Status}} | Contstraints]
    };
status_match_builder_fold(_, _, Acc) -> Acc.

-spec query_statuses(kz_term:ne_binary(), kz_term:ne_binary(), ets:match_spec(), pos_integer() | 'no_limit') -> 'ok'.
query_statuses(RespQ, MsgId, Match, Limit) ->
    Stats = ets:select(status_table_id(), Match),

    case Stats of
        [] -> lager:debug("no stats found (requester: ~s)", [RespQ]);
        _ -> 'ok'
    end,

    QueryResults = lists:foldl(fun query_status_fold/2, kz_json:new(), Stats),
    TrimmedResults = kz_json:map(fun(A, B) ->
                                         {A, trim_query_statuses(B, Limit)}
                                 end, QueryResults),

    Resp = [{<<"Agents">>, TrimmedResults}
           ,{<<"Msg-ID">>, MsgId}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_acdc_stats:publish_status_resp(RespQ, Resp).

-spec trim_query_statuses(kz_json:object(), pos_integer() | 'no_limit') -> kz_json:object().
trim_query_statuses(Statuses, Limit) ->
    StatusProps = kz_json:to_proplist(Statuses),
    SortedProps = lists:sort(fun({A, _}, {B, _}) ->
                                     kz_term:to_integer(A) >= kz_term:to_integer(B)
                             end, StatusProps),
    LimitedProps = case Limit of
                       'no_limit' -> SortedProps;
                       _ -> lists:sublist(SortedProps, Limit)
                   end,
    kz_json:from_list(LimitedProps).

-spec query_status_fold(status_stat(), kz_json:object()) -> kz_json:object().
query_status_fold(#status_stat{agent_id=AgentId
                              ,timestamp=T
                              }=Stat, Acc) ->
    Doc = kz_doc:public_fields(status_stat_to_doc(Stat)),
    kz_json:set_value([AgentId, kz_term:to_binary(T)], Doc, Acc).

-spec status_stat_to_doc(status_stat()) -> kz_json:object().
status_stat_to_doc(#status_stat{id=Id
                               ,agent_id=AgentId
                               ,account_id=AccountId
                               ,status=Status
                               ,timestamp=Timestamp
                               ,wait_time=WT
                               ,pause_time=PT
                               ,callid=CallId
                               ,caller_id_name=CIDName
                               ,caller_id_number=CIDNum
                               ,queue_id=QueueId
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
           ,{<<"queue_id">>, QueueId}
           ],
    kz_doc:update_pvt_parameters(kz_json:from_list(Prop)
                                ,acdc_stats_util:db_name(AccountId)
                                ,[{'account_id', AccountId}
                                 ,{'type', <<"status_stat">>}
                                 ]).

-spec archive_status_data(pid(), boolean()) -> 'ok'.
archive_status_data(Srv, 'true') ->
    kz_log:put_callid(<<"acdc_stats.force_status_archiver">>),
    Match = [{#status_stat{is_archived='$1'
                          ,_='_'
                          }
             ,[{'=:=', '$1', 'false'}]
             ,['$_']
             }],
    maybe_archive_status_data(Srv, Match);

archive_status_data(Srv, 'false') ->
    kz_log:put_callid(<<"acdc_stats.status_archiver">>),
    Past = kz_time:now_s() - ?ARCHIVE_WINDOW,
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
            kz_datamgr:suppress_change_notice(),
            ToSave = lists:foldl(fun archive_status_fold/2, dict:new(), Stats),
            _ = [kz_datamgr:save_docs(acdc_stats_util:db_name(Acct), Docs)
                 || {Acct, Docs} <- dict:to_list(ToSave)
                ],
            _ = [gen_listener:cast(Srv, {'update_status', Id, [{#status_stat.is_archived, 'true'}]})
                 || #status_stat{id=Id} <- Stats
                ],
            'ok'
    end.

-spec archive_status_fold(status_stat(), dict:dict()) -> dict:dict().
archive_status_fold(#status_stat{account_id=AccountId}=Stat, Acc) ->
    Doc = status_stat_to_doc(Stat),
    dict:update(AccountId, fun(L) -> [Doc | L] end, [Doc], Acc).

log_status_change(AccountId, Prop) ->
    Body = kz_json:normalize(kz_json:from_list([{<<"Event">>, <<"agent_status_change">>} | Prop])),
    kz_edr:event(?APP_NAME, ?APP_VERSION, 'ok', 'info', Body, AccountId).
