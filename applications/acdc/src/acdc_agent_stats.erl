%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Collector of stats for agents
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_agent_stats).

-export([agent_ready/2
         ,agent_logged_in/2
         ,agent_logged_out/2
         ,agent_connecting/3, agent_connecting/5
         ,agent_connected/3, agent_connected/5
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

status_table_id() -> 'acdc_stats_status'.
status_key_pos() -> #status_stat.id.
status_table_opts() ->
    ['protected', 'named_table'
     ,{'keypos', status_key_pos()}
    ].

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
                      ,{'create_status'
                        ,#status_stat{
                            id=status_stat_id(AgentId, Timestamp, EventName)
                            ,agent_id=AgentId
                            ,acct_id=wh_json:get_value(<<"Account-ID">>, JObj)
                            ,status=EventName
                            ,timestamp=Timestamp
                            ,callid=wh_json:get_value(<<"Call-ID">>, JObj)
                            ,wait_time=acdc_stats_util:wait_time(EventName, JObj)
                            ,pause_time=acdc_stats_util:pause_time(EventName, JObj)
                            ,caller_id_name=acdc_stats_util:caller_id_name(EventName, JObj)
                            ,caller_id_number=acdc_stats_util:caller_id_number(EventName, JObj)
                           }}).


-spec status_stat_id(ne_binary(), pos_integer(), _) -> ne_binary().
status_stat_id(AgentId, Timestamp, _EventName) ->
    <<AgentId/binary, "::", (wh_util:to_binary(Timestamp))/binary>>.

-spec handle_status_query(wh_json:object(), wh_proplist()) -> 'ok'.
handle_status_query(JObj, _Prop) ->
    'true' = wapi_acdc_stats:status_req_v(JObj),
    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
    MsgId = wh_json:get_value(<<"Msg-ID">>, JObj),
    Limit = acdc_stats_util:get_query_limit(JObj),

    case status_build_match_spec(JObj) of
        {'ok', Match} -> query_statuses(RespQ, MsgId, Match, Limit);
        {'error', Errors} -> publish_query_errors(RespQ, MsgId, Errors)
    end.

-spec publish_query_errors(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
publish_query_errors(RespQ, MsgId, Errors) ->
    API = [{<<"Error-Reason">>, Errors}
           ,{<<"Msg-ID">>, MsgId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("responding with errors to req ~s: ~p", [MsgId, Errors]),
    wapi_acdc_stats:publish_status_err(RespQ, API).

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

-spec query_status_fold(status_stat(), wh_json:object()) -> wh_json:object().
query_status_fold(#status_stat{agent_id=AgentId
                               ,timestamp=T
                              }=Stat, Acc) ->
    Doc = status_stat_to_doc(Stat),
    wh_json:set_value([AgentId, wh_util:to_binary(T)], Doc, Acc).

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
      ,acdc_stats_util:db_name(AcctId)
      ,[{'account_id', AcctId}
        ,{'type', <<"status_stat">>}
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
    case ets:select(acdc_agent_stats:status_table_id(), Match) of
        [] -> 'ok';
        Stats ->
            couch_mgr:suppress_change_notice(),
            ToSave = lists:foldl(fun archive_status_fold/2, dict:new(), Stats),
            [couch_mgr:save_docs(acdc_stats_util:db_name(Acct), Docs)
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
