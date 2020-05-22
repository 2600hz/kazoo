%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Stat util functions
%%% @author James Aimonetti
%%%
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_stats_util).

-export([wait_time/2
        ,pause_time/2
        ,caller_id_name/2
        ,caller_id_number/2

        ,get_query_limit/1
        ,db_name/1
        ,prev_modb/1

        ,cleanup_old_stats/0
        ,cleanup_old_calls/1, cleanup_old_statuses/1

        ,call_summary_req/1
        ,publish_summary_data/4
        ,publish_call_query_errors/3
        ,publish_query_errors/4
        ]).

-include("acdc.hrl").
-include("acdc_stats.hrl").

-spec wait_time(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_integer().
wait_time(<<"paused">>, _) -> 'undefined';
wait_time(_, JObj) -> kz_json:get_integer_value(<<"Wait-Time">>, JObj).

-spec pause_time(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_integer().
pause_time(<<"paused">>, JObj) ->
    case kz_json:get_integer_value(<<"Pause-Time">>, JObj) of
        'undefined' -> kz_json:get_integer_value(<<"Wait-Time">>, JObj);
        PT -> PT
    end;
pause_time(_, _JObj) -> 'undefined'.

-spec caller_id_name(any(), kz_json:object()) -> api_kz_term:ne_binary().
caller_id_name(_, JObj) ->
    kz_json:get_value(<<"Caller-ID-Name">>, JObj).

-spec caller_id_number(any(), kz_json:object()) -> kz_term:api_integer().
caller_id_number(_, JObj) ->
    kz_json:get_value(<<"Caller-ID-Number">>, JObj).

-spec get_query_limit(kz_json:object()) -> pos_integer() | 'no_limit'.
get_query_limit(JObj) ->
    get_query_limit(JObj, ?STATS_QUERY_LIMITS_ENABLED).

-spec get_query_limit(kz_json:object(), boolean()) -> pos_integer() | 'no_limit'.
get_query_limit(JObj, 'true') ->
    Max = ?MAX_RESULT_SET,
    case kz_json:get_integer_value(<<"Limit">>, JObj) of
        'undefined' -> Max;
        N when N > Max -> Max;
        N when N < 1 -> 1;
        N -> N
    end;
get_query_limit(JObj, 'false') ->
    case kz_json:get_integer_value(<<"Limit">>, JObj) of
        'undefined' -> 'no_limit';
        N when N < 1 -> 1;
        N -> N
    end.

-spec db_name(kz_term:ne_binary()) -> kz_term:ne_binary().
db_name(Account) ->
    kzs_util:format_account_mod_id(Account).
db_name(Account, {Yr, Mn}) ->
    kzs_util:format_account_mod_id(Account, Yr, Mn);
db_name(Account, Timestamp) ->
    kzs_util:format_account_mod_id(Account, Timestamp).


-spec prev_modb(kz_term:ne_binary()) -> kz_term:ne_binary().
prev_modb(Account) ->
    {{Year, Month, _}, _} = calendar:now_to_universal_time(os:timestamp()),
    prev_modb(Account, Year, Month-1).

-spec prev_modb(kz_term:ne_binary(), calendar:year(), integer()) -> kz_term:ne_binary().
prev_modb(Account, Year, 0) ->
    prev_modb(Account, Year-1, 12);
prev_modb(Account, Year, Month) ->
    kzs_util:format_account_id(Account, Year, Month).

-spec cleanup_old_stats() -> 'ok'.
cleanup_old_stats() ->
    cleanup_old_calls(1200),
    cleanup_old_statuses(14400).

-spec cleanup_old_calls(pos_integer()) -> 'ok'.
cleanup_old_calls(Window) ->
    acdc_stats:manual_cleanup_calls(Window).

-spec cleanup_old_statuses(pos_integer()) -> 'ok'.
cleanup_old_statuses(Window) ->
    acdc_stats:manual_cleanup_statuses(Window).


-spec call_summary_req(kz_json:object()) -> 'ok'.
call_summary_req(JObj) ->
    RespQ = kz_json:get_value(<<"Server-ID">>, JObj),
    MsgId = kz_json:get_value(<<"Msg-ID">>, JObj),
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    StartRange = kz_json:get_value(<<"Start-Range">>, JObj),
    EndRange = kz_json:get_value(<<"End-Range">>, JObj),
    Queues =
        case kz_json:get_value(<<"Queue-ID">>, JObj) of
            undefined -> [ {A,Q,StartRange,EndRange} || {_, {A, Q}} <- acdc_queues_sup:queues_running(), A == AccountId];
            Else -> [ {AccountId,Else,StartRange,EndRange}]
        end,
    Summary = query_call_summary(Queues),
    publish_summary_data(RespQ, MsgId, Summary, []).

-spec query_call_summary([kz_term:ne_binary()]) -> kz_term:proplist().
query_call_summary(Queues) ->
    QueryResults =
        lists:filter(fun(X) -> not kz_json:is_empty(X) end,
                     lists:foldl(fun query_call_summary_fold/2, [], Queues)),

    JsonResult = lists:foldl(fun(QR, JObj) ->
                                     TotalCalls = kz_json:get_value(<<"calls">>, QR),
                                     AbandonedCalls = kz_json:get_value(<<"abandoned">>, QR),
                                     QueueId = kz_json:get_value(<<"Queue-ID">>, QR),
                                     QueueJObj = kz_json:set_values([{<<"total_calls">>, TotalCalls }
                                                                    ,{<<"abandoned_calls">>, AbandonedCalls}
                                                                    ,{<<"average_wait_time">>, kz_json:get_value(<<"wait_time">>, QR) div TotalCalls}
                                                                    ,{<<"average_talk_time">>, kz_json:get_value(<<"talk_time">>, QR) div (TotalCalls - AbandonedCalls)}
                                                                    ,{<<"max_entered_position">>, kz_json:get_value(<<"entered_position">>, QR)}
                                                                    ]
                                                                   ,kz_json:new()),
                                     kz_json:set_value(QueueId, QueueJObj, JObj)
                             end
                            ,kz_json:new()
                            ,QueryResults),

    [{<<"Data">>, JsonResult}].

-spec query_call_summary_fold(kz_term:ne_binary(), kz_term:proplist()) -> [kz_json:object()].
query_call_summary_fold({AccountId, _QueueId, StartRange, EndRange} = Data, Acc) ->
    StartMODB = db_name(AccountId, StartRange),
    EndMODB = db_name(AccountId, EndRange),
    case StartMODB =:= EndMODB of
        true -> [get_results_from_db(StartMODB, Data)|Acc];
        false -> [get_results_from_dbs(modb_range(AccountId, StartRange, EndRange), Data)|Acc]
    end.

get_results_from_db(DB, {AccountId, QueueId, StartRange, EndRange}) ->
    Opts = [{'startkey', [QueueId, StartRange]}
           ,{'endkey', [QueueId, EndRange]}
           ,{'limit', 1}
           ,{'reduce', true}
           ],
    case kz_datamgr:get_results(DB, <<"call_stats/call_summary">>, Opts) of
        {'ok', []} -> kz_json:new();
        {'ok', [JObj]} ->
            V1 = kz_json:get_value(<<"value">>, JObj),
            V2 = kz_json:set_values([{<<"Account-ID">>, AccountId},
                                     {<<"Queue-ID">>,QueueId}],
                                    V1),
            V2;
        {'error', _E} ->
            lager:debug("error querying view: ~p", [_E]),
            kz_json:new()
    end.

get_results_from_dbs(DBs, Data) ->
    lists:foldl(fun(DB, Acc) ->
                        H = get_results_from_db(DB, Data),
                        merge_results(H, Acc)
                end, kz_json:new(), DBs).

merge_results(JObj1, JObj2) ->
    Fun = fun(_,{both, V1, V2}) when is_integer(V1), is_integer(V2) -> {ok, V1 + V2};
             (_,{both, V, V}) -> {ok, V} end,
    kz_json:merge(Fun, [JObj1, JObj2]).

modb_range(AccountId, StartRange, EndRange) ->
    {{SY,SM,_}, _} = calendar:gregorian_seconds_to_datetime(StartRange),
    {{EY,EM,_}, _} = calendar:gregorian_seconds_to_datetime(EndRange),
    modb_db_list(AccountId,  {SY,SM}, {EY,EM}, []).

modb_db_list(AccountId, Next, End, Acc)
  when Next =:= End ->
    [db_name(AccountId, Next)|Acc];
modb_db_list(AccountId, Next, End, Acc) ->
    modb_db_list(AccountId, next_modb(Next), End, [db_name(AccountId, Next)|Acc]).

next_modb({Yr,Mn}) ->
    case Mn of
        12 -> {Yr+1, 1};
        _ -> {Yr, Mn + 1}
    end.

-spec publish_summary_data(kz_term:ne_binary()
                          ,kz_term:ne_binary()
                          ,kz_term:proplist() | {'error', _}
                          ,kz_term:proplist() | {'error', _}) -> 'ok'.
publish_summary_data(RespQ, MsgId, {'error', Errors}, _) ->
    publish_call_summary_query_errors(RespQ, MsgId, Errors);
publish_summary_data(RespQ, MsgId, _, {'error', Errors}) ->
    publish_call_query_errors(RespQ, MsgId, Errors);
publish_summary_data(RespQ, MsgId, Summary, []) ->
    Resp = Summary ++
        kz_api:default_headers(?APP_NAME, ?APP_VERSION) ++
        [{<<"Query-Time">>, kz_time:current_tstamp()}
        ,{<<"Msg-ID">>, MsgId}
        ],
    kapi_acdc_stats:publish_call_summary_resp(RespQ, Resp);
publish_summary_data(RespQ, MsgId, Summary, Active) ->
    Resp = Summary ++
        remove_missed(Active) ++
        kz_api:default_headers(?APP_NAME, ?APP_VERSION) ++
        [{<<"Query-Time">>, kz_time:current_tstamp()}
        ,{<<"Msg-ID">>, MsgId}
        ],
    kapi_acdc_stats:publish_call_summary_resp(RespQ, Resp).


-spec publish_call_query_errors(kz_term:ne_binary()
                               ,kz_term:ne_binary()
                               ,kz_term:proplist() | {'error', _}) -> 'ok'.
publish_call_query_errors(RespQ, MsgId, Errors) ->
    publish_query_errors(RespQ, MsgId, Errors, fun kapi_acdc_stats:publish_current_calls_err/2).

-spec publish_call_summary_query_errors(kz_term:ne_binary()
                                       ,kz_term:ne_binary()
                                       ,kz_term:proplist() | {'error', _}) -> 'ok'.
publish_call_summary_query_errors(RespQ, MsgId, Errors) ->
    publish_query_errors(RespQ, MsgId, Errors, fun kapi_acdc_stats:publish_call_summary_err/2).

-spec publish_query_errors(kz_term:ne_binary()
                          ,kz_term:ne_binary()
                          ,kz_term:proplist() | {'error', _}
                          ,fun())  -> 'ok'.
publish_query_errors(RespQ, MsgId, Errors, PubFun) ->
    API = [{<<"Error-Reason">>, Errors}
          ,{<<"Msg-ID">>, MsgId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("responding with errors to req ~s: ~p", [MsgId, Errors]),
    PubFun(RespQ, API).

-spec remove_missed(kz_term:proplist()) -> kz_term:proplist().
remove_missed(Active) ->
    [{<<"Waiting">>, remove_misses_fold(props:get_value(<<"Waiting">>, Active, []))}
    ,{<<"Handled">>, remove_misses_fold(props:get_value(<<"Handled">>, Active, []))}
    ].

-spec remove_misses_fold(kz_json:objects()) -> kz_json:objects().
remove_misses_fold(JObjs) ->
    remove_misses_fold(JObjs, []).

-spec remove_misses_fold(kz_json:objects(), kz_json:objects()) -> kz_json:objects().
remove_misses_fold([], Acc) ->
    Acc;
remove_misses_fold([JObj|JObjs], Acc) ->
    remove_misses_fold(JObjs, [kz_json:delete_key(<<"misses">>, JObj) | Acc]).
