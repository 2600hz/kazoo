%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Helpers for cli commands
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_maintenance).

-export([current_calls/1, current_calls/2
         ,current_statuses/1
        ]).

-include("acdc.hrl").

-define(KEYS, [<<"Waiting">>, <<"Handled">>, <<"Processed">>, <<"Abandoned">>]).

current_statuses(AcctId) ->
    Agents = acdc_util:agent_statuses(AcctId),
    case wh_json:get_values(Agents) of
        {[], []} ->
            lager:debug("No agent statuses found for ~s", [AcctId]);
        {As, _} ->
            lager:debug("Agent Statuses for ~s", [AcctId]),
            lager:debug("~4s | ~35s | ~12s | ~20s |", [<<>>, <<"Agent-ID">>, <<"Status">>, <<"Timestamp">>]),
            log_current_statuses(As, 1),
            'ok'
    end.

log_current_statuses([], _) -> 'ok';
log_current_statuses([A|As], N) ->
    log_current_status(A, N),
    log_current_statuses(As, N+1).

log_current_status(A, N) ->
    lager:debug("~4b | ~35s | ~12s | ~20s |", [N, wh_json:get_value(<<"agent_id">>, A)
                                               ,wh_json:get_value(<<"status">>, A)
                                               ,wh_util:pretty_print_datetime(wh_json:get_integer_value(<<"timestamp">>, A))
                                              ]).

current_calls(AcctId) ->
    Req = [{<<"Account-ID">>, AcctId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    get_and_show(AcctId, <<"all">>, Req).

current_calls(AcctId, QueueId) when is_binary(QueueId) ->
    Req = [{<<"Account-ID">>, AcctId}
           ,{<<"Queue-ID">>, QueueId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    get_and_show(AcctId, QueueId, Req);
current_calls(AcctId, Props) ->
    Req = [{<<"Account-ID">>, AcctId}
           | Props ++ wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    get_and_show(AcctId, <<"custom">>, Req).

get_and_show(AcctId, QueueId, Req) ->
    put('callid', <<"acdc_maint.", AcctId/binary, ".", QueueId/binary>>),
    case whapps_util:amqp_pool_collect(Req, fun wapi_acdc_stats:publish_current_calls_req/1) of
        {_, []} ->
            lager:info("no call stats returned for account ~s (queue ~s)", [AcctId, QueueId]);
        {'ok', JObjs} ->
            lager:info("call stats for account ~s (queue ~s)", [AcctId, QueueId]),
            show_call_stats(JObjs, ?KEYS);
        {'timeout', JObjs} ->
            lager:info("call stats for account ~s (queue ~s)", [AcctId, QueueId]),
            show_call_stats(JObjs, ?KEYS);
        {'error', _E} ->
            lager:info("failed to lookup call stats for account ~s (queue ~s): ~p", [AcctId, QueueId, _E])
    end.

show_call_stats([], _) -> 'ok';
show_call_stats([Resp|Resps], Ks) ->
    show_call_stat_cat(Ks, Resp),
    show_call_stats(Resps, Ks).

show_call_stat_cat([], _) -> 'ok';
show_call_stat_cat([K|Ks], Resp) ->
    case wh_json:get_value(K, Resp) of
        'undefined' -> show_call_stat_cat(Ks, Resp);
        V ->
            lager:debug("call stats in ~s", [K]),
            show_stats(V),
            show_call_stat_cat(Ks, Resp)
    end.

show_stats([]) -> 'ok';
show_stats([S|Ss]) ->
    _ = [lager:info("~s: ~p", [K, V])
         || {K, V} <- wh_json:to_proplist(wh_doc:public_fields(S))
        ],
    show_stats(Ss).
