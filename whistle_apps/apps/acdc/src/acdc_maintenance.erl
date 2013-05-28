%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Helpers for cli commands
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_maintenance).

-export([current_calls/1, current_calls/2]).

-include("acdc.hrl").

-define(KEYS, [<<"Waiting">>, <<"Handled">>, <<"Processed">>, <<"Abandoned">>]).

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
