%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(hangups_maintenance).

-export([hangups_summary/0
        ,hangup_summary/1, hangup_summary/2
        ,account_summary/1
        ]).

-export([set_threshold/3, set_threshold/4
        ,set_metric/2, set_metric/3
        ]).

%% Deprecated
-export([activate_monitor/2, activate_monitors/2
        ,set_monitor_threshold/2, set_monitor_threshold/3
        ]).

-include("hangups.hrl").


-spec hangups_summary() -> 'ok'.
hangups_summary() ->
    Hangups = [{Name, hangups_query_listener:meter_resp(Name)}
               || Name <- folsom_metrics:get_metrics(),
                  hangups_util:is_hangup_meter(Name)
              ],
    print_stats(Hangups).

-spec hangup_summary(kz_term:ne_binary()) -> 'ok'.
hangup_summary(HangupCause) ->
    HC = kz_term:to_upper_binary(HangupCause),
    io:format("checking hangup summary for ~s~n", [HC]),
    Hangups = [{Name, hangups_query_listener:meter_resp(Name)}
               || Name <- folsom_metrics:get_metrics(),
                  hangups_util:is_hangup_meter(Name, HC)
              ],
    print_stats(Hangups).

-spec hangup_summary(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
hangup_summary(HangupCause, AccountId) ->
    HC = kz_term:to_upper_binary(HangupCause),
    io:format("checking hangup summary for ~s.~s~n", [HC, AccountId]),
    Hangups = [{Name, hangups_query_listener:meter_resp(Name)}
               || Name <- folsom_metrics:get_metrics(),
                  hangups_util:is_hangup_meter(Name, HC, AccountId)
              ],
    print_stats(Hangups).

-spec account_summary(kz_term:ne_binary()) -> 'ok'.
account_summary(AccountId) ->
    io:format("checking hangups summary for account ~s~n", [AccountId]),
    Hangups = [{Name, hangups_query_listener:meter_resp(Name)}
               || Name <- folsom_metrics:get_metrics(),
                  hangups_util:meter_account_id(Name) =:= AccountId
              ],
    print_stats(Hangups).

-define(STAT_SUMMARY_FORMAT
       ," ~-30s | ~-32s | ~-10s | ~-10s | ~-10s | ~-10s |~n"
       ).

-spec print_stats(kz_term:proplist()) -> 'ok'.
print_stats([]) -> io:format("No data found for request\n");
print_stats(Stats) ->
    io:format(?STAT_SUMMARY_FORMAT
             ,["Hangup Cause", "AccountId", "One", "Five", "Fifteen", "Thr. One"]
             ),
    lists:foreach(fun print_stat/1, lists:keysort(1, Stats)).

-spec print_stat({kz_term:ne_binary(), kz_term:proplist()}) -> 'ok'.
print_stat({Name, Stats}) ->
    AccountId = case hangups_util:meter_account_id(Name) of
                    'undefined' -> <<>>;
                    ID -> ID
                end,
    HangupCause = hangups_util:meter_hangup_cause(Name),
    ConfigName = hangups_util:meter_name(HangupCause),
    Threshold = kapps_config:get_float(ConfigName, <<"one">>),

    io:format(?STAT_SUMMARY_FORMAT
             ,[HangupCause
              ,AccountId
              ,props:get_binary_value(<<"one">>, Stats)
              ,props:get_binary_value(<<"five">>, Stats)
              ,props:get_binary_value(<<"fifteen">>, Stats)
              ,io_lib:format("~e", [Threshold])
              ]).

-spec activate_monitor(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
activate_monitor(AccountId, HangupCause) ->
    hangups_channel_destroy:start_meters(HangupCause),
    hangups_channel_destroy:start_meters(AccountId, HangupCause).

-spec activate_monitors(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
activate_monitors(AccountId, ThresholdOneMinute) ->
    F = fun(HangupCause) ->
                ConfigName = hangups_util:meter_name(HangupCause),
                case kapps_config:get_float(ConfigName, <<"one">>) of
                    'undefined' -> set_monitor_threshold(HangupCause, ThresholdOneMinute);
                    _ThresholdAlreadySet -> 'true'
                end
                    andalso activate_monitor(AccountId, HangupCause)
        end,
    lists:foreach(F, hangups_config:monitored_hangup_causes()).

-spec set_metric(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
set_metric(Metric, LoadAvg) ->
    case is_valid_request(Metric, LoadAvg) of
        'false' -> 'ok';
        'true' ->
            lists:foreach(fun(HC) ->
                                  save_threshold(HC, Metric, LoadAvg, fun kapps_config:set_default/3)
                          end
                         ,hangups_config:monitored_hangup_causes()
                         )
    end.

-spec set_metric(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
set_metric(AccountId, Metric, LoadAvg) ->
    case is_valid_request(Metric, LoadAvg) of
        'false' -> 'ok';
        'true' ->
            SaveFun = account_save_fun(AccountId),
            lists:foreach(fun(HC) ->
                                  save_threshold(HC, Metric, LoadAvg, SaveFun)
                          end
                         ,hangups_config:monitored_hangup_causes()
                         )
    end.

-spec set_threshold(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
set_threshold(HangupCause, Metric, LoadAvg) ->
    case is_valid_request(HangupCause, Metric, LoadAvg) of
        'false' -> 'ok';
        'true' ->
            save_threshold(HangupCause, Metric, LoadAvg, fun kapps_config:set_default/3)
    end.

-spec set_threshold(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
set_threshold(AccountId, HangupCause, Metric, LoadAvg) ->
    case is_valid_request(HangupCause, Metric, LoadAvg) of
        'false' -> 'ok';
        'true' ->
            SaveFun = account_save_fun(AccountId),
            save_threshold(HangupCause, Metric, LoadAvg, SaveFun)
    end.

-type save_fun() :: fun((kz_term:ne_binary(), kz_json:path(), kz_json:json_term()) -> any()).
-spec account_save_fun(kz_term:ne_binary()) -> save_fun().
account_save_fun(AccountId) ->
    fun(Cfg, K, V) ->
            kapps_account_config:set(AccountId, Cfg, K, V)
    end.

-spec save_threshold(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), save_fun()) -> 'ok'.
save_threshold(HangupCause, Metric, LoadAvg, SaveFun) ->
    ConfigName = hangups_util:meter_name(HangupCause),
    SaveFun(ConfigName, Metric, LoadAvg),
    io:format("set ~s for ~s to ~p~n", [Metric, ConfigName, LoadAvg]).

-spec is_valid_request(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_valid_request(Metric, LoadAvg) ->
    lists:all(fun is_valid_request_param/1
             ,[{fun is_valid_threshold_name/1, Metric}
              ,{fun is_valid_load_avg/1, LoadAvg}
              ]
             ).

-spec is_valid_request(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_valid_request(HangupCause, Metric, LoadAvg) ->
    lists:all(fun is_valid_request_param/1
             ,[{fun is_valid_threshold_name/1, Metric}
              ,{fun is_valid_load_avg/1, LoadAvg}
              ,{fun is_valid_hangup_cause/1, HangupCause}
              ]
             ).

-spec is_valid_request_param({fun((V) -> boolean()), V}) -> boolean().
is_valid_request_param({F, V}) ->
    F(V).

-spec is_valid_hangup_cause(kz_term:ne_binary()) -> 'true'.
is_valid_hangup_cause(HangupCause) ->
    HangupCauses = hangups_config:monitored_hangup_causes(),
    case lists:member(HangupCause, HangupCauses) of
        'true' -> 'true';
        'false' ->
            io:format("  '~s' not currently monitored, adding to ~s"
                     ,[HangupCause
                      ,kz_binary:join(HangupCauses, <<", ">>)
                      ]
                     ),
            'true'
    end.

-spec set_monitor_threshold(kz_term:text(), kz_term:text()) -> boolean().
set_monitor_threshold(HangupCause, TOM) ->
    ThresholdOnMinute = kz_term:to_float(TOM),
    update_monitor_thresholds(HangupCause, ThresholdOnMinute)
        andalso set_monitor_threshold(HangupCause, <<"one">>, ThresholdOnMinute).

-spec update_monitor_thresholds(kz_term:ne_binary(), float()) -> boolean().
update_monitor_thresholds(HangupCause, ThresholdOnMinute) ->
    Scales = [{<<"five">>, 5}
             ,{<<"fifteen">>, 15}
             ,{<<"day">>, 1440}
             ],

    lists:foldl(fun({ThresholdName, MinutesPer}, Acc) ->
                        Threshold = MinutesPer * ThresholdOnMinute,
                        Succeeded = set_monitor_threshold(HangupCause, ThresholdName, Threshold),
                        Acc and Succeeded
                end
               ,'true'
               ,Scales
               ).


-spec set_monitor_threshold(kz_term:ne_binary(), kz_term:ne_binary(), float()) -> boolean().
set_monitor_threshold(HangupCause, ThresholdName, T) ->
    Threshold = kz_term:to_float(T),
    set_monitor_threshold(kz_term:to_upper_binary(HangupCause)
                         ,ThresholdName
                         ,Threshold
                         ,is_valid_threshold_name(ThresholdName)
                         ).

-spec set_monitor_threshold(kz_term:ne_binary(), kz_term:ne_binary(), float(), boolean()) -> boolean().
set_monitor_threshold(_HangupCause, ThresholdName, _Threshold, 'false') ->
    io:format("Invalid threshold name (~s), not setting~n", [ThresholdName]),
    'false';
set_monitor_threshold(HangupCause, ThresholdName, Threshold, 'true') ->
    ConfigName = hangups_util:meter_name(HangupCause),
    case kapps_config:get_float(ConfigName, ThresholdName) of
        'undefined' ->
            kapps_config:set_default(ConfigName, ThresholdName, Threshold),
            io:format("setting ~s for ~s to ~p~n", [ThresholdName, ConfigName, Threshold]);
        _OldValue ->
            kapps_config:set_default(ConfigName, ThresholdName, Threshold),
            io:format("updating ~s for ~s to ~p from ~p~n", [ThresholdName, ConfigName, Threshold, _OldValue])
    end,
    'true'.

-spec is_valid_threshold_name(kz_term:ne_binary()) -> boolean().
is_valid_threshold_name(<<"one">>) -> 'true';
is_valid_threshold_name(<<"five">>) -> 'true';
is_valid_threshold_name(<<"fifteen">>) -> 'true';
is_valid_threshold_name(<<"day">>) -> 'true';
is_valid_threshold_name(<<"mean">>) -> 'true';
is_valid_threshold_name(_Metric) ->
    io:format("metric '~s' is invalid~n", [_Metric]),
    'false'.

-spec is_valid_load_avg(kz_term:ne_binary() | number()) -> boolean().
is_valid_load_avg(V) ->
    try kz_term:to_float(V) of
        F when F >= 0.0 -> 'true';
        _F ->
            io:format("load_avg of ~p is too low~n", [_F]),
            'false'
    catch
        'error':'badarg' ->
            io:format(" load_avg '~p' is invalid~n", [V]),
            'false'
    end.
