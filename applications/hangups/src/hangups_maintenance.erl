%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%%
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(hangups_maintenance).

-export([hangups_summary/0
         ,hangup_summary/1, hangup_summary/2
         ,account_summary/1
         ,set_monitor_threshold/3
        ]).

-include("hangups.hrl").

-spec hangups_summary() -> 'ok'.
-spec hangup_summary(ne_binary()) -> 'ok'.
-spec hangup_summary(ne_binary(), ne_binary()) -> 'ok'.

hangups_summary() ->
    Hangups = [{Name, hangups_query_listener:meter_resp(Name)}
                   || Name <- folsom_metrics:get_metrics(),
                      hangups_util:is_hangup_meter(Name)
                  ],
    print_stats(Hangups).

hangup_summary(HangupCause) ->
    HC = wh_util:to_upper_binary(HangupCause),
    io:format("checking hangup summary for ~s~n", [HC]),
    Hangups = [{Name, hangups_query_listener:meter_resp(Name)}
                   || Name <- folsom_metrics:get_metrics(),
                      hangups_util:is_hangup_meter(Name, HC)
                  ],
    print_stats(Hangups).

hangup_summary(HangupCause, AccountId) ->
    HC = wh_util:to_upper_binary(HangupCause),
    io:format("checking hangup summary for ~s.~s~n", [HC, AccountId]),
    Hangups = [{Name, hangups_query_listener:meter_resp(Name)}
                   || Name <- folsom_metrics:get_metrics(),
                      hangups_util:is_hangup_meter(Name, HC, AccountId)
                  ],
    print_stats(Hangups).

-spec account_summary(ne_binary()) -> 'ok'.
account_summary(AccountId) ->
    io:format("checking hangups summary for account ~s~n", [AccountId]),
    Hangups = [{Name, hangups_query_listener:meter_resp(Name)}
               || Name <- folsom_metrics:get_metrics(),
                  hangups_util:meter_account_id(Name) =:= AccountId
              ],
    print_stats(Hangups).

-define(STAT_SUMMARY_FORMAT
        ," ~-30s | ~-32s | ~-10s | ~-10s | ~-10s |~n"
       ).

-spec print_stats(wh_proplist()) -> 'ok'.
print_stats([]) -> io:format("No data found for request~n", []);
print_stats(Stats) ->
    io:format(?STAT_SUMMARY_FORMAT
              ,["Hangup Cause", "AccountId", "One", "Five", "Fifteen"]
             ),
    lists:foreach(fun print_stat/1, lists:keysort(1, Stats)).

-spec print_stat({ne_binary(), wh_proplist()}) -> 'ok'.
print_stat({Name, Stats}) ->
    AccountId = case hangups_util:meter_account_id(Name) of
                    'undefined' -> <<>>;
                    ID -> ID
                end,
    HangupCause = hangups_util:meter_hangup_cause(Name),

    io:format(?STAT_SUMMARY_FORMAT
              ,[HangupCause
                ,AccountId
                ,props:get_binary_value(<<"one">>, Stats)
                ,props:get_binary_value(<<"five">>, Stats)
                ,props:get_binary_value(<<"fifteen">>, Stats)
               ]).

-spec set_monitor_threshold(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
set_monitor_threshold(HangupCause, ThresholdName, Threshold) ->
    set_monitor_threshold(wh_util:to_lower_binary(HangupCause)
                          ,ThresholdName
                          ,Threshold
                          ,is_valid_threshold_name(ThresholdName)
                          ,is_valid_threshold(Threshold)
                         ).
set_monitor_threshold(HangupCause, ThresholdName, Threshold, _, 'false') ->
    io:format("Failed to set ~s for ~s: threshold value (~s) could not be converted to a number~n"
              ,[ThresholdName, HangupCause, Threshold]
             );
set_monitor_threshold(_HangupCause, ThresholdName, _Threshold, 'false', _) ->
    io:format("Invalid threshold name (~s), not setting~n", [ThresholdName]);
set_monitor_threshold(HangupCause, ThresholdName, _Threshold, 'true', ThresholdValue) ->
    ConfigName = <<?APP_NAME/binary, ".", HangupCause/binary>>,
    case whapps_config:get(ConfigName, ThresholdName) of
        'undefined' ->
            whapps_config:set_default(ConfigName, ThresholdName, ThresholdValue),
            io:format("Set ~s for ~s to ~p~n", [ThresholdName, ConfigName, ThresholdValue]);
        _OldValue ->
            whapps_config:set_default(ConfigName, ThresholdName, ThresholdValue),
            io:format("updating ~s for ~s to ~p from ~p~n", [ThresholdName, ConfigName, ThresholdValue, _OldValue])
    end.

-spec is_valid_threshold_name(ne_binary()) -> boolean().
is_valid_threshold_name(<<"one">>) -> 'true';
is_valid_threshold_name(<<"five">>) -> 'true';
is_valid_threshold_name(<<"fifteen">>) -> 'true';
is_valid_threshold_name(<<"day">>) -> 'true';
is_valid_threshold_name(<<"mean">>) -> 'true';
is_valid_threshold_name(_) -> 'false'.

-spec is_valid_threshold(ne_binary()) -> float() | 'false'.
is_valid_threshold(N) ->
    try wh_util:to_float(N) of
        Float -> Float
    catch
        'error':'badarg' -> 'false'
    end.
