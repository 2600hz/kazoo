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

-export([hangup_summary/0, hangup_summary/1, hangup_summary/2
        ]).

-include("hangups.hrl").

-spec hangup_summary() -> 'ok'.
-spec hangup_summary(ne_binary()) -> 'ok'.
-spec hangup_summary(ne_binary(), ne_binary()) -> 'ok'.

hangup_summary() ->
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
