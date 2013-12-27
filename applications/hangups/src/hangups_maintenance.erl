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

-export([hangup_summary/0]).

hangup_summary() ->
    HangupNames = [Name || Name <- folsom_metrics:get_metrics(),
                           hangups_listener:is_hangup_meter(Name)
                  ],
    Stats = [{Name, hangups_query_listener:meter_resp(Name)}
             || Name <- HangupNames
            ],
    print_stats(Stats).

-define(STAT_SUMMARY_FORMAT
        ," ~-60s | ~-10s | ~-10s | ~-10s |~n"
       ).

print_stats([]) ->
    'ok';
print_stats(Stats) ->
    io:format(?STAT_SUMMARY_FORMAT
              ,["Stat", "One", "Five", "Fifteen"]
             ),
    lists:foreach(fun print_stat/1, Stats).

print_stat({Name, Stats}) ->
    io:format(?STAT_SUMMARY_FORMAT
              ,[Name
                ,props:get_binary_value(<<"one">>, Stats)
                ,props:get_binary_value(<<"five">>, Stats)
                ,props:get_binary_value(<<"fifteen">>, Stats)
               ]).
