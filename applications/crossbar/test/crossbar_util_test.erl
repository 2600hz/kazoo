%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(crossbar_util_test).

-include_lib("eunit/include/eunit.hrl").

year_month_sequence_test() ->
    ?assertEqual([{2013, 11}, {2013, 12}, {2014, 1}]
                 ,crossbar_util:generate_year_month_sequence({2013, 11}
                                                             ,{2014, 1}
                                                            )).
