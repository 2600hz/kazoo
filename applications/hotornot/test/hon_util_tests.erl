%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(hon_util_tests).

-include_lib("eunit/include/eunit.hrl").

%% KAZOO-5860
dollars_and_units_test_() ->
    BaseCost = 5100,
    Charges = [{0.01, 60, 0.5}
              ,{100, 60, 5000}
              ],
    [?_assertEqual(BaseCost, kapps_call_util:base_call_cost(RateCost, RateMin, RateSurcharge))
     || {RateCost, RateMin, RateSurcharge} <- Charges
    ].


-define(RATE_1
       ,kz_json:from_list([{<<"prefix">>, 432}
                          ,{<<"weight">>, 50}
                          ,{<<"rate_cost">>, 0.5}
                          ,{<<"rate_name">>, <<"1">>}
                          ])
       ).
-define(RATE_2
       ,kz_json:from_list([{<<"prefix">>, 432}
                          ,{<<"weight">>, 51}
                          ,{<<"rate_cost">>, 0.6}
                          ,{<<"rate_name">>, <<"2">>}
                          ])
       ).
-define(RATES, [?RATE_1, ?RATE_2]).

sort_rates_by_weight_test_() ->
    Sorted = hon_util:sort_rates_by_weight(?RATES),
    ?_assertEqual([<<"1">>, <<"2">>]
                 ,[kzd_rates:rate_name(Rate) || Rate <- Sorted]
                 ).

sort_rates_by_cost_test_() ->
    Sorted = hon_util:sort_rates_by_cost(?RATES),
    ?_assertEqual([<<"2">>, <<"1">>]
                 ,[kzd_rates:rate_name(Rate) || Rate <- Sorted]
                 ).
