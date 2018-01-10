%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_iso3166a2_itu_test).

-include_lib("eunit/include/eunit.hrl").

to_itu_test_() ->
    [?_assertEqual(<<"+1">>, knm_iso3166a2_itu:to_itu(<<"US">>))
    ,?_assertEqual(<<"+33">>, knm_iso3166a2_itu:to_itu(<<"FR">>))
    ].
