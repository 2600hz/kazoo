%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module (knm_iso3166_util_tests).

-include_lib("eunit/include/eunit.hrl").

to_iso3166a2_test_() ->
    [?_assertEqual(<<"US">>, knm_iso3166_util_tests:to_iso3166a2(<<"USA">>))
    ,?_assertEqual(<<"FO">>, knm_iso3166_util_tests:to_iso3166a2(<<"FRO">>))
    ,?_assertEqual(<<"DE">>, knm_iso3166_util_tests:to_iso3166a2(<<"DEU">>))
    ,?_assertEqual(<<"FR">>, knm_iso3166_util_tests:to_iso3166a2(<<"FRA">>))].

to_iso3166a3_test_() ->
    [?_assertEqual(knm_iso3166_util_tests:to_iso3166a3(<<"US">>), <<"USA">>)
    ,?_assertEqual(knm_iso3166_util_tests:to_iso3166a3(<<"FO">>), <<"FRO">>)
    ,?_assertEqual(knm_iso3166_util_tests:to_iso3166a3(<<"DE">>), <<"DEU">>)
    ,?_assertEqual(knm_iso3166_util_tests:to_iso3166a3(<<"FR">>), <<"FRA">>)].
