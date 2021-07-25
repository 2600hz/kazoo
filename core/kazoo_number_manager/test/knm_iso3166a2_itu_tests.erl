%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2021, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_iso3166a2_itu_tests).

-include_lib("eunit/include/eunit.hrl").

to_itu_test_() ->
    [?_assertEqual(<<"+1">>, knm_iso3166a2_itu:to_itu(<<"US">>))
    ,?_assertEqual(<<"+33">>, knm_iso3166a2_itu:to_itu(<<"FR">>))
    ].
