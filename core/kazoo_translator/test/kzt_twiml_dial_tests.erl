%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzt_twiml_dial_tests).

-include_lib("eunit/include/eunit.hrl").

cleanup_dial_me_test() ->
    ?assertEqual(<<"+14158867900">>
                ,kzt_twiml_dial:cleanup_dial_me(<<"+1 (415) 886-7900">>)
                ).
