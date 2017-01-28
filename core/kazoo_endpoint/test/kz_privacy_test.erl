%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_privacy_test).

-include_lib("eunit/include/eunit.hrl").

anon_test_() ->
    [?_assertEqual(<<"anonymous">>, kz_privacy:anonymous_caller_id_name())
    ,?_assertEqual(<<"0000000000">>, kz_privacy:anonymous_caller_id_number())
    ].
