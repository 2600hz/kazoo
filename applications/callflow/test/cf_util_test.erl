%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_util_test).

-include_lib("eunit/include/eunit.hrl").

alpha_to_dialpad_test() ->
    ?assertEqual(<<"222">>, cf_util:alpha_to_dialpad(<<"abc">>)),
    ?assertEqual(<<"23456789">>, cf_util:alpha_to_dialpad(<<"behknqux">>)),
    ?assertEqual(<<"23456789">>, cf_util:alpha_to_dialpad(<<"BeHkNqUx">>)),
    ?assertEqual(<<"23456789">>, cf_util:alpha_to_dialpad(<<"1BeH@k(N$q-u+x=">>)).

alpha_to_dialpad_nonascii_test() ->
    ?assertEqual(<<"8378">>, cf_util:alpha_to_dialpad(<<"teÕst">>)),
    ?assertEqual(<<"8378896">>, cf_util:alpha_to_dialpad(<<"testÕÀ«©§two">>)),
    ?assertEqual(<<"">>, cf_util:alpha_to_dialpad(<<"Ì¸ûî">>)).
