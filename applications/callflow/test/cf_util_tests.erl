%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_util_tests).

-include_lib("eunit/include/eunit.hrl").

alpha_to_dialpad_test_() ->
    [?_assertEqual(<<"222">>, cf_util:alpha_to_dialpad(<<"abc">>))
    ,?_assertEqual(<<"23456789">>, cf_util:alpha_to_dialpad(<<"behknqux">>))
    ,?_assertEqual(<<"23456789">>, cf_util:alpha_to_dialpad(<<"BeHkNqUx">>))
    ,?_assertEqual(<<"23456789">>, cf_util:alpha_to_dialpad(<<"1BeH@k(N$q-u+x=">>))
    ].

alpha_to_dialpad_nonascii_test_() ->
    [?_assertEqual(<<"8378">>, cf_util:alpha_to_dialpad(<<"teÕst">>))
    ,?_assertEqual(<<"8378896">>, cf_util:alpha_to_dialpad(<<"testÕÀ«©§two">>))
    ,?_assertEqual(<<"">>, cf_util:alpha_to_dialpad(<<"Ì¸ûî">>))
    ].
