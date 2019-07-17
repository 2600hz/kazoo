%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_util_tests).

-include_lib("eunit/include/eunit.hrl").

-export([db_dependant/0]).

knm_number_test_() ->
    knm_test_util:start_db(fun db_dependant/0).

db_dependant() ->
    [pretty_print()
    ].

pretty_print() ->
    [?_assertEqual(Result, knm_util:pretty_print(Format, Number))
     || {Number, Format, Result}
            <- [{<<"+14158867900">>, <<"SS(###) ### - *">>, <<"(415) 886 - 7900">>}
               ,{<<"+14158867900">>, <<"S\\SS(###\\#) ### - *\\*">>, <<"S(415#) 886 - 7900*">>}
               ,{<<"+14158867900">>, <<"*">>, <<"+14158867900">>}
               ]
    ].
