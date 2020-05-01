%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzt_twiml_dial_tests).

-include_lib("eunit/include/eunit.hrl").

cleanup_dial_me_test() ->
    ?assertEqual(<<"+14158867900">>
                ,kzt_twiml_dial:cleanup_dial_me(<<"+1 (415) 886-7900">>)
                ).
