%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_iso3166a2_itu_tests).

-include_lib("eunit/include/eunit.hrl").

to_itu_test_() ->
    [?_assertEqual(<<"+1">>, knm_iso3166a2_itu:to_itu(<<"US">>))
    ,?_assertEqual(<<"+33">>, knm_iso3166a2_itu:to_itu(<<"FR">>))
    ].
