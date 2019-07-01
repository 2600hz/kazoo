%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module (knm_iso3166_util_tests).

-include_lib("eunit/include/eunit.hrl").

iso3166a2_test_() ->
    [?_assertEqual(<<"US">>, knm_iso3166_util:iso3166a2(<<"USA">>))
    ,?_assertEqual(<<"FO">>, knm_iso3166_util:iso3166a2(<<"FRO">>))
    ,?_assertEqual(<<"DE">>, knm_iso3166_util:iso3166a2(<<"DEU">>))
    ,?_assertEqual(<<"FR">>, knm_iso3166_util:iso3166a2(<<"FRA">>))].

iso3166a3_test_() ->
    [?_assertEqual(knm_iso3166_util:iso3166a3(<<"US">>), <<"USA">>)
    ,?_assertEqual(knm_iso3166_util:iso3166a3(<<"FO">>), <<"FRO">>)
    ,?_assertEqual(knm_iso3166_util:iso3166a3(<<"DE">>), <<"DEU">>)
    ,?_assertEqual(knm_iso3166_util:iso3166a3(<<"FR">>), <<"FRA">>)].
