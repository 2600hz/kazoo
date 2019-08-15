%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_endpoint_tests).

-include_lib("eunit/include/eunit.hrl").

attributes_keys_unique_test_() ->
    Keys = kz_endpoint:attributes_keys(),
    [?_assertEqual(length(Keys), length(lists:usort(Keys)))].
