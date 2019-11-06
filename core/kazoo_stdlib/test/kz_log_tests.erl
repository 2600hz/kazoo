%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_log_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

%% Just to please coverage :)
log_test_() ->
    ST = try throw('just_for_fun')
         catch
             ?STACKTRACE(_E, _R, Stack)
             Stack
             end,
    [?_assertEqual('ok', kz_log:log_stacktrace(ST))].
