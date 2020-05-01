%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_test_util).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

-export([start_db/1, start_db/2, start_db/3]).

start_db(TestdListFun) ->
    start_db(TestdListFun, 'undefined').

start_db(TestdListFun, SetupFun) ->
    start_db(TestdListFun, SetupFun, 'undefined').

start_db(TestdListFun, SetupFun, CleanupFun) ->
    {setup
    ,fun() -> setup(SetupFun) end
    ,fun(State) -> cleanup(State, CleanupFun) end
    ,fun(_ReturnOfSetup) ->
             TestdListFun()
     end
    }.

setup('undefined') ->
    #{fixturedb_pid => kz_fixturedb_util:start_me()};
setup(SetupFun) ->
    SetupFun(setup('undefined')).

cleanup(#{fixturedb_pid := Pid}, 'undefined') ->
    kz_fixturedb_util:stop_me(Pid);
cleanup(#{fixturedb_pid := Pid}=State, CleanupFun) ->
    _ = kz_fixturedb_util:stop_me(Pid),
    CleanupFun(State).
