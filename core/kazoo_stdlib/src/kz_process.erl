%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_process).

-export([runs_in/3
        ,spawn/1, spawn/2
        ,spawn_link/1, spawn_link/2
        ,spawn_monitor/2, spawn_monitor/3
        ]).

%%------------------------------------------------------------------------------
%% @doc Gives `MaxTime' milliseconds to `Fun' of `Arguments' to apply.
%% If time is elapsed, the sub-process is killed and returns `timeout'.
%% @end
%%------------------------------------------------------------------------------
-spec runs_in(number(), fun(), list()) -> {'ok', any()} | 'timeout'.
runs_in(MaxTime, Fun, Arguments)
  when is_integer(MaxTime), MaxTime > 0 ->
    {Parent, Ref} = {self(), erlang:make_ref()},
    Child = ?MODULE:spawn(fun () -> Parent ! {Ref, erlang:apply(Fun, Arguments)} end),
    receive {Ref, Result} -> {'ok', Result}
    after MaxTime ->
            exit(Child, 'kill'),
            'timeout'
    end;
runs_in(MaxTime, Fun, Arguments)
  when is_number(MaxTime), MaxTime > 0 ->
    runs_in(kz_term:to_integer(MaxTime), Fun, Arguments).

-spec spawn(fun(), list()) -> pid().
spawn(Fun, Arguments) ->
    CallId = kz_log:get_callid(),
    Application = kapps_util:get_application(),
    erlang:spawn(fun() ->
                         _ = kz_log:put_callid(CallId),
                         _ = kapps_util:put_application(Application),
                         erlang:apply(Fun, Arguments)
                 end).

-spec spawn(fun(() -> any())) -> pid().
spawn(Fun) ->
    CallId = kz_log:get_callid(),
    Application = kapps_util:get_application(),
    erlang:spawn(fun() ->
                         _ = kz_log:put_callid(CallId),
                         _ = kapps_util:put_application(Application),
                         Fun()
                 end).

-spec spawn_link(fun(), list()) -> pid().
spawn_link(Fun, Arguments) ->
    CallId = kz_log:get_callid(),
    Application = kapps_util:get_application(),
    erlang:spawn_link(fun () ->
                              _ = kz_log:put_callid(CallId),
                              _ = kapps_util:put_application(Application),
                              erlang:apply(Fun, Arguments)
                      end).

-spec spawn_link(fun(() -> any())) -> pid().
spawn_link(Fun) ->
    CallId = kz_log:get_callid(),
    Application = kapps_util:get_application(),
    erlang:spawn_link(fun() ->
                              _ = kz_log:put_callid(CallId),
                              _ = kapps_util:put_application(Application),
                              Fun()
                      end).

-spec spawn_monitor(fun(), list()) -> kz_term:pid_ref().
spawn_monitor(Fun, Arguments) ->
    CallId = kz_log:get_callid(),
    Application = kapps_util:get_application(),
    erlang:spawn_monitor(fun () ->
                                 _ = kz_log:put_callid(CallId),
                                 _ = kapps_util:put_application(Application),
                                 erlang:apply(Fun, Arguments)
                         end).

-spec spawn_monitor(module(), atom(), list()) -> kz_term:pid_ref().
spawn_monitor(Module, Fun, Args) ->
    CallId = kz_log:get_callid(),
    Application = kapps_util:get_application(),
    erlang:spawn_monitor(fun () ->
                                 _ = kz_log:put_callid(CallId),
                                 _ = kapps_util:put_application(Application),
                                 erlang:apply(Module, Fun, Args)
                         end).
