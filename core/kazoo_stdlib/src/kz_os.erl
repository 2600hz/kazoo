%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Functions for interacting with the underlying system (assumes Linux).
%%% @author Sean Wysor
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_os).

-export([cmd/1, cmd/2, cmd/3]).

-export([kill/1, brutally_kill/1]).

-export([run_cmd/3]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(DEFAULT_TIMEOUT, 10000).
-define(DEFAULT_MAX_SIZE, 1000000).
-define(DEFAULT_ABSOLUTE_TIMEOUT, 10000).
-define(DEFAULT_READ_MODE, {'line', 1000}).

%% @equiv cmd(Command, [])
-spec cmd(iodata()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', any(), binary()}.
cmd(Command) ->
    cmd(Command, []).

%% @equiv cmd(Command, Args, [])
-spec cmd(iodata(), kz_term:proplist()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', any(), binary()}.
cmd(Command, Args) ->
    cmd(Command, Args, []).

%%------------------------------------------------------------------------------
%% @doc Execute system commands safely.
%%
%% Execute a system command safely with protection from unexpectedly
%% large output or commands that run forever. This is implemented
%% using erlang:open_ports.
%%
%% For commands which require injection of arguments, simply name the
%% arguments in the command with bash variables, add the variable
%% names and values to a proplist of `Args' and they will be injected
%% into the command via environment variables. This decouples the
%% ordering from the commands, which was a limitation in the
%% customizability of the old io_lib:format/os:cmd method of
%% running/storing user customizable commands.
%%
%% For commands that do not require injection of arguments, simply use
%% {@link cmd/1} or specify an empty list in {@link cmd/3}. {@link
%% cmd/2} is used when default options are fine, but arguments are
%% required.
%%
%% cmd/3 permits read mode ports options as well as timeout and size
%% thresholds.  `binary', `exit_status', `use_stdio', and
%% `stderr_to_stdout' are always set as ports options as their use is
%% assumed in processing the command.
%%
%% Examples:
%% ```
%%    {'ok', <<"10\n">>} = kz_os:cmd(<<"echo 10">>).
%%    {'ok', <<"10\n">>} = kz_os:cmd(<<"echo -n $ARG">>, [{<<"ARG">>, 10}]).
%%    {'error', 'timeout', <<>>} = kz_os:cmd(<<"sleep 1;echo -n $ARG">>, [{<<"ARG">>, 10}], [{<<"timeout">>, 100}]).
%%    {'ok', 10} = kz_os:cmd(<<"echo -n $ARG">>, [{<<"ARG">>, 10}], [{<<"read_mode">>, 'stream'}]).
%% '''
%%
%% <ul>
%%    <li><strong>`timeout', value</strong>The time the command will wait in milliseconds with no output before the process is killed. The default is 10 seconds</li>
%%    <li><strong>`absolute_timeout', value</strong>The absolute time in milliseconds a command is permitted to run before it is killed. The default is 10 seconds</li>
%%    <li><strong>`max_size'</strong>The max size of output in bytes to allow before the process is killed. The default is 1,000,000 bytes</li>
%%    <li><strong>`read_mode'</strong>The default readmode is `{line, 1000}', but `stream' can be used when you want to run a command which does not emit newlines.</li>
%%    <li></li>
%% </ul>
%%
%% @end
%%------------------------------------------------------------------------------

-spec cmd(iodata(), kz_term:proplist(), kz_term:proplist()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', any(), binary()}.
cmd(Command, Args, Options) ->
    Owner = props:get_value(<<"owner">>, Options, self()),
    CmdTimeout = props:get_value(<<"absolute_timeout">>, Options, ?DEFAULT_ABSOLUTE_TIMEOUT),
    CmdOptions = [Command
                 ,Args
                 ,props:set_value(<<"owner">>, Owner, Options)
                 ],
    {Pid, Ref} = erlang:spawn_monitor(?MODULE, 'run_cmd', CmdOptions),
    monitor_cmd(Pid, Ref, CmdTimeout, 'undefined').

-spec monitor_cmd(pid(), reference(), non_neg_integer(), kz_term:api_port()) ->
          {'ok', kz_term:ne_binary()}|
          {'error', any(), binary()}.
monitor_cmd(Pid, Ref, Timeout, Port) ->
    receive
        {'port', NewPort, Pid} ->
            monitor_cmd(Pid, Ref, Timeout, NewPort);
        {{'ok', _}=Ok, Pid} ->
            _ = erlang:demonitor(Ref, ['flush']),
            Ok;
        {{'error', _, _}=Error,Pid} ->
            _ = erlang:demonitor(Ref, ['flush']),
            lager:info("cmd errored: ~p", [Error]),
            Error;
        {'DOWN', Ref, _, Pid, Reason} ->
            _ = erlang:demonitor(Ref, ['flush']),
            lager:info("cmd process died unexpectedly with reason: ~p", [Reason]),
            {'error', 'died_unexpectedly', <<>>};
        Else ->
            lager:debug("unexpected message ~p", [Else]),
            monitor_cmd(Pid, Ref, Timeout, Port)
    after
        Timeout ->
            maybe_kill_cmd(Port),
            _ = erlang:demonitor(Ref, ['flush']),
            _ = erlang:exit(Pid, 'timeout'),
            lager:info("command timed out after ~pms", [Timeout]),
            {'error', 'absolute_timeout', <<>>}
    end.

-spec run_cmd(iodata(), kz_term:proplist(), kz_term:proplist()) ->
          {{'ok', kz_term:ne_binary()} |
           {'error', atom(), kz_term:ne_binary()}
          ,pid()
          }.
run_cmd(Command, Args, Options) ->
    OwnerPid = props:get_value(<<"owner">>, Options),
    OwnerRef = erlang:monitor('process', OwnerPid),
    Timeout = props:get_value(<<"timeout">>, Options, ?DEFAULT_TIMEOUT),
    MaxSize = props:get_value(<<"max_size">>, Options, ?DEFAULT_MAX_SIZE),
    ReadMode = props:get_value(<<"read_mode">>, Options, ?DEFAULT_READ_MODE),
    PortOptions = [ReadMode
                  ,'binary'
                  ,'exit_status'
                  ,'use_stdio'
                  ,'stderr_to_stdout'
                  ,{'env', opts_to_strings(Args)}
                  ],
    Port = erlang:open_port({'spawn', kz_term:to_list(Command)}, PortOptions),
    OwnerPid ! {'port', Port, self()},
    Out = cmd_read({Port, MaxSize, Timeout, OwnerRef}, <<>>),
    OwnerPid ! {Out, self()}.

-spec cmd_read({port(), integer(), integer(), reference()}, binary()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', atom(), binary()}.
cmd_read({Port, _MaxSize, Timeout, OwnerRef}=LoopParams, Acc) ->
    receive
        {'DOWN', OwnerRef, _, _, _}  ->
            lager:debug("parent died - no reason to continue"),
            {'error', 'parent_died', <<>>};
        {Port, {'data', {'eol', Data}}} ->
            cmd_read_data(LoopParams, kz_binary:join([Acc, Data, <<"\n">>], <<>>));
        {Port, {'data', {'noeol', Data}}} ->
            cmd_read_data(LoopParams, <<Acc/binary,Data/binary>>);
        {Port, {'data', Data}} ->
            cmd_read_data(LoopParams, <<Acc/binary,Data/binary>>);
        {Port, {'exit_status', 0}} ->
            {'ok', Acc};
        {Port, {'exit_status', Status}} ->
            {'error', Status, Acc};
        Any ->
            lager:debug("unhandled message ~p", [Any]),
            cmd_read(LoopParams, Acc)
    after
        Timeout ->
            lager:debug("timeout reached on command ~p", [Timeout]),
            _ = maybe_kill_cmd(Port),
            {'error', 'timeout', Acc}
    end.

cmd_read_data({Port, MaxSize, _, _}=LoopParams, Data) ->
    case byte_size(Data) of
        Len when Len >= MaxSize ->
            _ = maybe_kill_cmd(Port),
            {'error', 'max_size', Data};
        _ -> cmd_read(LoopParams, Data)
    end.


-spec maybe_kill_cmd(kz_term:api_port()) -> 'ok'.
maybe_kill_cmd('undefined') ->
    'ok';
maybe_kill_cmd(Port) ->
    try erlang:port_info(Port, 'os_pid') of
        {'os_pid', OsPid} -> kill(OsPid);
        _ -> 'ok'
    catch
        _ -> 'ok'
    end.

-spec kill(integer()) -> 'ok'.
kill(OsPid) ->
    lager:debug("killing pid: ~p with SIGINT: good day sir!", [OsPid]),
    case os:cmd(io_lib:format("kill -6 ~b", [OsPid])) of
        "" -> 'ok';
        _ ->
            lager:debug("SIGINT kill failed!"),
            brutally_kill(OsPid)
    end.

-spec brutally_kill(integer()) -> 'ok'.
brutally_kill(OsPid) ->
    lager:debug("brutally killing ~p with SIGKILL: I said good day!", [OsPid]),
    case os:cmd(io_lib:format("kill -9 ~b", [OsPid])) of
        "" -> 'ok';
        _ ->
            lager:debug("brutal kill failed, process ~b is still running!", [OsPid]),
            'ok'
    end.

-spec opts_to_strings(kz_term:proplist()) -> kz_term:proplist().
opts_to_strings(Args) ->
    opts_to_strings(Args, []).

-spec opts_to_strings(kz_term:proplist(), kz_term:proplist()) -> kz_term:proplist().
opts_to_strings([{Key, Value}|Args], Acc) ->
    opts_to_strings(Args, Acc ++ [{kz_term:to_list(Key), kz_term:to_list(Value)}]);
opts_to_strings([], Acc) ->
    Acc.
