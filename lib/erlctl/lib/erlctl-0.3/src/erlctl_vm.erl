% Copyright 2010, Scatterbit, Inc. & Jayson Vantuyl <jvantuyl@scatterbit.com>
%
% This file is part of erlctl.
%
% Erlctl is open source.  See LICENSE.txt for details.
-module (erlctl_vm).
-export([start_vm/1]).

-include_lib("erlctl/include/internal.hrl").

start_vm(Opts) ->
  % Build Target Node and Networking Info
  TgtName = proplists:get_value(target,Opts),
  TgtNode = list_to_atom(TgtName),
  case proplists:get_value(names,Opts,?DEF_NAMES) of
    long ->
      NameType = "-name";
    short ->
      NameType = "-sname"
  end,
  % Build VM Parameters
  NameArgs   = [NameType,TgtName],
  DaemonArgs = ["-detached","-noshell","-mode","interactive"],
  StartArgs  = ["-s","erlctl","start",atom_to_list(node())],
  Args = NameArgs ++ DaemonArgs ++ StartArgs,
  % Find VM Binary
  Path = case os:find_executable("erl") of
    false ->
      erlctl_err:cannot_start_vm("cannot find executable",[]);
    FoundPath ->
      FoundPath
  end,
  % Register Process To Catch Started Message
  register(erlctl_cmd_runner,self()),
  % Open VM As Port
  Opts0 = [ {args,Args}, exit_status, hide ],
  erlctl_err:info(Opts,"spawning ~p ~p~n",[Path,Args]),
  Port = try
    open_port({spawn_executable,Path},Opts0)
  catch
    error:badarg ->
      Spawn = lists:flatten( [ [X,$ ] || X <- [Path | Args]] ),
      Opts1 = [ exit_status ],
      open_port({spawn,Spawn},Opts1)
  end,
  % Wait for It To Fail or Daemonize
  receive
    {Port,{exit_status,0}} ->
      started;
    {Port,{exit_status,X}} ->
      erlctl_err:cannot_start_vm("exited with error ~p",[X])
  end,
  % Wait for Node To Report In
  receive
    {vm_started,TgtNode} ->
      erlctl_err:info(Opts,"remote node started as ~p~n",[TgtNode]),
      {ok,TgtNode};
    {vm_started,ActualNode} ->
      erlctl_err:format("Unexpected VM name ~p~n",[ActualNode]),
      {ok,ActualNode}
    after ?STARTUP_DELAY ->
      erlctl_err:cannot_start_vm("timed out waiting for VM to start",[]),
      {error,timeout} % never reached
  end.
