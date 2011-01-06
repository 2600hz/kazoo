% Copyright 2010, Scatterbit, Inc. & Jayson Vantuyl <jvantuyl@scatterbit.com>
%
% This file is part of erlctl.
%
% Erlctl is open source.  See LICENSE.txt for details.
-module (erlctl_cmdline).
-export([process_cmdline/0]).

% Command Line Handling Functions
process_cmdline() ->
  try
    do_process_cmdline()
  catch
    error:{sysarg,[BadArg | _Rest] } ->
      erlctl_err:bad_cmdline("bad system argument: ~p",[BadArg]);
    C:T ->
      erlctl_err:bad_cmdline("unknown problem (~p:~p) processing arguments!",
        [C,T])
  end.

do_process_cmdline() ->
  % Get Script Name and Args
  [ScriptName | Args0] = init:get_plain_arguments(),
  RunName = filename:basename(ScriptName),
  CmdLine = [ RunName | Args0],
  Opts0 = [ {cmdline,CmdLine}, {script,RunName} ],
  erlctl:set_opts(Opts0),
  % Process Options and Arguments
  {ok,Opts1,Args1} = handle_sysargs(Args0,Opts0),
  erlctl:set_opts(Opts1),
  % Infer Usage From Name of Control Script and Args
  {ok,AppName,Cmd,Args2} = handle_cmd(RunName,Args1),
  Module = list_to_atom(AppName ++ "_cli"),
  Function = list_to_atom(Cmd),
  Opts2 = [
    {app,AppName},
    {command,Cmd},
    {mfa,{Module,Function,Args2}}
    | Opts1
  ],
  erlctl:set_opts(Opts2),
  {ok,Opts2}.

handle_cmd(RunName,Args) ->
  case normalize(RunName,Args) of
    % "erlctl", show generic usage
    [] ->
      {ok,"erlctl","help",[]};
    % "erlctl <app>" or "<app>ctl", show app usage
    [App] ->
      {ok,App,"help",[]};
    % "erlctl <app> <cmd> ..." or "<app>ctl <cmd> ...", run command
    [App, Command | CArgs] ->
      {ok,App,Command,CArgs}
  end.

normalize("erlctl",Args) -> % Strip off "erlctl"
  Args;
normalize(Name,Args) -> % Convert "<app>_?ctl" into "<app>"
  Len = length(Name),
  case lists:suffix("ctl",Name) of
    true ->
      case lists:suffix("_ctl",Name) of
        true ->
          [ lists:sublist(Name,Len - 4) | Args ];
        false ->
          [ lists:sublist(Name,Len - 3) | Args ]
      end;
    false ->
      [Name | Args]
  end.

% System Arguments
handle_sysargs(["-v" | Rest ], Opts) ->
  handle_sysargs(Rest,[verbose | Opts]);
handle_sysargs(["-h",HostName | Rest ], Opts) ->
  handle_sysargs(Rest,[{host,HostName}   | Opts]);
handle_sysargs(["-l"          | Rest ], Opts) ->
  handle_sysargs(Rest,[{names,long}      | Opts]);
handle_sysargs(["-s"          | Rest ], Opts) ->
  handle_sysargs(Rest,[{names,short}     | Opts]);
handle_sysargs(["-N",FN       | Rest ], Opts) ->
  handle_sysargs(Rest,[{fullnode,FN}     | Opts]);
handle_sysargs(["-n",NodeName | Rest ], Opts) ->
  handle_sysargs(Rest,[{node,NodeName}   | Opts]);
handle_sysargs(["-c",ConfFile | Rest ], Opts) ->
  handle_sysargs(Rest,[{config,ConfFile} | Opts]);
handle_sysargs([ [X | _] | _ ] = Args, Opts) when X =/= $- ->
  {ok,Opts,Args};
handle_sysargs([],Opts) ->
  {ok,Opts,[]};
handle_sysargs(Args, _Opts) ->
  erlang:error({sysarg,Args}).

