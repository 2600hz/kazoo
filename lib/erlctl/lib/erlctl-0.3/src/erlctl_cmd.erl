% Copyright 2010, Scatterbit, Inc. & Jayson Vantuyl <jvantuyl@scatterbit.com>
%
% This file is part of erlctl.
%
% Erlctl is open source.  See LICENSE.txt for details.
-module (erlctl_cmd).
-export([start/0]).

-include_lib("erlctl/include/internal.hrl").

% Entry Point for Running Commands
-spec( start() -> no_return() ).
start() ->
  ok = init(),
  {ok,Opts} = erlctl_cmdline:process_cmdline(),
  verify_module(Opts),
  run_stage(Opts,always).

verify_module(Opts) ->
  {M,_,_} = get_mfa(Opts),
  case code:which(M) of
    non_existing ->
      App = proplists:get_value(app,Opts),
      erlctl_err:app_not_installed(App);
    _ ->
      ok
  end.

run_stage(Opts0,Stage0) ->
  erlctl_err:info(Opts0,"Running Stage ~p~n",[Stage0]),
  try
    stage(Opts0,Stage0)
  of
    {ok,Opts1,Stage1} ->
      run_stage(Opts1,Stage1);
    wait ->
      receive
        never_comes -> never_exits
      end;
    X ->
      io:format("~p",[X]),
      erlctl_err:halt_with_error() % FIXME: Better Error
  catch
    A:B ->
      io:format("EEK! ~p:~p~n",[A,B]),
      erlctl_err:halt_with_error() % FIXME: Better Error
  end.

init() ->
  % Clear Out Default Log Handlers
  lists:foreach(fun error_logger:delete_report_handler/1,
    gen_event:which_handlers(error_logger)),
  % Start Delegate
  erlctl_proc:start_delegate(),
  ok.

get_mfa(Opts) ->
  proplists:get_value(mfa,Opts).

stage(Opts,Stage) ->
  MFA = get_mfa(Opts),
  Where = where(Opts,Stage),
  erlctl_err:info(Opts,"running stage ~p on ~p as ~p~n",[Stage,Where,MFA]),
  Result = erlctl_proc:run(Where,Stage,MFA),
  handle_result(Opts,Stage,Result).

where(_Opts,always)      -> local;
where(_Opts,not_running) -> local;
where( Opts,running)     -> list_to_atom(proplists:get_value(target,Opts));
where( Opts,started)     -> list_to_atom(proplists:get_value(target,Opts)).

safe_format(F,D) ->
  case
    erlctl:format(F ++ "~n", D)
  of
    ok ->
      ok;
    {error,{C,T}} ->
      io:format(
        "error formatting return message~n"
        "  error_class=~p~n"
        "  error_type=~p~n"
        "  format=~p~n"
        "  data=~p~n",
        [C,T,F,D]
      )
  end.

handle_result(Opts0,always,skip) ->
  {ok,Opts1} = erlctl_net:start_networking(Opts0),
  erlctl:set_opts(Opts1),
  Node = list_to_atom(proplists:get_value(target,Opts1)),
  case net_adm:ping(Node) of
    pang ->
      {ok,Opts1,not_running};
    pong ->
      {ok,Opts1,running}
  end;

handle_result(Opts,Stage,restart) ->
  handle_result(Opts,Stage,{restart,[]});
handle_result(Opts,Stage,{restart,SOpts}) ->
  case erlctl_net:ensure(down,Opts) of
    down ->
      handle_result(Opts,Stage,{start,SOpts});
    up ->
      erlctl_err:cannot_start_vm("Timeout stopping server for restart",[])
  end;
handle_result(Opts,Stage,{restart,SOpts,Msg}) ->
  handle_result(Opts,Stage,{restart,SOpts,Msg,[]});
handle_result(Opts,Stage,{restart,SOpts,Msg,Data})
    when is_list(Msg),is_list(Data) ->
  safe_format(Msg,Data),
  handle_result(Opts,Stage,{restart,SOpts});

handle_result(Opts,Stage,start) ->
  handle_result(Opts,Stage,{start,[]});
handle_result(Opts0,_Stage,{start,SOpts}) ->
  erlctl_err:info(Opts0,"Starting VM with options ~p~n",[SOpts ++ Opts0]),
  {ok,Node} = erlctl_vm:start_vm(SOpts ++ Opts0),
  Opts1 = [{node,Node} | Opts0],
  erlctl:set_opts(Opts1),
  {ok,Opts1,started};
handle_result(Opts,Stage,{start,SOpts,Msg}) when is_list(Msg) ->
  handle_result(Opts,Stage,{start,SOpts,Msg,[]});
handle_result(Opts,Stage,{start,SOpts,Msg,Data})
    when is_list(Msg),is_list(Data) ->
      safe_format(Msg,Data),
  handle_result(Opts,Stage,{start,SOpts});
handle_result(_Opts,_Stage,skip) ->
  erlctl_err:unknown_command(),
  wait;

handle_result(_Opts,_Stage,ok) ->
  erlctl:exit_with_code(0),
  wait;
handle_result(Opts,Stage,{ok,Msg}) when is_list(Msg)->
  handle_result(Opts,Stage,{ok,Msg,[]});
handle_result(Opts,Stage,{ok,Msg,Data}) when is_list(Msg),is_list(Data) ->
  safe_format(Msg,Data),
  handle_result(Opts,Stage,ok);

handle_result(Opts,Stage,error) ->
  handle_result(Opts,Stage,{error,255});
handle_result(_Opts,_Stage,{error,N}) ->
  erlctl:exit_with_code(N),
  wait;
handle_result(Opts,Stage,{error,N,Msg}) when is_list(Msg) ->
  handle_result(Opts,Stage,{error,N,Msg,[]});
handle_result(Opts,Stage,{error,N,Msg,Data})
    when is_list(Msg),is_list(Data) ->
  safe_format(Msg,Data),
  handle_result(Opts,Stage,{error,N});

handle_result(Opts,Ctx,Other) ->
  Msg = "Unexpected Return from Stage ~p:~n  ~p~n",
  Data = [Ctx,Other],
  safe_format(Msg,Data),
  handle_result(Opts,Ctx,{error,254}).
