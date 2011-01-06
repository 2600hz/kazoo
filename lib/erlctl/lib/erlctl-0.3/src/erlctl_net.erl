% Copyright 2010, Scatterbit, Inc. & Jayson Vantuyl <jvantuyl@scatterbit.com>
%
% This file is part of erlctl.
%
% Erlctl is open source.  See LICENSE.txt for details.
-module (erlctl_net).
-export([start_networking/1,ensure/2]).

-include_lib("erlctl/include/internal.hrl").

start_networking(Opts) ->
  erlctl_err:info(Opts,"starting epmd~n"),
  start_epmd(),
  case proplists:get_value(names,Opts,?DEF_NAMES) of
    long -> Names = longnames;
    short -> Names = shortnames
  end,
  CN = cli_nodename(Opts),
  erlctl_err:info(Opts,"using ~p with name ~p~n",[Names,CN]),
  case net_kernel:start([CN,Names]) of
    {ok,_} ->
      ok;
    {error,Error} ->
      erlctl_err:networking_failure(Error)
  end,
  erlctl_err:info(Opts,"started as ~p~n",[node()]),
  SvrNode = svr_nodename(Opts),
  {ok,[{target,SvrNode} | Opts]}.

start_epmd() ->
  start_epmd(0).

start_epmd(N) ->
  case {N,erl_epmd:names()} of % FIXME: undocumented function call
    {_,{ok,_}} -> % EPMD running
      started;
    {N,_} when N >= ?EPMD_TRIES -> % checked too many times, error
      erlctl_err:networking_failure("EPMD failed to start!");
    {0,_} -> % first time, try to start EPMD if it wasn't running
      os:cmd(os:find_executable("epmd") ++ " -daemon"),
      timer:sleep(?EPMD_CHECK_DELAY),
      start_epmd(1);
    {N,_} -> % wait another cycle
      timer:sleep(?EPMD_CHECK_DELAY),
      start_epmd(N+1)
  end.

ensure(Intent,Opts) when is_atom(Intent),is_list(Opts) ->
  Node = list_to_atom(proplists:get_value(target,Opts)),
  ensure(Intent,Node);
ensure(up,Node) when is_atom(Node) ->
  ensure({pong,pang,up,down},Node);
ensure(down,Node) when is_atom(Node) ->
  ensure({pang,pong,down,up},Node);
ensure({Good,Bad,Success,Failure},Node) when is_atom(Node) ->
  ensure_loop(Good,Bad,Success,Failure,Node,30).

ensure_loop(_Good,_Bad,_Success,Failure,_Node,0) ->
  Failure;
ensure_loop(Good,Bad,Success,Failure,Node,Tries) ->
  case net_adm:ping(Node) of
    Good ->
      Success;
    Bad ->
      timer:sleep(100),
      ensure_loop(Good,Bad,Success,Failure,Node,Tries - 1)
  end.

is_longname(Name) -> lists:member($.,Name).

make_hostname(short,auto) ->
  {ok,HN} = inet:gethostname(),
  HN;
make_hostname(short,Manual) ->
  Manual;
make_hostname(long,auto) ->
  {ok,HN} = inet:gethostname(),
  DN = inet_db:res_option(domain), % Networking Must Be Running Here!
  HN ++ "." ++ DN;
make_hostname(long,Manual) ->
  case is_longname(Manual) of
    true ->
      Manual;
    false ->
      DN = inet_db:res_option(domain),
      Manual ++ "." ++ DN % Networking Must Be Running Here!
  end.

get_hostname(Opts) ->
  NmOpt = proplists:get_value(names,Opts,?DEF_NAMES),
  HnOpt = proplists:get_value(host,Opts,auto),
  HostName = make_hostname(NmOpt,HnOpt),
  case {NmOpt,is_longname(HostName)} of
    {short,true} ->
      erlctl_err:format(
        "Warning: using name with dot as a shortname (~p)~n",[HostName]),
        erlctl_err:networking_failure(mixed_names);
    {long,false} ->
      erlctl_err:format(
        "Warning: using name without a dot as a longname (~p)~n",[HostName]),
        erlctl_err:networking_failure(mixed_names);
    _ ->
      ok
  end,
  HostName.

cli_nodename(Opts) ->
  AppName = proplists:get_value(app,Opts),
  list_to_atom(AppName ++ "ctl_" ++ os:getpid() ++ "@" ++get_hostname(Opts)).

svr_nodename(Opts) ->
  case proplists:get_value(fullnode,Opts) of
    undefined ->
      HostName = get_hostname(Opts),
      case proplists:get_value(node,Opts) of
        undefined ->
          NodeName = proplists:get_value(app,Opts);
        NName ->
          NodeName = NName
      end,
      NodeName ++ "@" ++ HostName;
    NodeName ->
      NodeName % FIXME: Verify fully specified node names?
  end.
