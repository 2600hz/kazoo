% Copyright 2010, Scatterbit, Inc. & Jayson Vantuyl <jvantuyl@scatterbit.com>
%
% This file is part of erlctl.
%
% Erlctl is open source.  See LICENSE.txt for details.
-module (erlctl).
-export([start/1,start_ack/1]).
-export([format/1,format/2,exit_with_code/1,server_exit/0]).
-export([get_opts/0,set_opts/1]).

start([NotifyNode]) ->
  ok = application:start(sasl),
  ok = application:start(erlctl),
  spawn(NotifyNode,erlctl,start_ack,[node()]).

start_ack(StartedNode) ->
  erlctl_cmd_runner ! {vm_started,StartedNode}.

exit_with_code(Code) ->
  erlctl_proc:send({halt,Code}),
  ok.

server_exit() ->
  timer:apply_after(250,init,stop,[]).

format(Format) ->
  erlctl_proc:send({format,Format,[]}).

format(Format,Data) ->
  erlctl_proc:send({format,Format,Data}).

get_opts() ->
  erlctl_proc:send(get_opts).

set_opts(Opts) ->
  erlctl_proc:send({set_opts,Opts}).
