% Copyright 2010, Scatterbit, Inc. & Jayson Vantuyl <jvantuyl@scatterbit.com>
%
% This file is part of erlctl.
%
% Erlctl is open source.  See LICENSE.txt for details.
-module(ctl_ex_sup).
-behavior(supervisor).

-export([start_link/0,init/1]).

start_link() ->
  supervisor:start_link(?MODULE,[]).

init(_Args) ->
  Restart = {one_for_all,5,1},
  Children = [
      {ctl_ex,{ctl_ex,start_link,[]},permanent,1000,worker,[ctl_ex]}
    ],
  {ok,{Restart,Children}}.
