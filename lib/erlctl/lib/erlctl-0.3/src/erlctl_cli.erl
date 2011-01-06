% Copyright 2010, Scatterbit, Inc. & Jayson Vantuyl <jvantuyl@scatterbit.com>
%
% This file is part of erlctl.
%
% Erlctl is open source.  See LICENSE.txt for details.
-module (erlctl_cli).
-export([help/2]).

help(always,[]) ->
  erlctl:format("Usage: erlctl <app> [<command> ...]~n"),
  erlctl:exit_with_code(1).
