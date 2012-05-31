%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

-module(couchbeam_app).

-behaviour(application).

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
    couchbeam_util:start_app_deps(couchbeam),
    couchbeam_sup:start_link().

%%--------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

