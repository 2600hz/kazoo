%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(dth_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(term(), term()) ->
                   {'ok', pid()} |
                   {'error', term()}.
start(_StartType, _StartArgs) ->
    case dth:start_link() of
        {'ok', P} -> {'ok', P};
        {'error', {'already_started', P} } -> {'ok', P};
        {'error', _}=E -> E
    end.

stop(_State) -> dth:stop().
