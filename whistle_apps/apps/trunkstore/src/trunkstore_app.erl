-module(trunkstore_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("ts.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(term(), term()) -> {'ok', pid()} | {'error', term()}.
start(_StartType, _StartArgs) ->
    case trunkstore:start_link() of
        {ok, P} -> {ok, P};
        {error,{already_started, P}} -> {ok, P};
        {error, _}=E -> E;
        ignore -> {error, failed_to_start}
    end.

stop(_State) ->
    ok.
