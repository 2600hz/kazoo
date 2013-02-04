-module(cdr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec(start(StartType :: term(), StartArgs :: term()) -> tuple(ok, pid()) | tuple(error, term())).
start(_StartType, _StartArgs) ->
    case cdr:start_link() of
	{ok, P} -> {ok, P};
	{error,{already_started, P}} -> {ok, P};
	{error, _}=E -> E
    end.

stop(_State) ->
    ok.
