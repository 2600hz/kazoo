-module(monitor_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("../include/monitor_amqp.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec(start/2 :: (StartType :: term(), StartArgs :: term()) -> tuple(ok, pid()) | tuple(error, term())).
start(_StartType, _StartArgs) ->
    case monitor:start_link(?AMQP_HOST) of
	{ok, P} -> {ok, P};
	{error,{already_started, P}} -> {ok, P};
	{error, _}=E -> E
    end.

stop(_State) ->
    ok.
