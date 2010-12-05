%%% @author Karl Anderson <james@2600hz.org>
%%% @copyright (C) 2010 Karl Anderson
%%% @doc
%%% 
%%% @end
%%% Created :  Sun, 05 Dec 2010 07:58:36 GMT: Karl Anderson <james@2600hz.org>
-module(restful_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec(start/2 :: (StartType :: term(), StartArgs :: term()) -> tuple(ok, pid()) | tuple(error, term())).
start(_StartType, _StartArgs) ->
    case restful:start_link() of
	{ok, P} -> {ok, P};
	{error, {already_started, P} } -> {ok, P};
	{error, _}=E -> E
    end.

stop(_State) ->
    ok.
