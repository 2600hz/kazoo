%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Carriers routing WhApp entry module
%%% @end
%%% Created :  14 June 2011 by Karl Anderson <karl@2600hz.org>

-module(carriers_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec(start/2 :: (StartType :: term(), StartArgs :: term()) -> tuple(ok, pid()) | tuple(error, term())).
start(_StartType, _StartArgs) ->
    case carriers:start_link() of
	{ok, P} -> {ok, P};
	{error,{already_started, P}} -> {ok, P};
	{error, _}=E -> E
    end.

stop(_State) ->
    ok.
