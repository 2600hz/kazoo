%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 27 June 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(conference_app).

-behaviour(application).

-include_lib("whistle/include/wh_types.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec start(StartType, StartArgs) -> tuple(ok, pid()) | tuple(error, startlink_err()) when
      StartType :: term(),
      StartArgs :: term().
start(_, _) ->
    case conference:start_link() of
	{ok, P} -> {ok, P};
	{error, {already_started, P} } -> {ok, P};
	{error, _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(State) -> ok when
      State :: term().
stop(_) ->
    ok.
