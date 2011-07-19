%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010 James Aimonetti
%%% @doc
%%%
%%% @end
%%% Created :  3 May 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec start/2 :: (StartType, StartArgs) -> tuple(ok, pid()) | tuple(error, term()) when
      StartType :: term(),
      StartArgs :: term().
start(_StartType, _StartArgs) ->
    case notify:start_link() of
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
-spec stop/1 :: (State) -> ok when
      State :: term().
stop(_State) ->
    ok.
