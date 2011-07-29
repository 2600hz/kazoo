%%%-------------------------------------------------------------------
%%% @author Edouard Swiac <edouard@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created :  29 Jul 2011 by Edouard Swiac <edouard@2600hz.org>
%%%-------------------------------------------------------------------
-module(hotdesking_app).

-behaviour(application).

-include_lib("whistle/include/whistle_types.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec start/2 :: (StartType, StartArgs) -> tuple(ok, pid()) | tuple(error, startlink_err()) when
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
