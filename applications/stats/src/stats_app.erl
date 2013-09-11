%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%    Stephen Gibberd <stephen.gibberd@2600hz.com>
%%%-------------------------------------------------------------------
-module(stats_app).

-behaviour(application).

-include_lib("whistle/include/wh_types.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec start(term(), term()) ->
                   {'ok', pid()} |
                   {'error', startlink_err()}.
start(_Type, _Args) -> stats:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(term()) -> 'ok'.
stop(_State) -> stats:stop().
