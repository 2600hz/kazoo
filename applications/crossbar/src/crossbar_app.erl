%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(crossbar_app).

-behaviour(application).

-include_lib("whistle/include/wh_types.hrl").

%% Application callbacks
-export([start/2, stop/1]).

-include("crossbar.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec start(_, _) ->
                   {'ok', pid()} |
                   {'ok', pid(), _} |
                   {'error', startlink_err()}.
start(_StartType, _StartArgs) -> crossbar:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(_) -> 'ok'.
stop(_State) -> crossbar:stop().
