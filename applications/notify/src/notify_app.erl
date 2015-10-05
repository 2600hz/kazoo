%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(notify_app).

-behaviour(application).

-include_lib("whistle/include/wh_types.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec start(_, _) ->
                   {'ok', pid()} |
                   {'error', _}.
start(_StartType, _StartArgs) -> notify:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(_) -> 'ok'.
stop(_State) -> notify:stop().
