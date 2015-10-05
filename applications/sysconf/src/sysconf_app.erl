%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(sysconf_app).

-behaviour(application).

-include("sysconf.hrl").

%% Application callbacks
-export([start/2
         ,stop/1
        ]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec start(_, _) ->
                   {'ok', pid()} |
                   {'error', startlink_err()}.
start(_StartType, _StartArgs) -> sysconf:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(_) -> 'ok'.
stop(_State) -> sysconf:stop().
