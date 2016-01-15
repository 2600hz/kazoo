%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(fax_app).

-behaviour(application).

-include_lib("whistle/include/wh_types.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec start(any(), any()) ->
                   {'ok', pid()} |
                   {'error', startlink_err()}.
start(_Type, _Args) -> fax_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(any()) -> 'true'.
stop(_State) ->
    'ok' = cowboy:stop_listener('fax_file'),
    exit(whereis('fax_sup'), 'shutdown').
