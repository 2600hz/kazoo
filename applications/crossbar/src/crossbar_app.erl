%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(crossbar_app).

-behaviour(application).

-include_lib("kazoo/include/kz_types.hrl").

%% Application callbacks
-export([start/2, stop/1]).

-include("crossbar.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> startapp_ret().
start(_StartType, _StartArgs) ->
    _ = kapps_maintenance:bind('migrate', 'crossbar_maintenance', 'migrate'),
    _ = kapps_maintenance:bind({'refresh_account', <<"*">>}, 'crossbar_util', 'descendants_count'),
    crossbar_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    _ = kapps_maintenance:unbind('migrate', 'crossbar_maintenance', 'migrate'),
    _ = kapps_maintenance:unbind({'refresh_account', <<"*">>}, 'crossbar_util', 'descendants_count'),
    _ = cowboy:stop_listener('api_resource'),
    _ = cowboy:stop_listener('api_resource_ssl'),
    _ = crossbar_bindings:flush(),
    'ok'.
