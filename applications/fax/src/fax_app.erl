%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(fax_app).

-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([start/2, stop/1]).
-export([register_views/0]).

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    register_views(),
    _ = kapps_maintenance:bind('migrate', 'fax_maintenance', 'migrate'),
    _ = kapps_maintenance:bind_and_register_views('fax', 'fax_app', 'register_views'),
    fax_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    _ = kapps_maintenance:unbind('migrate', 'fax_maintenance', 'migrate'),
    _ = kapps_maintenance:unbind('register_views', 'fax_app', 'register_views'),
    _ = cowboy:stop_listener('fax_file'),
    'ok'.

-spec register_views() -> 'ok'.
register_views() ->
    kz_datamgr:register_views_from_folder('fax'),
    'ok'.
