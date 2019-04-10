%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(tasks_app).
-behaviour(application).

-include("tasks.hrl").

-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    _ = kapi_self:declare_exchanges(),
    _ = kapi_tasks:declare_exchanges(),
    kapps_maintenance:bind_and_register_views(?APP, 'tasks_maintenance', 'register_views'),
    _ = kz_datamgr:db_create(?KZ_TASKS_DB),
    _ = kapps_maintenance:refresh(?KZ_TASKS_DB),
    Ok = tasks_sup:start_link(),
    _ = tasks_bindings:init(),
    Ok.

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> 'ok'.
stop(_State) ->
    _ = kapps_maintenance:unbind('register_views', 'tasks_maintenance', 'register_views'),
    tasks_bindings:flush().
