%%%-------------------------------------------------------------------
%%% @copyright (C) 2016-2018, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(tasks_app).
-behaviour(application).

-include("tasks.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    _ = kapi_self:declare_exchanges(),
    _ = kapi_tasks:declare_exchanges(),
    _ = kz_datamgr:revise_views_from_folder(?KZ_TASKS_DB, 'tasks'),
    Ok = tasks_sup:start_link(),
    _ = tasks_bindings:init(),
    Ok.

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> 'ok'.
stop(_State) ->
    tasks_bindings:flush().
