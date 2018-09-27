%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2018, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_number_manager_app).
-behaviour(application).

-include("knm.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%==============================================================================
%% Application callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    declare_exchanges(),
    _ = kz_datamgr:register_view('numbers', ?APP, "views/numbers.json"),
    kapps_maintenance:bind({'migrate', <<"4.0">>}, 'kazoo_number_manager_maintenance', 'migrate'),
    kapps_maintenance:bind({'refresh_account', <<"*">>}, 'kazoo_number_manager_maintenance', 'update_number_services_view'),
    kapps_maintenance:bind_and_register_views(?APP, 'kazoo_number_manager_maintenance', 'register_views'),
    kazoo_number_manager_maintenance:init_dbs(),
    kazoo_number_manager_sup:start_link().

-spec stop(any()) -> any().
stop(_State) ->
    kapps_maintenance:unbind({'migrate', <<"4.0">>}, 'kazoo_number_manager_maintenance', 'migrate'),
    kapps_maintenance:unbind({'refresh_account', <<"*">>}, 'kazoo_number_manager_maintenance', 'update_number_services_view'),
    'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_discovery:declare_exchanges(),
    kapi_self:declare_exchanges().
