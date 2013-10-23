%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(fax).

-include("fax.hrl").

-export([start_link/0
         ,start/0
         ,stop/0
        ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    _ = start_deps(),
    _ = declare_exchanges(),
    Dispatch = cowboy_router:compile([
                                      %% {HostMatch, list({PathMatch, Handler, Opts})}
                                      {'_', [{<<"/fax/[...]">>, 'fax_file_proxy', []}]}
                                     ]),

    Port = whapps_config:get_integer(?CONFIG_CAT, <<"port">>, 30950),
    Workers = whapps_config:get_integer(?CONFIG_CAT, <<"workers">>, 50),
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_http('fax_file', Workers
                      ,[{'port', Port}]
                      ,[{'env', [{'dispatch', Dispatch}]}]
                     ),
    fax_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the application
%% @end
%%--------------------------------------------------------------------
-spec start() -> 'ok' | {'error', _}.
start() ->
    application:start(?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop() -> 'ok'.
stop() ->
    exit(whereis('fax_sup'), 'shutdown'),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps() -> 'ok'.
start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    _ = [wh_util:ensure_started(App) || App <- ['crypto'
                                                ,'inets'
                                                ,'lager'
                                                ,'whistle_amqp'
                                                ,'whistle_couch'
                                                ,'ranch'
                                                ,'cowboy'
                                               ]],
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all exchanges used are declared
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = wapi_fax:declare_exchanges(),
    _ = wapi_notifications:declare_exchanges(),
    _ = wapi_offnet_resource:declare_exchanges(),
    _ = wapi_call:declare_exchanges(),
    _ = wapi_dialplan:declare_exchanges(),
    wapi_self:declare_exchanges().
