%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(fax).

-include("fax.hrl").

-export([start_link/0, stop/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    _ = start_deps(),

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
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop() -> 'ok'.
stop() ->
    cowboy:stop_listener('fax_file'),
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
    _ = [wh_util:ensure_started(App) || App <- ['sasl'
                                                ,'crypto'
                                                ,'inets'
                                                ,'ranch'
                                                ,'cowboy'
                                                ,'whistle_amqp'
                                               ]],
    'ok'.
