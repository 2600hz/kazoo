%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(fax).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("fax/src/fax.hrl").

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
    Dispatch = [{'_', [{[<<"fax">>, '...'], fax_file_proxy, []}]}],
    Port = whapps_config:get_integer(?CONFIG_CAT, <<"port">>, 30950),
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener(fax_file, 50
                          ,cowboy_tcp_transport, [{port, Port}]
                          ,cowboy_http_protocol, [{dispatch, Dispatch}]
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
    cowboy:stop_listener(fax_file),
    application:stop(fax).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps() -> 'ok'.
start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    _ = [wh_util:ensure_started(App) || App <- [sasl, crypto, inets, cowboy, whistle_amqp]],
    ok.
