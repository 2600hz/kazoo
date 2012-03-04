%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012 VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(media_mgr).

-export([start/0, start_link/0, stop/0]).

-include("media.hrl").

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    put(callid, ?LOG_SYSTEM_ID),

    _ = start_deps(),

    Dispatch = [
                {'_', [{['single', '...'], media_single, []}
                       ,{['continuous','...'], media_continuous, []}
                      ]}
                ],

    Port = whapps_config:get_integer(?CONFIG_CAT, <<"port">>, 1234),
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener(media_mgr, 2
                          ,cowboy_tcp_transport, [{port, Port}]
                          ,media_shout_protocol, [{dispatch, Dispatch}]
                         ),

    media_mgr_sup:start_link().

%% @spec start() -> ok
%% @doc Start the app
start() ->
    start_deps(),
    application:start(media_mgr).

%% @spec stop() -> ok
%% @doc Stop the basicapp server.
stop() ->
    application:stop(media_mgr).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps/0 :: () -> 'ok'.
start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    _ = [ wh_util:ensure_started(App) || App <- [sasl, crypto, inets, cowboy, whistle_amqp]],
    ok.
