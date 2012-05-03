%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(crossbar).

-export([start_link/0, stop/0
         ,start_mod/1, stop_mod/1
        ]).

-include("../include/crossbar.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    put(callid, ?LOG_SYSTEM_ID),

    _ = start_deps(),

    %% maybe move this into a config file?
    %% {Host, list({Path, Handler, Opts})}
    Dispatch = [{'_', [{['v1', '...'], v1_resource, []}
                       ,{'_', crossbar_default_handler, []}
                      ]}
               ],

    Port = whapps_config:get_integer(?CONFIG_CAT, <<"port">>, 8000),
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener(v1_resource, 100
                          ,cowboy_tcp_transport, [{port, Port}]
                          ,cowboy_http_protocol, [{dispatch, Dispatch}]
                         ),

    case whapps_config:get_is_true(?CONFIG_CAT, <<"ssl">>, false) of
        false -> ok;
        true ->
            SSLPort = whapps_config:get_integer(?CONFIG_CAT, <<"ssl_port">>, 8443),
            SSLCert = whapps_config:get_string(?CONFIG_CAT, <<"ssl_cert">>, "priv/ssl/cert.pem"),
            SSLKey = whapps_config:get_string(?CONFIG_CAT, <<"ssl_key">>, "priv/ssl/key.pem"),
            SSLPassword = whapps_config:get_string(?CONFIG_CAT, <<"ssl_password">>, "2600hz"),

            cowboy:start_listener(v1_resource_ssl, 100
                                  ,cowboy_ssl_transport, [
                                                          {port, SSLPort}
                                                          ,{certfile, SSLCert}
                                                          ,{keyfile, SSLKey}
                                                          ,{password, SSLPassword}
                                                         ]
                                  ,cowboy_http_protocol, [{dispatch, Dispatch}]
                                 )
    end,

    crossbar_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop/0 :: () -> 'ok'.
stop() ->
    cowboy:stop_listener(v1_resource),
    ok = application:stop(crossbar).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Load a crossbar module's bindings into the bindings server
%% @end
%%--------------------------------------------------------------------
-spec start_mod/1 :: (atom()) -> any().
start_mod(CBMod) ->
    CBMod:init().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Load a crossbar module's bindings into the bindings server
%% @end
%%--------------------------------------------------------------------
-spec stop_mod/1 :: (atom()) -> any().
stop_mod(CBMod) ->
    crossbar_bindings:flush_mod(CBMod),
    case erlang:function_exported(CBMod, stop, 0) of
        true -> CBMod:stop();
        false -> ok
    end.

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
