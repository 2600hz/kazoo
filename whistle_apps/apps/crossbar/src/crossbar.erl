%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%%
%%% @end
%%% Created :  19 Aug 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar).

-export([start_link/0, stop/0]).

-include("../include/crossbar.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    _ = start_deps(),

    %% maybe move this into a config file?
    Dispatch = [
                %% {Host, list({Path, Handler, Opts})}
                {'_', [{['v1', '...'], v1_resource, []}]}
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

            cowboy:start_listener(v1_resource, 100
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
    ok = application:stop(crossbar).

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
