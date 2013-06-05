%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%   Jon Blanton
%%%-------------------------------------------------------------------
-module(crossbar).

-export([start_link/0, stop/0
         ,start_mod/1, stop_mod/1
        ]).

-include("crossbar.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    put('callid', ?LOG_SYSTEM_ID),

    _ = start_deps(),

    %% maybe move this into a config file?
    %% {Host, list({Path, Handler, Opts})}
    Dispatch = [{'_', [{['v1', '...'], 'v1_resource', []}
                       ,{'_', 'crossbar_default_handler', []}
                      ]}
               ],

    Port = whapps_config:get_integer(?CONFIG_CAT, <<"port">>, 8000),
    ReqTimeout = whapps_config:get_integer(?CONFIG_CAT, <<"request_timeout_ms">>, 10000),
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener('v1_resource', 100
                          ,'cowboy_tcp_transport', [{'port', Port}]
                          ,'cowboy_http_protocol', [{'dispatch', Dispatch}
                                                    ,{'timeout', ReqTimeout}
                                                   ]
                         ),

    case whapps_config:get_is_true(?CONFIG_CAT, <<"use_ssl">>, 'false') of
        'false' -> lager:info("ssl api support not enabled");
        'true' ->
            lager:debug("trying to start SSL API server"),
            ssl:start(),
            ReqTimeout = whapps_config:get_integer(?CONFIG_CAT, <<"request_timeout_ms">>, 10000),
            Workers = whapps_config:get_integer(?CONFIG_CAT, <<"ssl_workers">>, 100),
            SSLOpts = ssl_opts(code:lib_dir('crossbar')),

            try cowboy:start_https('v1_resource_ssl', Workers
                                   ,SSLOpts
                                   ,[{'env', [{'dispatch', Dispatch}
                                              ,{'timeout', ReqTimeout}
                                             ]}
                                    ]
                                  )
            of
                {'ok', _} ->
                    lager:info("started SSL API server on port ~b", [props:get_value('port', SSLOpts)]);
                _Err ->
                    lager:info("unexpected result when starting SSL API server: ~p", [_Err])
            catch
                'throw':{'invalid_file', _File} ->
                    lager:info("SSL disabled: failed to find ~s (tried prepending ~s too)", [_File])
            end
    end,

    crossbar_sup:start_link().

ssl_opts(RootDir) ->
    BaseOpts = base_ssl_opts(RootDir),
        case whapps_config:get_string(?CONFIG_CAT, <<"ssl_ca_cert">>) of
        'undefined' ->
                BaseOpts;
                    SSLCACert -> [{'cacertfile', SSLCACert} | BaseOpts]
    end.

base_ssl_opts(RootDir) ->
    [{'port', whapps_config:get_integer(?CONFIG_CAT, <<"ssl_port">>, 8443)}
     ,{'certfile', find_file(whapps_config:get_string(?CONFIG_CAT
                                                      ,<<"ssl_cert">>
                                                      ,filename:join([RootDir, <<"priv/ssl/crossbar.crt">>])
                                                     ), RootDir)}
     ,{'keyfile', find_file(whapps_config:get_string(?CONFIG_CAT
                                                     ,<<"ssl_key">>
                                                     ,filename:join([RootDir, <<"priv/ssl/crossbar.key">>])
                                                    ), RootDir)}
     ,{'password', whapps_config:get_string(?CONFIG_CAT, <<"ssl_password">>, <<>>)}
    ].


find_file(File, Root) ->
    case filelib:is_file(File) of
        'true' -> File;
        'false' ->
            FromRoot = filename:join([Root, File]),
            lager:info("failed to find file at ~s, trying ~s", [File, FromRoot]),
            case filelib:is_file(FromRoot) of
                'true' -> FromRoot;
                'false' ->
                    lager:info("failed to find file at ~s", [FromRoot]),
                    throw({'invalid_file', File})
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop() -> 'ok'.
stop() ->
    cowboy:stop_listener('v1_resource'),
    'ok' = application:stop('crossbar').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Load a crossbar module's bindings into the bindings server
%% @end
%%--------------------------------------------------------------------
-spec start_mod(atom() | string() | binary()) -> any().
start_mod(CBMod) when not is_atom(CBMod) -> start_mod(wh_util:to_atom(CBMod, 'true'));
start_mod(CBMod) -> CBMod:init().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Load a crossbar module's bindings into the bindings server
%% @end
%%--------------------------------------------------------------------
-spec stop_mod(atom() | string() | binary()) -> any().

stop_mod(CBMod) when not is_atom(CBMod) -> stop_mod(wh_util:to_atom(CBMod, 'true'));
stop_mod(CBMod) ->
    crossbar_bindings:flush_mod(CBMod),
    case erlang:function_exported(CBMod, 'stop', 0) of
        'true' -> CBMod:stop();
        'false' -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps() -> 'ok'.
start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    _ = [ wh_util:ensure_started(App) || App <- ['sasl', 'crypto', 'inets', 'cowboy', 'whistle_amqp']],
    'ok'.
