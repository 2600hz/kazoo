%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Jon Blanton
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(blackhole_init).

-export([start_link/0]).

-include("blackhole.hrl").

-define(USE_COMPRESSION, kapps_config:get_is_true(?CONFIG_CAT, <<"compress_response_body">>, 'true')).
-define(SOCKET_PORT, kapps_config:get_integer(?APP_NAME, <<"port">>, 5555)).
-define(SOCKET_ACCEPTORS, kapps_config:get_integer(?APP_NAME, <<"acceptors">>, 100)).

-spec blackhole_routes() -> cowboy_router:routes().
blackhole_routes() -> [{'_', paths_list()}].

paths_list() ->
    [api_path()].

api_path() ->
    {'_', [], 'blackhole_socket_handler', []}.

%%------------------------------------------------------------------------------
%% @doc Starts the application for inclusion in a supervisor tree.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),

    Dispatch = cowboy_router:compile(blackhole_routes()),

    DefaultIP = kz_network_utils:default_binding_ip(),
    IP = kapps_config:get_string(?CONFIG_CAT, <<"ip">>, DefaultIP),
    IPAddress = kz_network_utils:get_supported_binding_ip(IP, DefaultIP),

    maybe_start_plaintext(Dispatch, IPAddress),
    maybe_start_ssl(Dispatch, IPAddress),
    'ignore'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_start_plaintext(cowboy_router:dispatch_rules(), inet:ip_address()) -> 'ok'.
maybe_start_plaintext(Dispatch, IP) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"use_plaintext">>, 'true') of
        'false' -> lager:info("plaintext websocket support not enabled");
        'true' ->
            Port = ?SOCKET_PORT,
            ReqTimeout = kapps_config:get_integer(?CONFIG_CAT, <<"request_timeout_ms">>, 10 * ?MILLISECONDS_IN_SECOND),
            Workers = ?SOCKET_ACCEPTORS,

            %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
            try
                lager:info("trying to bind to address ~s port ~b", [inet:ntoa(IP), Port]),
                cowboy:start_clear('blackhole_socket_handler'
                                  ,#{'socket_opts' => [{'ip', IP}
                                                      ,{'port', Port}
                                                      ]
                                    ,'num_acceptors' => Workers
                                    }
                                  ,#{'env' => #{'dispatch' => Dispatch
                                               ,'timeout' => ReqTimeout
                                               }
                                    ,'stream_handlers' => maybe_add_compression_handler()
                                    }
                                  )
            of
                {'ok', _} ->
                    lager:info("started plaintext Websocket server");
                {'error', {'already_started', _P}} ->
                    lager:info("already started plaintext Websocket server at ~p", [_P])
            catch
                _E:_R ->
                    lager:warning("crashed starting WEBSOCKET server: ~s: ~p", [_E, _R])
            end
    end.

-spec maybe_start_ssl(cowboy_router:dispatch_rules(), inet:ip_address()) -> 'ok'.
maybe_start_ssl(Dispatch, IP) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"use_ssl">>, 'false') of
        'false' -> lager:info("ssl websocket support not enabled");
        'true' -> start_ssl(Dispatch, IP)
    end.

-spec start_ssl(cowboy_router:dispatch_rules(), inet:ip_address()) -> 'ok'.
start_ssl(Dispatch, IP) ->
    try ssl_opts(code:lib_dir(?APP)) of
        SSLOpts ->
            lager:debug("trying to start SSL WEBSOCKET server"),
            _SslStarted = ssl:start(),
            lager:debug("starting SSL : ~p", [_SslStarted]),
            ReqTimeout = kapps_config:get_integer(?CONFIG_CAT, <<"request_timeout_ms">>, 10 * ?MILLISECONDS_IN_SECOND),
            Workers = kapps_config:get_integer(?CONFIG_CAT, <<"ssl_workers">>, 100),

            try
                lager:info("trying to bind SSL WEBSOCKET server to address ~s port ~b"
                          ,[inet:ntoa(IP)
                           ,props:get_value('port', SSLOpts)
                           ]
                          ),
                cowboy:start_tls('blackhole_socket_handler_ssl'
                                ,#{'socket_opts' => [{'ip', IP} | SSLOpts]
                                  ,'num_acceptors' => Workers
                                  }
                                ,#{'env' => #{'dispatch' => Dispatch
                                             ,'timeout' => ReqTimeout
                                             }
                                  ,'stream_handlers' => maybe_add_compression_handler()
                                  }
                                )
            of
                {'ok', _} ->
                    lager:info("started SSL WEBSOCKET server on port ~b", [props:get_value('port', SSLOpts)]);
                {'error', {'already_started', _P}} ->
                    lager:info("already started SSL WEBSOCKET server on port ~b at ~p"
                              ,[props:get_value('port', SSLOpts), _P]
                              )
            catch
                'throw':{'invalid_file', _File} ->
                    lager:info("SSL disabled: failed to find ~s", [_File]);
                _E:_R ->
                    lager:warning("crashed starting SSL WEBSOCKET server: ~s: ~p", [_E, _R])
            end
    catch
        'throw':_E ->
            lager:warning("failed to start SSL WEBSOCKET server: ~p", [_E])
    end.

-spec ssl_opts(list()) -> kz_term:proplist().
ssl_opts(RootDir) ->
    BaseOpts = base_ssl_opts(RootDir),
    case kapps_config:get_string(?CONFIG_CAT, <<"ssl_ca_cert">>) of
        'undefined' -> BaseOpts;
        SSLCACert -> [{'cacertfile', SSLCACert} | BaseOpts]
    end.

-spec base_ssl_opts(list()) -> kz_term:proplist().
base_ssl_opts(RootDir) ->
    [{'port', kapps_config:get_integer(?CONFIG_CAT, <<"ssl_port">>, 5556)}
    ,{'certfile', find_file(kapps_config:get_string(?CONFIG_CAT
                                                   ,<<"ssl_cert">>
                                                   ,filename:join([RootDir, <<"priv/ssl/blackhole.crt">>])
                                                   ), RootDir)}
    ,{'keyfile', find_file(kapps_config:get_string(?CONFIG_CAT
                                                  ,<<"ssl_key">>
                                                  ,filename:join([RootDir, <<"priv/ssl/blackhole.key">>])
                                                  ), RootDir)}
    ,{'password', kapps_config:get_string(?CONFIG_CAT, <<"ssl_password">>, <<>>)}
    ].

-spec find_file(list(), list()) -> list().
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

-spec maybe_add_compression_handler() -> [atom()].
maybe_add_compression_handler() ->
    case ?USE_COMPRESSION of
        'true' -> ['cowboy_compress_h', 'cowboy_stream_h'];
        'false' -> ['cowboy_stream_h']
    end.
