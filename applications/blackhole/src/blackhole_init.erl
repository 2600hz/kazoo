%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Jon Blanton
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
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID),

    Dispatch = cowboy_router:compile(blackhole_routes()),

    maybe_start_plaintext(Dispatch),
    maybe_start_ssl(Dispatch),
    'ignore'.

%%------------------------------------------------------------------------------
%% @doc Functions for onrequest and onresponse callbacks.
%% @end
%%------------------------------------------------------------------------------
-spec on_request(cowboy_req:req()) -> cowboy_req:req().
on_request(Req) -> Req.

-spec on_response(cowboy:http_status(), cowboy:http_headers(), kz_term:text(), cowboy_req:req()) ->
                         cowboy_req:req().
on_response(_Status, _Headers, _Body, Req) -> Req.

-spec maybe_start_plaintext(cowboy_router:dispatch_rules()) -> 'ok'.
maybe_start_plaintext(Dispatch) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"use_plaintext">>, 'true') of
        'false' -> lager:info("plaintext websocket support not enabled");
        'true' ->
            Port = ?SOCKET_PORT,
            ReqTimeout = kapps_config:get_integer(?CONFIG_CAT, <<"request_timeout_ms">>, 10 * ?MILLISECONDS_IN_SECOND),
            Workers = ?SOCKET_ACCEPTORS,

            %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
            try
                IP = get_binding_ip(),
                lager:info("trying to bind to address ~s port ~b", [inet:ntoa(IP), Port]),
                cowboy:start_clear('blackhole_socket_handler'
                                  ,[{'ip', IP}
                                   ,{'port', Port}
                                   ,{'num_acceptors', Workers}
                                   ]
                                  ,#{'env' => #{'dispatch' => Dispatch
                                               ,'timeout' => ReqTimeout
                                               }
                                    ,'onrequest' => fun on_request/1
                                    ,'onresponse' => fun on_response/4
                                    ,'compress' => ?USE_COMPRESSION
                                    }
                                  )
            of
                {'ok', _} ->
                    lager:info("started plaintext WebSocket server");
                {'error', {'already_started', _P}} ->
                    lager:info("already started plaintext WebSocket server at ~p", [_P])
            catch
                _E:_R ->
                    lager:warning("crashed starting WEBSOCKET server: ~s: ~p", [_E, _R])
            end
    end.

-spec get_binding_ip() -> inet:ip_address().
get_binding_ip() ->
    IsIPv6Enabled = kz_network_utils:is_ip_family_supported('inet6'),
    IsIPv4Enabled = kz_network_utils:is_ip_family_supported('inet'),

    DefaultIP = kapps_controller:default_binding_all_ip(),

    IP = kapps_config:get_string(?CONFIG_CAT, <<"ip">>, DefaultIP),

    {'ok', DefaultIPAddress} = inet:parse_address(DefaultIP),

    case inet:parse_ipv6strict_address(IP) of
        {'ok', IPv6} when IsIPv6Enabled -> IPv6;
        {'ok', _} ->
            lager:warning("address ~s is ipv6, but ipv6 is not supported by the system, enforcing default ip ~s"
                         ,[IP, inet:ntoa(DefaultIPAddress)]
                         ),
            DefaultIPAddress;
        {'error', 'einval'} ->
            case inet:parse_ipv4strict_address(IP) of
                {'ok', IPv4} when IsIPv4Enabled -> IPv4;
                {'ok', _} when IsIPv6Enabled ->
                    lager:warning("address ~s is ipv4, but ipv4 is not supported by the system, enforcing default ip ~s"
                                 ,[IP, inet:ntoa(DefaultIPAddress)]
                                 ),
                    DefaultIPAddress;
                {'ok', _} ->
                    lager:warning("address ~s is ipv4, but system reports that ipv4 and ipv6 are not supported by the system, enforcing default ip ~s"
                                 ,[IP, inet:ntoa(DefaultIPAddress)]
                                 ),
                    DefaultIPAddress;
                {'error', 'einval'} ->
                    lager:warning("address ~s is not a valid ipv6 or ipv4 address, enforcing default ip ~s"
                                 ,[IP, inet:ntoa(DefaultIPAddress)]
                                 ),
                    DefaultIPAddress
            end
    end.

-spec maybe_start_ssl(cowboy_router:dispatch_rules()) -> 'ok'.
maybe_start_ssl(Dispatch) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"use_ssl">>, 'false') of
        'false' -> lager:info("ssl websocket support not enabled");
        'true' -> start_ssl(Dispatch)
    end.

-spec start_ssl(cowboy_router:dispatch_rules()) -> 'ok'.
start_ssl(Dispatch) ->
    try ssl_opts(code:lib_dir(?APP)) of
        SSLOpts ->
            lager:debug("trying to start SSL WEBSOCKET server"),
            _SslStarted = ssl:start(),
            lager:debug("starting SSL : ~p", [_SslStarted]),
            ReqTimeout = kapps_config:get_integer(?CONFIG_CAT, <<"request_timeout_ms">>, 10 * ?MILLISECONDS_IN_SECOND),
            Workers = kapps_config:get_integer(?CONFIG_CAT, <<"ssl_workers">>, 100),

            try
                IP = get_binding_ip(),
                lager:info("trying to bind SSL WEBSOCKET server to address ~s port ~b"
                          ,[inet:ntoa(IP)
                           ,props:get_value('port', SSLOpts)
                           ]
                          ),
                cowboy:start_tls('blackhole_socket_handler_ssl'
                                ,[{'ip', IP}
                                 ,{'num_acceptors', Workers}
                                  | SSLOpts
                                 ]
                                ,#{'env' => #{'dispatch' => Dispatch
                                             ,'timeout' => ReqTimeout
                                             }
                                  ,'onrequest' => fun on_request/1
                                  ,'onresponse' => fun on_response/4
                                  ,'compress' => ?USE_COMPRESSION
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
