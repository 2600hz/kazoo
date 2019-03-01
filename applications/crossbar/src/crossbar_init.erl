%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Jon Blanton
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_init).

-export([start_link/0
        ,start_mod/1, stop_mod/1
        ]).

-include("crossbar.hrl").

-define(USE_COMPRESSION, kapps_config:get_is_true(?CONFIG_CAT, <<"compress_response_body">>, 'true')).

-spec crossbar_routes() -> cowboy_router:routes().
crossbar_routes() -> [{'_', paths_list()}].

paths_list() ->
    [api_path(), default_path()].

default_path() ->
    {'_', 'crossbar_default_handler', []}.

api_path() ->
    {<<"/:version/[...]">>, [api_version_constraint()], 'api_resource', []}.

-spec api_version_constraint() -> cowboy_router:constraint().
api_version_constraint() ->
    {'version', fun api_version_constraint/2}.

-spec api_version_constraint('forward', kz_term:ne_binary()) ->
                                    {'ok', kz_term:ne_binary()} |
                                    {'error', 'not_a_version'}.
api_version_constraint('forward', <<"v", ApiVersion/binary>>=Vsn) ->
    try kz_term:to_integer(ApiVersion) of
        Int ->
            lager:debug("routing to version ~b", [Int]),
            {'ok', Vsn}
    catch
        _:_ ->
            lager:debug("not routing to version ~s", [ApiVersion]),
            {'error', 'not_a_version'}
    end;
api_version_constraint('forward', NotVersion) ->
    case lists:member(NotVersion, ?INBOUND_HOOKS) of
        'true' -> {'ok', NotVersion};
        'false' -> {'error', 'not_a_version'}
    end.

%%------------------------------------------------------------------------------
%% @doc Starts the application for inclusion in a supervisor tree.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    Dispatch = cowboy_router:compile(crossbar_routes()),

    DefaultIP = kz_network_utils:default_binding_ip(),
    IP = kapps_config:get_ne_binary(?CONFIG_CAT, <<"ip">>, DefaultIP),
    IPAddress = kz_network_utils:get_supported_binding_ip(IP, DefaultIP),

    maybe_start_plaintext(Dispatch, IPAddress),
    maybe_start_ssl(Dispatch, IPAddress),
    'ignore'.

%%------------------------------------------------------------------------------
%% @doc Load a crossbar module's bindings into the bindings server.
%% @end
%%------------------------------------------------------------------------------
-spec is_versioned_module(binary()) -> boolean().
is_versioned_module(Module) ->
    Mod = lists:reverse(binary_to_list(Module)),
    case Mod of
        "1v_" ++ _ -> 'true';
        "2v_" ++ _ -> 'true';
        _ -> 'false'
    end.

-spec start_mod(atom() | string() | binary()) -> 'ok' | {'error', any()}.
start_mod(CBMod) when is_binary(CBMod) ->
    case is_versioned_module(CBMod) of
        'true' -> {'error', 'version_supplied'};
        'false' -> start_mod(kz_term:to_atom(CBMod, 'true'))
    end;
start_mod(CBMod) when is_atom(CBMod) ->
    try CBMod:init() of
        _ -> 'ok'
    catch
        _E:_R ->
            lager:debug("failed to initialize ~s: ~p (trying other versions)", [CBMod, _R]),
            maybe_start_mod_versions(?VERSION_SUPPORTED, CBMod)
    end;
start_mod(CBMod) ->
    start_mod(kz_term:to_binary(CBMod)).

-spec maybe_start_mod_versions(kz_term:ne_binaries(), kz_term:ne_binary() | atom()) -> 'ok'.
maybe_start_mod_versions(Versions, Mod) ->
    case lists:all(fun(Version) -> start_mod_version(Version, Mod) end, Versions) of
        'true' -> 'ok';
        'false' -> {'error', 'no_modules_started'}
    end.

-spec start_mod_version(kz_term:ne_binary(), kz_term:ne_binary() | atom()) -> boolean().
start_mod_version(Version, Mod) ->
    Module = list_to_binary([kz_term:to_binary(Mod), "_", kz_term:to_binary(Version)]),
    CBMod = kz_term:to_atom(Module, 'true'),
    try CBMod:init() of
        _ ->
            lager:debug("module ~s version ~s successfully loaded", [Mod, Version]),
            'true'
    catch
        _E:_R ->
            lager:warning("failed to initialize module ~s version ~s: ~p", [Mod, Version, _R]),
            'false'
    end.
%%------------------------------------------------------------------------------
%% @doc Load a crossbar module's bindings into the bindings server.
%% @end
%%------------------------------------------------------------------------------
-spec stop_mod(atom() | string() | binary()) -> 'ok'.
stop_mod(CBMod) when not is_atom(CBMod) ->
    stop_mod(kz_term:to_atom(CBMod, 'true'));
stop_mod(CBMod) ->
    crossbar_bindings:flush_mod(CBMod),
    case erlang:function_exported(CBMod, 'stop', 0) of
        'true' -> do_stop_mod(CBMod);
        'false' ->
            lager:debug("failed to stop ~s (trying other versions)", [CBMod]),
            maybe_stop_mod_versions(?VERSION_SUPPORTED, CBMod)
    end.

-spec do_stop_mod(atom()) -> 'ok'.
do_stop_mod(CBMod) ->
    try CBMod:stop() of
        _ -> 'ok'
    catch
        _E:_R ->
            lager:notice("failed to stop ~s: ~p (trying other versions)", [CBMod, _R]),
            maybe_stop_mod_versions(?VERSION_SUPPORTED, CBMod)
    end.

-spec maybe_stop_mod_versions(kz_term:ne_binaries(), kz_term:ne_binary() | atom()) -> 'ok'.
maybe_stop_mod_versions(Versions, Mod) ->
    lists:foreach(fun(Version) -> stop_mod_version(Version, Mod) end, Versions).

-spec stop_mod_version(kz_term:ne_binary(), kz_term:ne_binary() | atom()) -> boolean().
stop_mod_version(Version, Mod) ->
    Module = list_to_binary([kz_term:to_binary(Mod), "_", kz_term:to_binary(Version)]),
    CBMod = kz_term:to_atom(Module, 'true'),
    crossbar_bindings:flush_mod(CBMod),
    try CBMod:stop() of
        _ ->
            lager:notice("module ~s version ~s successfully stopped", [Mod, Version]),
            'true'
    catch
        _E:_R ->
            lager:warning("failed to stop module ~s version ~s: ~p", [Mod, Version, _R]),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_start_plaintext(cowboy_router:dispatch_rules(), inet:ip_address()) -> 'ok'.
maybe_start_plaintext(Dispatch, IP) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"use_plaintext">>, 'true') of
        'false' -> lager:info("plaintext api support not enabled");
        'true' ->
            Port = kapps_config:get_integer(?CONFIG_CAT, <<"port">>, 8000),
            ReqTimeout = kapps_config:get_integer(?CONFIG_CAT, <<"request_timeout_ms">>, 10 * ?MILLISECONDS_IN_SECOND),
            IdleTimeout = kapps_config:get_integer(?CONFIG_CAT, <<"idle_timeout_ms">>, 120 * ?MILLISECONDS_IN_SECOND),
            Workers = kapps_config:get_integer(?CONFIG_CAT, <<"workers">>, 100),

            %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
            try
                lager:info("trying to bind to address ~s port ~b", [inet:ntoa(IP), Port]),
                cowboy:start_clear('api_resource'
                                  ,[{'ip', IP}
                                   ,{'port', Port}
                                   ,{'num_acceptors', Workers}
                                   ]
                                  ,#{'env' => #{'dispatch' => Dispatch
                                               }
                                    ,'stream_handlers' => maybe_add_compression_handler()
                                    ,'shutdown_timeout' => ReqTimeout
                                    ,'idle_timeout' => IdleTimeout
                                    }
                                  )
            of
                {'ok', _} ->
                    lager:info("started plaintext API server");
                {'error', {'already_started', _P}} ->
                    lager:info("already started plaintext API server at ~p", [_P])
            catch
                _E:_R ->
                    lager:warning("crashed starting API server: ~s: ~p", [_E, _R])
            end
    end.

-spec maybe_start_ssl(cowboy_router:dispatch_rules(), inet:ip_address()) -> 'ok'.
maybe_start_ssl(Dispatch, IP) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"use_ssl">>, 'false') of
        'false' -> lager:info("ssl api support not enabled");
        'true' -> start_ssl(Dispatch, IP)
    end.

-spec start_ssl(cowboy_router:dispatch_rules(), inet:ip_address()) -> 'ok'.
start_ssl(Dispatch, IP) ->
    try ssl_opts(code:lib_dir(?APP)) of
        SSLOpts ->
            lager:debug("trying to start SSL API server"),
            _SslStarted = ssl:start(),
            lager:debug("starting SSL : ~p", [_SslStarted]),
            ReqTimeout = kapps_config:get_integer(?CONFIG_CAT, <<"request_timeout_ms">>, 10 * ?MILLISECONDS_IN_SECOND),
            Workers = kapps_config:get_integer(?CONFIG_CAT, <<"ssl_workers">>, 100),

            try
                lager:info("trying to bind SSL API server to address ~s port ~b"
                          ,[inet:ntoa(IP)
                           ,props:get_value('port', SSLOpts)
                           ]
                          ),
                cowboy:start_tls('api_resource_ssl'
                                ,[{'ip', IP}
                                 ,{'num_acceptors', Workers}
                                  | SSLOpts
                                 ]
                                ,#{'env' => #{'dispatch' => Dispatch
                                             }
                                  ,'stream_handlers' => maybe_add_compression_handler()
                                  ,'shutdown_timeout' => ReqTimeout
                                  ,'idle_timeout' => 120 * ?MILLISECONDS_IN_SECOND
                                  }
                                )
            of
                {'ok', _} ->
                    lager:info("started SSL API server on port ~b", [props:get_value('port', SSLOpts)]);
                {'error', {'already_started', _P}} ->
                    lager:info("already started SSL API server on port ~b at ~p"
                              ,[props:get_value('port', SSLOpts), _P]
                              )
            catch
                'throw':{'invalid_file', _File} ->
                    lager:info("SSL disabled: failed to find ~s", [_File]);
                _E:_R ->
                    lager:warning("crashed starting SSL API server: ~s: ~p", [_E, _R])
            end
    catch
        'throw':_E ->
            lager:warning("failed to start SSL API server: ~p", [_E])
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
    [{'port', kapps_config:get_integer(?CONFIG_CAT, <<"ssl_port">>, 8443)}
    ,{'certfile', find_file(kapps_config:get_string(?CONFIG_CAT
                                                   ,<<"ssl_cert">>
                                                   ,filename:join([RootDir, <<"priv/ssl/crossbar.crt">>])
                                                   ), RootDir)}
    ,{'keyfile', find_file(kapps_config:get_string(?CONFIG_CAT
                                                  ,<<"ssl_key">>
                                                  ,filename:join([RootDir, <<"priv/ssl/crossbar.key">>])
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
