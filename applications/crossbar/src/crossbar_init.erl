%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%   Jon Blanton
%%%-------------------------------------------------------------------
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
    {'version', 'function', fun api_version_constraint/1}.

-spec api_version_constraint(ne_binary()) -> boolean().
api_version_constraint(<<"v", ApiVersion/binary>>) ->
    try kz_util:to_integer(ApiVersion) of
        _Int -> lager:debug("routing to version ~b", [_Int]), 'true'
    catch
        _:_ -> lager:debug("not routing to version ~s", [ApiVersion]), 'false'
    end;
api_version_constraint(NotVersion) ->
    lists:member(NotVersion, ?INBOUND_HOOKS).

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the app for inclusion in a supervisor tree
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    kz_util:put_callid(?LOG_SYSTEM_ID),
    _ = declare_exchanges(),

    Dispatch = cowboy_router:compile(crossbar_routes()),

    maybe_start_plaintext(Dispatch),
    maybe_start_ssl(Dispatch),
    'ignore'.

%%--------------------------------------------------------------------
%% @public
%% @doc Load a crossbar module's bindings into the bindings server
%%--------------------------------------------------------------------

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
        'false' -> start_mod(kz_util:to_atom(CBMod, 'true'))
    end;
start_mod(CBMod) when is_atom(CBMod) ->
    try CBMod:init() of
        _ -> 'ok'
    catch
        _E:_R ->
            lager:notice("failed to initialize ~s: ~p (trying other versions)", [CBMod, _R]),
            maybe_start_mod_versions(?VERSION_SUPPORTED, CBMod)
    end;
start_mod(CBMod) ->
    start_mod(kz_util:to_binary(CBMod)).

-spec maybe_start_mod_versions(ne_binaries(), ne_binary() | atom()) -> 'ok'.
maybe_start_mod_versions(Versions, Mod) ->
    case lists:all(fun(Version) -> start_mod_version(Version, Mod) end, Versions) of
        'true' -> 'ok';
        'false' -> {'error', 'no_modules_started'}
    end.

-spec start_mod_version(ne_binary(), ne_binary() | atom()) -> boolean().
start_mod_version(Version, Mod) ->
    Module = <<(kz_util:to_binary(Mod))/binary
               , "_", (kz_util:to_binary(Version))/binary
             >>,
    CBMod = kz_util:to_atom(Module, 'true'),
    lager:notice("starting module ~s version ~s", [Mod, Version]),
    try CBMod:init() of
        _ ->
            lager:notice("module ~s version ~s successfully loaded", [Mod, Version]),
            'true'
    catch
        _E:_R ->
            lager:warning("failed to initialize module ~s version ~s: ~p", [Mod, Version, _R]),
            'false'
    end.
%%--------------------------------------------------------------------
%% @public
%% @doc Load a crossbar module's bindings into the bindings server
%%--------------------------------------------------------------------
-spec stop_mod(atom() | string() | binary()) -> any().
stop_mod(CBMod) when not is_atom(CBMod) -> stop_mod(kz_util:to_atom(CBMod, 'true'));
stop_mod(CBMod) ->
    crossbar_bindings:flush_mod(CBMod),
    case erlang:function_exported(CBMod, 'stop', 0) of
        'true' -> do_stop_mod(CBMod);
        'false' ->
            lager:notice("failed to stop ~s (trying other versions)", [CBMod]),
            maybe_stop_mod_versions(?VERSION_SUPPORTED, CBMod)
    end.

-spec do_stop_mod(atom()) -> any().
do_stop_mod(CBMod) ->
    try CBMod:stop() of
        _ -> 'ok'
    catch
        _E:_R ->
            lager:notice("failed to stop ~s: ~p (trying other versions)", [CBMod, _R]),
            maybe_stop_mod_versions(?VERSION_SUPPORTED, CBMod)
    end.

-spec maybe_stop_mod_versions(ne_binaries(), ne_binary() | atom()) -> 'ok'.
maybe_stop_mod_versions(Versions, Mod) ->
    lists:foreach(fun(Version) -> stop_mod_version(Version, Mod) end, Versions).

-spec stop_mod_version(ne_binary(), ne_binary() | atom()) -> boolean().
stop_mod_version(Version, Mod) ->
    Module = <<(kz_util:to_binary(Mod))/binary
               , "_", (kz_util:to_binary(Version))/binary
             >>,
    CBMod = kz_util:to_atom(Module, 'true'),
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

%%--------------------------------------------------------------------
%% @private
%% @doc Ensures that all exchanges used are declared
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_acdc_agent:declare_exchanges(),
    _ = kapi_acdc_stats:declare_exchanges(),
    _ = kapi_money:declare_exchanges(),
    _ = kapi_notifications:declare_exchanges(),
    _ = kapi_presence:declare_exchanges(),
    _ = kapi_registration:declare_exchanges(),
    _ = kapi_resource:declare_exchanges(),
    _ = kapi_switch:declare_exchanges(),
    _ = kapi_sysconf:declare_exchanges(),
    _ = kapi_call:declare_exchanges(),
    _ = kapi_dialplan:declare_exchanges(),
    kapi_self:declare_exchanges().

%%--------------------------------------------------------------------
%% @private
%% @doc Functions for onrequest and onresponse callbacks
%%--------------------------------------------------------------------
-spec on_request(cowboy_req:req()) -> cowboy_req:req().
on_request(Req0) ->
    {Method, Req1} = cowboy_req:method(Req0),
    case Method of
        ?HTTP_OPTIONS -> Req1;
        _ -> Req1
    end.

-spec on_response(cowboy:http_status(), cowboy:http_headers(), text(), cowboy_req:req()) ->
                         cowboy_req:req().
on_response(_Status, _Headers, _Body, Req0) ->
    {Method, Req1} = cowboy_req:method(Req0),
    case Method of
        ?HTTP_OPTIONS -> Req1;
        _ -> Req1
    end.

-spec maybe_start_plaintext(cowboy_router:dispatch_rules()) -> 'ok'.
maybe_start_plaintext(Dispatch) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"use_plaintext">>, 'true') of
        'false' -> lager:info("plaintext api support not enabled");
        'true' ->
            Port = kapps_config:get_integer(?CONFIG_CAT, <<"port">>, 8000),
            ReqTimeout = kapps_config:get_integer(?CONFIG_CAT, <<"request_timeout_ms">>, 10 * ?MILLISECONDS_IN_SECOND),
            Workers = kapps_config:get_integer(?CONFIG_CAT, <<"workers">>, 100),

            %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
            try cowboy:start_http('api_resource', Workers
                                 ,[{'ip', {0,0,0,0,0,0,0,0}}
                                  ,{'port', Port}
                                  ]
                                 ,[{'env', [{'dispatch', Dispatch}
                                           ,{'timeout', ReqTimeout}
                                           ]}
                                  ,{'onrequest', fun on_request/1}
                                  ,{'onresponse', fun on_response/4}
                                  ,{'compress', ?USE_COMPRESSION}
                                  ]
                                 ) of
                {'ok', _} ->
                    lager:info("started plaintext API server");
                {'error', {'already_started', _P}} ->
                    lager:info("already started plaintext API server at ~p", [_P])
            catch
                _E:_R ->
                    lager:warning("crashed starting API server: ~s: ~p", [_E, _R])
            end
    end.

-spec maybe_start_ssl(cowboy_router:dispatch_rules()) -> 'ok'.
maybe_start_ssl(Dispatch) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"use_ssl">>, 'false') of
        'false' -> lager:info("ssl api support not enabled");
        'true' -> start_ssl(Dispatch)
    end.

-spec start_ssl(cowboy_router:dispatch_rules()) -> 'ok'.
start_ssl(Dispatch) ->
    try ssl_opts(code:lib_dir('crossbar')) of
        SSLOpts ->
            lager:debug("trying to start SSL API server"),
            _SslStarted = ssl:start(),
            lager:debug("starting SSL : ~p", [_SslStarted]),
            ReqTimeout = kapps_config:get_integer(?CONFIG_CAT, <<"request_timeout_ms">>, 10 * ?MILLISECONDS_IN_SECOND),
            Workers = kapps_config:get_integer(?CONFIG_CAT, <<"ssl_workers">>, 100),

            try cowboy:start_https('api_resource_ssl', Workers
                                  ,SSLOpts
                                  ,[{'env', [{'dispatch', Dispatch}
                                            ,{'timeout', ReqTimeout}
                                            ]}
                                   ,{'onrequest', fun on_request/1}
                                   ,{'onresponse', fun on_response/4}
                                   ,{'compress', ?USE_COMPRESSION}
                                   ]
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

-spec ssl_opts(list()) -> kz_proplist().
ssl_opts(RootDir) ->
    BaseOpts = base_ssl_opts(RootDir),
    case kapps_config:get_string(?CONFIG_CAT, <<"ssl_ca_cert">>) of
        'undefined' -> BaseOpts;
        SSLCACert -> [{'cacertfile', SSLCACert} | BaseOpts]
    end.

-spec base_ssl_opts(list()) -> kz_proplist().
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
