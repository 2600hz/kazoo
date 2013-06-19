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

    Dispatch = cowboy_router:compile([
                                      %% {HostMatch, list({PathMatch, Handler, Opts})}
                                      {'_', [{<<"/v1/[...]">>, 'v1_resource', []}
                                             ,{'_', 'crossbar_default_handler', []}
                                            ]}
                                     ]),

    maybe_start_plaintext(Dispatch),
    maybe_start_ssl(Dispatch),

    crossbar_sup:start_link().

maybe_start_plaintext(Dispatch) ->
    case whapps_config:get_is_true(?CONFIG_CAT, <<"use_plaintext">>, 'true') of
        'false' -> lager:info("plaintext api support not enabled");
        'true' ->
            Port = whapps_config:get_integer(?CONFIG_CAT, <<"port">>, 8000),
            ReqTimeout = whapps_config:get_integer(?CONFIG_CAT, <<"request_timeout_ms">>, 10000),
            Workers = whapps_config:get_integer(?CONFIG_CAT, <<"workers">>, 100),

            %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
            try cowboy:start_http('v1_resource', Workers
                                  ,[{'port', Port}]
                                  ,[{'env', [{'dispatch', Dispatch}
                                             ,{'timeout', ReqTimeout}
                                            ]}
                                    ,{'onrequest', fun on_request/1}
                                    ,{'onresponse', fun on_response/4}
                                   ]
                                 ) of
                {'ok', _} ->
                    lager:info("started plaintext API server");
                _Err ->
                    lager:info("unexpected result when starting API server: ~p", [_Err])
            catch
                _E:_R ->
                    lager:info("crashed starting API server: ~s: ~p", [_E, _R])
            end
    end.

maybe_start_ssl(Dispatch) ->
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
                                     ,{'onrequest', fun on_request/1}
                                     ,{'onresponse', fun on_response/4}
                                    ]
                                  )
            of
                {'ok', _} ->
                    lager:info("started SSL API server on port ~b", [props:get_value('port', SSLOpts)]);
                _Err ->
                    lager:info("unexpected result when starting SSL API server: ~p", [_Err])
            catch
                'throw':{'invalid_file', _File} ->
                    lager:info("SSL disabled: failed to find ~s", [_File]);
                _E:_R ->
                    lager:info("crashed starting SSL API server: ~s: ~p", [_E, _R])
            end
    end.

ssl_opts(RootDir) ->
    BaseOpts = base_ssl_opts(RootDir),
    case whapps_config:get_string(?CONFIG_CAT, <<"ssl_ca_cert">>) of
        'undefined' -> BaseOpts;
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
    cowboy:stop_listener('v1_resource_ssl').

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
    _ = [wh_util:ensure_started(App) || App <- ['sasl', 'crypto', 'inets'
                                                ,'ranch' ,'cowboy', 'whistle_amqp'
                                               ]],
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Functions for onrequest and onresponse callbacks
%% @end
%%--------------------------------------------------------------------
-spec on_request(cowboy_req:req()) -> cowboy_req:req().
on_request(Req0) ->
    {Method, Req1} = cowboy_req:method(Req0),
    case Method of
        'OPTIONS' -> Req1;
        _ ->
            wh_counter:inc(<<"crossbar.requests.methods.", (wh_util:to_upper_binary(Method))/binary>>),
            Req1
    end.

-spec on_response(cowboy_http:status(), cowboy_http:headers(), text(), cowboy_req:req()) -> cowboy_req:req().
on_response(Status, _Headers, _Body, Req0) ->
    {Method, Req1} = cowboy_req:method(Req0),
    case Method of
        ?HTTP_OPTIONS -> Req1;
        'OPTIONS' -> Req1;
        _ ->
            wh_counter:inc(<<"crossbar.responses.", (wh_util:to_binary(Status))/binary>>),
            Req1
    end.
