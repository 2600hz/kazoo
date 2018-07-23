%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_media_proxy).

-export([start_link/0
        ,stop/0
        ]).

-include("kazoo_media.hrl").

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID),

    Dispatch = cowboy_router:compile([{'_', [{<<"/store/[...]">>, [], 'kz_media_store_proxy', []}
                                            ,{<<"/single/[...]">>, [], 'kz_media_proxy_handler', ['single']}
                                            ,{<<"/continuous/[...]">>, [], 'kz_media_proxy_handler', ['continuous']}
                                            ]}
                                     ]),
    maybe_start_plaintext(Dispatch),
    maybe_start_ssl(Dispatch),

    'ignore'.

-spec stop() -> 'ok'.
stop() ->
    _ = cowboy:stop_listener(?MODULE),
    _ = cowboy:stop_listener('media_mgr_ssl'),
    lager:debug("stopped kz_media_proxy listeners").

maybe_start_plaintext(Dispatch) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"use_plaintext">>, 'true') of
        'false' -> lager:debug("plaintext media proxy support not enabled");
        'true' ->
            Port = kapps_config:get_integer(?CONFIG_CAT, <<"proxy_port">>, 24517),
            IP = get_binding_ip(),
            lager:info("trying to bind to address ~s port ~b", [inet:ntoa(IP), Port]),
            Listeners = kapps_config:get_integer(?CONFIG_CAT, <<"proxy_listeners">>, 25),

            {'ok', _Pid} = cowboy:start_clear(?MODULE
                                             ,[{'ip', IP}
                                              ,{'port', Port}
                                              ,{'num_acceptors', Listeners}
                                              ]
                                             ,#{'env' => #{'dispatch' => Dispatch}}
                                             ),
            lager:info("started media proxy(~p) on port ~p", [_Pid, Port])
    end.

maybe_start_ssl(Dispatch) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"use_ssl_proxy">>, 'false') of
        'false' -> lager:debug("ssl media proxy support not enabled");
        'true' ->
            RootDir = code:lib_dir('kazoo_media'),

            SSLCert = kapps_config:get_string(?CONFIG_CAT
                                             ,<<"ssl_cert">>
                                             ,filename:join([RootDir, <<"priv/ssl/media_mgr.crt">>])
                                             ),
            SSLKey = kapps_config:get_string(?CONFIG_CAT
                                            ,<<"ssl_key">>
                                            ,filename:join([RootDir, <<"priv/ssl/media_mgr.key">>])
                                            ),

            SSLPort = kapps_config:get_integer(?CONFIG_CAT, <<"ssl_port">>, 24518),
            SSLPassword = kapps_config:get_string(?CONFIG_CAT, <<"ssl_password">>, <<>>),

            Listeners = kapps_config:get_integer(?CONFIG_CAT, <<"proxy_listeners">>, 25),

            IP = get_binding_ip(),
            lager:info("trying to bind SSL API server to address ~s port ~b", [inet:ntoa(IP), SSLPort]),

            try
                {'ok', _Pid} = cowboy:start_tls('media_mgr_ssl'
                                               ,[{'ip', IP}
                                                ,{'port', SSLPort}
                                                ,{'num_acceptors', Listeners}
                                                ,{'certfile', find_file(SSLCert, RootDir)}
                                                ,{'keyfile', find_file(SSLKey, RootDir)}
                                                ,{'password', SSLPassword}
                                                ]
                                               ,#{'env' => #{'dispatch' => Dispatch}
                                                 ,'onrequest' => fun on_request/1
                                                 ,'onresponse' => fun on_response/3
                                                 }
                                               ),
                lager:info("started ssl media proxy(~p) on port ~p", [_Pid, SSLPort])
            catch
                'throw':{'invalid_file', _File} ->
                    lager:info("SSL disabled: failed to find ~s (tried prepending ~s too)", [_File, RootDir])
            end
    end.

-spec on_request(cowboy_req:req()) -> cowboy_req:req().
on_request(Req) -> Req.

-spec on_response(cowboy:http_status(), cowboy:http_headers(), cowboy_req:req()) -> cowboy_req:req().
on_response(_Status, _Headers, Req) -> Req.

-spec find_file(string(), string()) -> string().
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

-spec get_binding_ip() -> inet:ip_address().
get_binding_ip() ->
    IsIPv6Enabled = kz_network_utils:is_ip_family_supported('inet6'),
    IsIPv4Enabled = kz_network_utils:is_ip_family_supported('inet'),

    DefaultIP = kapps_controller:default_binding_all_ip(),

    IP = kapps_config:get_string(?CONFIG_CAT, <<"proxy_ip">>, DefaultIP),

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
