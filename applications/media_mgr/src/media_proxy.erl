%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(media_proxy).

-export([start_link/0
        ,stop/0
        ]).

-include("media.hrl").

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),

    Dispatch = cowboy_router:compile([{'_', [{<<"/store/[...]">>, [], 'media_store_proxy', []}
                                            ,{<<"/single/[...]">>, [], 'media_proxy_handler', ['single']}
                                            ,{<<"/continuous/[...]">>, [], 'media_proxy_handler', ['continuous']}
                                            ]}
                                     ]),

    DefaultIP = kz_network_utils:default_binding_ip(),
    IP = kapps_config:get_string(?CONFIG_CAT, <<"proxy_ip">>, DefaultIP),
    IPAddress = kz_network_utils:get_supported_binding_ip(IP, DefaultIP),

    maybe_start_plaintext(Dispatch, IPAddress),
    maybe_start_ssl(Dispatch, IPAddress),

    'ignore'.

-spec stop() -> 'ok'.
stop() ->
    _ = cowboy:stop_listener(?MODULE),
    _ = cowboy:stop_listener('media_mgr_ssl'),
    lager:debug("stopped kz_media_proxy listeners").

-spec maybe_start_plaintext(cowboy_router:dispatch_rules(), inet:ip_address()) -> 'ok'.
maybe_start_plaintext(Dispatch, IP) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"use_plaintext">>, 'true') of
        'false' -> lager:debug("plaintext media proxy support not enabled");
        'true' ->
            Port = kapps_config:get_integer(?CONFIG_CAT, <<"proxy_port">>, 24517),
            lager:info("trying to bind to address ~s port ~b", [inet:ntoa(IP), Port]),
            Listeners = kapps_config:get_integer(?CONFIG_CAT, <<"proxy_listeners">>, 25),

            {'ok', _Pid} = cowboy:start_clear(?MODULE
                                             ,#{'socket_opts' => [{'ip', IP}
                                                                 ,{'port', Port}
                                                                 ]
                                               ,'num_acceptors' => Listeners
                                               }
                                             ,#{'env' => #{'dispatch' => Dispatch}}
                                             ),
            lager:info("started media proxy(~p) on port ~p", [_Pid, Port])
    end.

-spec maybe_start_ssl(cowboy_router:dispatch_rules(), inet:ip_address()) -> 'ok'.
maybe_start_ssl(Dispatch, IP) ->
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

            lager:info("trying to bind SSL API server to address ~s port ~b", [inet:ntoa(IP), SSLPort]),

            try
                {'ok', _Pid} = cowboy:start_tls('media_mgr_ssl'
                                               ,#{'socket_opts' => [{'ip', IP}
                                                                   ,{'port', SSLPort}
                                                                   ,{'certfile', find_file(SSLCert, RootDir)}
                                                                   ,{'keyfile', find_file(SSLKey, RootDir)}
                                                                   ,{'password', SSLPassword}
                                                                   ]
                                                 ,'num_acceptors' => Listeners
                                                 }
                                               ,#{'env' => #{'dispatch' => Dispatch}
                                                 }
                                               ),
                lager:info("started ssl media proxy(~p) on port ~p", [_Pid, SSLPort])
            catch
                'throw':{'invalid_file', _File} ->
                    lager:info("SSL disabled: failed to find ~s (tried prepending ~s too)", [_File, RootDir])
            end
    end.

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
