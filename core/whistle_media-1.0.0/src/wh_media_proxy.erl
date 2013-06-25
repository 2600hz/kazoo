%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012 VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_media_proxy).

-export([start_link/0
         ,stop/0
        ]).

-include("whistle_media.hrl").

-spec start_link() -> 'ignore'.
start_link() ->
    put('callid', ?LOG_SYSTEM_ID),

    _ = application:start('crypto'),
    _ = application:start('ranch'),
    _ = application:start('cowboy'),

    Dispatch = cowboy_router:compile([
                                      {'_', [{<<"/store/[...]">>, 'wh_media_store_proxy', []}
                                             ,{<<"/single/[...]">>, 'wh_media_single_proxy', []}
                                             ,{<<"/continuous/[...]">>, 'wh_media_continuous_proxy', []}
                                            ]}
                                     ]),
    maybe_start_plaintext(Dispatch),
    maybe_start_ssl(Dispatch),

    'ignore'.

-spec stop() -> 'ok'.
stop() ->
    _ = cowboy:stop_listener('wh_media_proxy'),
    _ = cowboy:stop_listener('media_mgr_ssl'),
    lager:debug("stopped wh_media_proxy listeners").

maybe_start_plaintext(Dispatch) ->
    case whapps_config:get_is_true(?CONFIG_CAT, <<"use_plaintext">>, 'true') of
        'false' -> lager:debug("plaintext media proxy support not enabled");
        'true' ->
            Port = whapps_config:get_integer(?CONFIG_CAT, <<"proxy_port">>, 24517),
            Listeners = whapps_config:get_integer(?CONFIG_CAT, <<"proxy_listeners">>, 25),

            cowboy:start_http('wh_media_proxy', Listeners
                              ,[{'port', Port}]
                              ,[{'env', [{'dispatch', Dispatch}]}]
                             ),
            lager:info("started media proxy on port ~p", [Port])
    end.

maybe_start_ssl(Dispatch) ->
    case whapps_config:get_is_true(?CONFIG_CAT, <<"use_ssl_proxy">>, 'false') of
        'false' -> lager:debug("ssl media proxy support not enabled");
        'true' ->
            RootDir = code:lib_dir('whistle_media'),

            SSLCert = whapps_config:get_string(?CONFIG_CAT
                                               ,<<"ssl_cert">>
                                               ,filename:join([RootDir, <<"priv/ssl/media_mgr.crt">>])
                                              ),
            SSLKey = whapps_config:get_string(?CONFIG_CAT
                                              ,<<"ssl_key">>
                                              ,filename:join([RootDir, <<"priv/ssl/media_mgr.key">>])
                                             ),

            SSLPort = whapps_config:get_integer(?CONFIG_CAT, <<"ssl_port">>, 24518),
            SSLPassword = whapps_config:get_string(?CONFIG_CAT, <<"ssl_password">>, <<>>),

            Listeners = whapps_config:get_integer(?CONFIG_CAT, <<"proxy_listeners">>, 25),

            try
                cowboy:start_https('media_mgr_ssl', Listeners
                                   ,[{'port', SSLPort}
                                     ,{'certfile', find_file(SSLCert, RootDir)}
                                     ,{'keyfile', find_file(SSLKey, RootDir)}
                                     ,{'password', SSLPassword}
                                    ]
                                   ,[{'env', [{'dispatch', Dispatch}]}
                                     ,{'onrequest', fun on_request/1}
                                     ,{'onresponse', fun on_response/3}
                                    ]
                                  ),
                lager:info("started ssl media proxy on port ~p", [SSLPort])
            catch
                'throw':{'invalid_file', _File} ->
                    lager:info("SSL disabled: failed to find ~s (tried prepending ~s too)", [_File, RootDir])
            end
    end.

-spec on_request(cowboy_req:req()) -> cowboy_req:req().
on_request(Req0) ->

    {Method, Req1} = cowboy_req:method(Req0),

    _ = wh_counter:inc(<<"media_proxy.requests.methods.", (wh_util:to_upper_binary(Method))/binary>>),
    Req1.

-spec on_response(cowboy_http:status(), cowboy_http:headers(), cowboy_req:req()) -> cowboy_req:req().
on_response(Status, _Headers, Req) ->
    wh_counter:inc(<<"media_proxy.responses.", (wh_util:to_binary(Status))/binary>>),
    Req.

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
