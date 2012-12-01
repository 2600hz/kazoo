%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012 VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_media_proxy).

-export([start_link/0]).

-include("whistle_media.hrl").

start_link() ->
    put(callid, ?LOG_SYSTEM_ID),
    Dispatch = [
                {'_', [{[<<"store">>,'...'], wh_media_store_proxy, []}
                       ,{[<<"single">>, '...'], wh_media_single_proxy, []}
                       ,{[<<"continuous">>,'...'], wh_media_continuous_proxy, []}
                      ]}
               ],

    Port = whapps_config:get_integer(?CONFIG_CAT, <<"proxy_port">>, 24517),
    Listeners = whapps_config:get_integer(?CONFIG_CAT, <<"proxy_listeners">>, 25),
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener(wh_media_proxy, Listeners
                          ,cowboy_tcp_transport, [{port, Port}]
                          ,cowboy_http_protocol, [{dispatch, Dispatch}]
                         ),

    case whapps_config:get_is_true(?CONFIG_CAT, <<"use_ssl_proxy">>, false) of
        false -> ok;
        true ->
            RootDir = code:lib_dir(whistle_media),

            try
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

                cowboy:start_listener(media_mgr_ssl, 10
                                      ,cowboy_ssl_transport, [{port, SSLPort}
                                                              ,{certfile, find_file(SSLCert, RootDir)}
                                                              ,{keyfile, find_file(SSLKey, RootDir)}
                                                              ,{password, SSLPassword}
                                                             ]
                                      ,cowboy_http_protocol, [{dispatch, Dispatch}
                                                              ,{onrequest, fun on_request/1}
                                                              ,{onresponse, fun on_response/3}
                                                             ]
                                     ),
                ok
            catch
                throw:{invalid_file, _File} ->
                    lager:info("SSL disabled: failed to find ~s (tried prepending ~s too)", [_File, RootDir])
            end
    end,
    lager:debug("started media proxy on port ~p", [Port]),
    ignore.

-spec on_request/1 :: (#http_req{}) -> #http_req{}.
on_request(Req0) ->

    {Method, Req1} = cowboy_http_req:method(Req0),



    _ = wh_counter:inc(<<"media_proxy.requests.methods.", (wh_util:to_upper_binary(Method))/binary>>),
    Req1.

-spec on_response/3 :: (cowboy_http:status(), cowboy_http:headers(), #http_req{}) -> #http_req{}.
on_response(Status, _Headers, Req) ->
    wh_counter:inc(<<"media_proxy.responses.", (wh_util:to_binary(Status))/binary>>),
    Req.

-spec find_file/2 :: (string(), string()) -> string().    
find_file(File, Root) ->
    case filelib:is_file(File) of
        true -> File;
        false ->
            FromRoot = filename:join([Root, File]),
            lager:info("failed to find file at ~s, trying ~s", [File, FromRoot]),
            case filelib:is_file(FromRoot) of
                true -> FromRoot;
                false ->
                    lager:info("failed to find file at ~s", [FromRoot]),
                    throw({invalid_file, File})
            end
    end.
