%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012 VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(media_mgr).

-export([start/0, start_link/0, stop/0]).

-include("media.hrl").

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    put(callid, ?LOG_SYSTEM_ID),

    _ = start_deps(),

    Dispatch = [
                {'_', [{['single', '...'], media_single, []}
                       ,{['continuous','...'], media_continuous, []}
                      ]}
               ],

    Port = whapps_config:get_integer(?CONFIG_CAT, <<"port">>, 24517),
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener(media_mgr, 50
                          ,cowboy_tcp_transport, [{port, Port}]
                          ,cowboy_http_protocol, [{dispatch, Dispatch}]
                         ),

    case whapps_config:get_is_true(?CONFIG_CAT, <<"use_ssl">>, false) of
        false -> ok;
        true ->
            RootDir = code:lib_dir(media_mgr),

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
                                      ,cowboy_ssl_transport, [
                                                              {port, SSLPort}
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

    media_mgr_sup:start_link().

%% @spec start() -> ok
%% @doc Start the app
start() ->
    start_deps(),
    application:start(media_mgr).

%% @spec stop() -> ok
%% @doc Stop the basicapp server.
stop() ->
    application:stop(media_mgr).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps/0 :: () -> 'ok'.
start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    _ = [ wh_util:ensure_started(App) || App <- [sasl, crypto, inets, cowboy, whistle_amqp]],
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Functions for onrequest and onresponse callbacks
%% @end
%%--------------------------------------------------------------------
-spec on_request/1 :: (#http_req{}) -> #http_req{}.
on_request(Req0) ->
    {Method, Req1} = cowboy_http_req:method(Req0),
    _ = wh_counter:inc(<<"media_mgr.requests.methods.", (wh_util:to_upper_binary(Method))/binary>>),
    Req1.

-spec on_response/3 :: (cowboy_http:status(), cowboy_http:headers(), #http_req{}) -> #http_req{}.
on_response(Status, _Headers, Req) ->
    wh_counter:inc(<<"media_mgr.responses.", (wh_util:to_binary(Status))/binary>>),
    Req.
    
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
