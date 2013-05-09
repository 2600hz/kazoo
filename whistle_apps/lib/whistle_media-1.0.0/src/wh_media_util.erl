%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_media_util).

-export([base_url/1, base_url/2, base_url/3]).
-export([convert_stream_type/1]).

-include("whistle_media.hrl").

-define(USE_HTTPS, whapps_config:get_is_true(?CONFIG_CAT, <<"use_https">>, 'false')).
-define(AUTH_PLAYBACK, whapps_config:get_is_true(?CONFIG_CAT, <<"authenticated_playback">>, 'false')).
-define(AUTH_USERNAME, whapps_config:get_string(?CONFIG_CAT, <<"proxy_username">>, wh_util:rand_hex_binary(8))).
-define(AUTH_PASSWORD, whapps_config:get_string(?CONFIG_CAT, <<"proxy_password">>, wh_util:rand_hex_binary(8))).
-define(USE_AUTH_STORE, whapps_config:get_is_true(?CONFIG_CAT, <<"authenticated_store">>, 'true')).

base_url(Host) ->
    Port = wh_couch_connections:get_port(),
    base_url(Host, Port).

base_url(Host, Port) ->
    base_url(Host, Port, 'proxy_playback').

base_url(Host, Port, 'direct_playback') ->
    case ?AUTH_PLAYBACK of
        'false' -> build_url(Host, Port, [], []);
        'true' ->
            {Username, Password} = wh_couch_connections:get_creds(),
            build_url(Host, Port, Username, Password)
    end;
base_url(Host, Port, 'proxy_playback') ->
    case ?AUTH_PLAYBACK of
        'false' -> build_url(Host, Port, [], []);
        'true' -> build_url(Host, Port, ?AUTH_USERNAME, ?AUTH_PASSWORD)
    end;
base_url(Host, Port, 'direct_store') ->
    case ?USE_AUTH_STORE of
        'false' -> build_url(Host, Port, [], []);
        'true' ->
            {Username, Password} = wh_couch_connections:get_creds(),
            build_url(Host, Port, Username, Password)
    end;
base_url(Host, Port, 'proxy_store') ->
    case ?USE_AUTH_STORE of
        'false' -> build_url(Host, Port, [], []);
        'true' -> build_url(Host, Port, ?AUTH_USERNAME, ?AUTH_PASSWORD)
    end.

build_url(H, P, [], []) ->
    Scheme = case ?USE_HTTPS of
                 'true' -> <<"https">>;
                 'false' -> <<"http">>
             end,
    list_to_binary([Scheme, "://", wh_util:to_binary(H), ":", wh_util:to_binary(P), "/"]);
build_url(H, P, User, Pwd) ->
    Scheme = case ?USE_HTTPS of
                 'true' -> <<"https">>;
                 'false' -> <<"http">>
             end,
    list_to_binary([Scheme, "://", wh_util:to_binary(User), ":", wh_util:to_binary(Pwd)
                    ,"@", wh_util:to_binary(H), ":", wh_util:to_binary(P), "/"
                   ]).

convert_stream_type(<<"extant">>) -> <<"continuous">>;
convert_stream_type(<<"store">>) -> <<"store">>;
convert_stream_type(_) -> <<"single">>.
