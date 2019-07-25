-ifndef(KAZOO_MEDIA_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_media/include/kz_media.hrl").

-define(APP_NAME, <<"media_srv">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(CONFIG_CAT, <<"media">>).

-define(MEDIA_DB, <<"system_media">>).
-define(PORT_RANGE, 0). % use 0 to have OS assign port #, {Low, Hi} for range of ports to try
-define(PORT_OPTIONS, ['binary'
                      ,{'packet', 0}
                      ,{'active', 'false'}
                      ,{'reuseaddr', 'true'}
                      ]).
-define(MAX_RESERVED_PORTS, 10).
-define(MAX_WAIT_FOR_LISTENERS, (10 * 60 * ?MILLISECONDS_IN_SECOND)).

-define(PROMPT_LANGUAGE_KEY, <<"default_language">>).

-define(CONFIG_KVS, [{<<"use_https">>, 'false'}
                    ,{<<"authenticated_playback">>, 'false'}
                    ,{<<"authenticated_store">>, 'true'}
                    ,{<<"proxy_store_authenticate">>, 'true'}
                    ,{<<"proxy_username">>, 'undefined'}
                    ,{<<"proxy_password">>, 'undefined'}
                    ,{<<"proxy_store_acls">>, [<<"127.0.0.0/24">>]}
                    ,{<<"max_recording_time_limit">>, kz_media_util:max_recording_time_limit()}
                    ,{[<<"call_recording">>, <<"extension">>], <<"mp3">>}
                    ,{<<"store_recordings">>, 'false'}
                    ,{<<"third_party_bigcouch_host">>, 'undefined'}
                    ,{<<"third_party_bigcouch_port">>, 5984}
                    ,{<<"use_bigcouch_direct">>, 'true'}
                    ,{<<"bigcouch_host">>, 'undefined'}
                    ,{<<"bigcouch_port">>, 'undefined'}
                    ,{<<"use_media_proxy">>, 'true'}
                    ,{<<"proxy_port">>, 24517}
                    ,{<<"use_plaintext">>, 'true'}
                    ,{<<"proxy_listeners">>, 25}
                    ,{<<"use_ssl_proxy">>, 'false'}
                    ,{<<"ssl_cert">>, 'undefined'}
                    ,{<<"ssl_key">>, 'undefined'}
                    ,{<<"ssl_port">>, 'undefined'}
                    ,{<<"ssl_password">>, 'undefined'}
                    ,{<<"record_min_sec">>, 0}
                    ]).

-define(KAZOO_MEDIA_HRL, 'true').
-endif.
