-ifndef(WHISTLE_MEDIA_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_media.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(WHM_CONFIG_CAT, <<"media">>).

-define(APP_NAME, <<"media_srv">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(CACHE_NAME, 'media_mgr_cache').

-define(MEDIA_DB, <<"system_media">>).
-define(PORT_RANGE, 0). % use 0 to have OS assign port #, {Low, Hi} for range of ports to try
-define(PORT_OPTIONS, ['binary'
                       ,{'packet', 0}
                       ,{'active', 'false'}
                       ,{'reuseaddr', 'true'}
                      ]).
-define(MAX_RESERVED_PORTS, 10).
-define(MAX_WAIT_FOR_LISTENERS, 600 * ?MILLISECONDS_IN_SECOND). %% 600 secs = 10 minutes

-define(CONFIG_CAT, ?WHM_CONFIG_CAT).

-define(PROMPT_LANGUAGE_KEY, <<"default_language">>).

-define(CONFIG_KVS, [{<<"use_https">>, 'false'}
                     ,{<<"authenticated_playback">>, 'false'}
                     ,{<<"proxy_username">>, 'undefined'}
                     ,{<<"proxy_password">>, 'undefined'}
                     ,{<<"authenticated_store">>, 'true'}
                     ,{<<"proxy_store_authenticate">>, 'true'}
                     ,{<<"proxy_username">>, 'undefined'}
                     ,{<<"proxy_password">>, 'undefined'}
                     ,{<<"proxy_store_acls">>, [<<"127.0.0.0/24">>]}
                     ,{<<"max_recording_time_limit">>, 3600}
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

-record(media_store_path,
        {db :: ne_binary()
        ,id :: ne_binary()
        ,att :: ne_binary()
        ,opt = [] :: wh_proplist()
        }).

-type media_store_path() :: #media_store_path{}.

-define(WHISTLE_MEDIA_HRL, 'true').
-endif.
