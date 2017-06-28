-ifndef(MEDIA_HRL).

-include_lib("kazoo_types/include/kz_types.hrl").
-include_lib("kazoo/include/kz_media.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").

-define(APP_NAME, <<"media_srv">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-define(MEDIA_DB, <<"system_media">>).
-define(PORT_RANGE, 0). % use 0 to have OS assign port #, {Low, Hi} for range of ports to try
-define(PORT_OPTIONS, ['binary', {'packet',0}, {'active','false'}, {'reuseaddr', 'true'}]).
-define(MAX_RESERVED_PORTS, 10).
-define(MAX_WAIT_FOR_LISTENERS, 600 * ?MILLISECONDS_IN_SECOND). %% 600 secs = 10 minutes

-define(MEDIA_HRL, 'true').
-endif.
