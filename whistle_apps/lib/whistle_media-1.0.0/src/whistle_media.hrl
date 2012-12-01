-ifndef(WH_MEDIA_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_media.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("cowboy/include/http.hrl").

-define(WHS_CONFIG_CAT, <<"media">>).

-define(APP_NAME, <<"media_srv">>).
-define(APP_VERSION, <<"0.2.0">>).

-define(MEDIA_DB, <<"system_media">>).
-define(PORT_RANGE, 0). % use 0 to have OS assign port #, {Low, Hi} for range of ports to try
-define(PORT_OPTIONS, [binary, {packet,0}, {active,false}, {reuseaddr, true}]).
-define(MAX_RESERVED_PORTS, 10).
-define(MAX_WAIT_FOR_LISTENERS, 600000). %% 600 secs = 10 minutes

-define(CONFIG_CAT, <<"media_mgr">>).

-define(WH_MEDIA_HRL, true).
-endif.
