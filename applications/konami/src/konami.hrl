-ifndef(KONAMI_HRL).
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").

-define(APP_NAME, <<"konami">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(CONFIG_CAT, ?APP_NAME).

-define(EVENT(CallId, EventName, Event)
       ,{'event', CallId, EventName, Event}
       ).

-define(WSD_ENABLED, kapps_config:get_is_true(?CONFIG_CAT, <<"webseq_enabled">>, 'false')).

-define(KONAMI_HRL, 'true').
-endif.
