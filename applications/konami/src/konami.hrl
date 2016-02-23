-ifndef(KONAMI_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"konami">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(CONFIG_CAT, ?APP_NAME).

-define(EVENT(CallId, EventName, Event)
        ,{'event', CallId, EventName, Event}
       ).

-define(KONAMI_HRL, 'true').
-endif.
