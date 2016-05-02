-ifndef(CAMPER_HRL).
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo_apps/src/kapps_call_command_types.hrl").

-define(APP_NAME, <<"camper">>).
-define(APP_VERSION, <<"4.0.0">> ).

-define(CAMPER_CONFIG_CAT, <<"camper">>).

-define(TIMEOUT, <<"timeout">>).
-define(DEFAULT_TIMEOUT, 15).

-define(CAMPER_HRL, 'true').
-endif.
