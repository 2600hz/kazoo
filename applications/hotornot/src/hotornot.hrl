-ifndef(HOTORNOT_HRL).
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").

-define(APP_NAME, <<"hotornot">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(CACHE_NAME, 'hotornot_cache').

-type trunking_options() :: ne_binaries().

-define(DEFAULT_FILTER_LIST, [<<"direction">>
                             ,<<"route_options">>
                             ,<<"routes">>
                             ]).

-define(DEFAULT_MINIMUM, kapps_config:get_integer(?APP_NAME, <<"default_rate_minimum">>, 60)).
-define(DEFAULT_INCREMENT, kapps_config:get_integer(?APP_NAME, <<"default_rate_increment">>, 60)).
-define(DEFAULT_NOCHARGE, kapps_config:get_integer(?APP_NAME, <<"default_rate_nocharge_time">>, 0)).
-define(DEFAULT_SURCHARGE, kapps_config:get_float(?APP_NAME, <<"default_rate_surcharge">>, 0.0)).
-define(DEFAULT_COST, kapps_config:get_float(?APP_NAME, <<"default_rate_cost">>, 0.0)).
-define(DEFAULT_INT_COST, kapps_config:get_float(?APP_NAME, <<"default_rate_internal_cost">>, 0.0)).

-define(HOTORNOT_HRL, 'true').
-endif.
