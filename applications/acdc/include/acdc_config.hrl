-ifndef(ACDC_CONFIG_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(ACDC_CONFIG_CAT, <<"acdc">>).

-define(DEFAULT_AGENT_PAUSE_TIMEOUT, kapps_config:get(?ACDC_CONFIG_CAT, <<"default_agent_pause_timeout">>, 600)).

%% Save data to the DB
-define(ACDC_ARCHIVE_WINDOW,
        kapps_config:get_integer(<<"acdc">>, <<"archive_window_s">>, ?SECONDS_IN_HOUR)).

%% Remove data from ETS
-define(ACDC_CLEANUP_WINDOW,
        kapps_config:get_integer(<<"acdc">>, <<"cleanup_window_s">>, ?SECONDS_IN_DAY)).

-define(ACDC_CONFIG_HRL, 'true').
-endif.
