-ifndef(ACDC_CONFIG_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

%% Save data to the DB
-define(ACDC_ARCHIVE_WINDOW,
        kapps_config:get_integer(<<"acdc">>, <<"archive_window_s">>, ?SECONDS_IN_HOUR)).

-define(ACDC_CONFIG_HRL, 'true').
-endif.
