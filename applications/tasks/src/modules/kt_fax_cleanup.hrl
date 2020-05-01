-ifndef(KT_FAX_CLEANUP_HRL).

-include("tasks.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".fax_cleanup">>).

-define(FAX_CLEANUP_ENABLED
       ,kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"enabled">>, 'false')).
-define(PAGE_SIZE
       ,kapps_config:get_integer(?MOD_CONFIG_CAT, <<"page_size">>, 32)).
-define(PER_PAGE_PAUSE,
        kapps_config:get_integer(?MOD_CONFIG_CAT, <<"per_page_pause_ms">>, ?MILLISECONDS_IN_SECOND)).

-define(CROSSBAR_LISTING, <<"faxes/crossbar_listing">>).

-define(STALE_AFTER, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"stale_after_s">>, 7 * ?SECONDS_IN_DAY)).


-define(MOVE_OPTIONS, ['override_existing_document'
                      ,{'transform', fun(_, D) -> kt_fax_cleanup:transform_doc(D) end}
                      ]
       ).

-define(KT_FAX_CLEANUP_HRL, 'true').

-endif.
