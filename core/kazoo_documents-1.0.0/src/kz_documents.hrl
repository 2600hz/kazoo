-ifndef(KZ_DOCUMENTS_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").

-define(DEFAULT_TIMEZONE
        ,whapps_config:get(<<"accounts">>, <<"default_timezone">>, <<"America/Los_Angeles">>)
       ).

-define(KZ_DOCUMENTS_HRL, 'true').
-endif.
