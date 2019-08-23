-include("knm.hrl").

-define(KNM_BICS_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".bics">>).

-define(BICS_BASE_URL
       ,kapps_config:get_binary(?KNM_BICS_CONFIG_CAT, <<"api_url">>, <<>>)
       ).

-define(BICS_BEARER_TOKEN
       ,kapps_config:get_binary(?KNM_BICS_CONFIG_CAT, <<"api_token">>, <<>>)
       ).
