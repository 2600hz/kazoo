%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------

-include("knm.hrl").

-define(KNM_VOXBONE_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".voxbone">>).

-define(VOXBONE_DEFAULT_PAGE_SIZE, 20).
-define(VOXBONE_PRODUCTION_HOST, <<"api.voxbone.com">>).
-define(VOXBONE_SANDBOX_HOST, <<"sandbox.voxbone.com">>).
-define(VOXBONE_BETA_HOST, <<"beta.voxbone.com">>).
-define(VOXBONE_ENVIRONMENT
       ,kapps_config:get_binary(?KNM_VOXBONE_CONFIG_CAT, <<"environment">>, <<"sandbox">>)
       ).
-define(VOXBONE_API_USERNAME
       ,kapps_config:get_binary(?KNM_VOXBONE_CONFIG_CAT, <<"api_username">>, <<>>)
       ).
-define(VOXBONE_API_PASSWORD
       ,kapps_config:get_binary(?KNM_VOXBONE_CONFIG_CAT, <<"api_password">>, <<>>)
       ).
-define(VOXBONE_API_PAGE_SIZE
       ,kapps_config:get_integer(?KNM_VOXBONE_CONFIG_CAT, <<"max_page_size">>, ?VOXBONE_DEFAULT_PAGE_SIZE)
       ).

-type qs_option() :: {'areaCode', kz_term:ne_binary() | kz_term:pos_integer()} |
                     {'didIds', kz_term:pos_integer()} |
                     {'didType', kz_term:ne_binary()} |
                     {'didGroupIds', kz_term:ne_binary() | kz_term:pos_integer()} |
                     {'countryCodeA3', kz_term:ne_binary()} |
                     {'e164Pattern', kz_term:ne_binary()} |
                     {'orderReference', kz_term:pos_integer()} |
                     {'pageNumber', kz_term:pos_integer()} |
                     {'pageSize', kz_term:pos_integer()} |
                     {'showEmpty', kz_term:boolean()}.

-type qs_options() :: [qs_option()].
