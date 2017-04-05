-module(knm_config).

-export([should_permanently_delete/0
        ,released_state/0
        ,locality_url/0
        ,should_age/0
        ]).

-include("knm.hrl").

-define(LOCALITY_CONFIG_CAT, <<"number_manager.locality">>).

-define(DEFAULT_LOCALITY_URL
       ,kapps_config:get_ne_binary(<<"number_manager.other">>, <<"phonebook_url">>)).


-spec should_age() -> boolean().
should_age() ->
    kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"should_age">>, false).

-spec should_permanently_delete() -> boolean().
should_permanently_delete() ->
    kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"should_permanently_delete">>, false).

-spec released_state() -> ne_binary().
released_state() ->
    kapps_config:get_ne_binary(?KNM_CONFIG_CAT, <<"released_state">>, ?NUMBER_STATE_AVAILABLE).

-spec locality_url() -> api_ne_binary().
locality_url() ->
    kapps_config:get_ne_binary(?LOCALITY_CONFIG_CAT, <<"url">>, ?DEFAULT_LOCALITY_URL).
