-module(knm_config).

-export([should_permanently_delete/0, should_permanently_delete/1
        ,released_state/0, released_state/1
        ,default_locality_url/0, default_locality_url/1
        ,locality_url/0, locality_url/1
        ,should_age/0
        ,feature_e911/1
        ]).

-include("knm.hrl").

-define(DEFAULT_E911_FEATURE, ?DASH_KEY).

-define(LOCALITY_CONFIG_CAT, <<"number_manager.locality">>).

-spec should_age() -> boolean().
should_age() ->
    kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"should_age">>, 'false').

-spec should_permanently_delete() -> boolean().
-spec should_permanently_delete(boolean()) -> boolean().
should_permanently_delete() ->
    should_permanently_delete('false').
should_permanently_delete(Default) ->
    kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"should_permanently_delete">>, Default).

-spec released_state() -> ne_binary().
-spec released_state(ne_binary()) -> ne_binary().
released_state() ->
    released_state(?NUMBER_STATE_AVAILABLE).
released_state(Default) ->
    kapps_config:get_binary(?KNM_CONFIG_CAT, <<"released_state">>, Default).

-spec default_locality_url() -> api_binary().
-spec default_locality_url(api_binary()) -> api_binary().
default_locality_url() ->
    default_locality_url('undefined').
default_locality_url(Default) ->
    kapps_config:get(<<"number_manager.other">>, <<"phonebook_url">>, Default).

-spec locality_url() -> api_binary().
-spec locality_url(api_binary()) -> api_binary().
locality_url() ->
    locality_url(default_locality_url()).
locality_url(Default) ->
    kapps_config:get(?LOCALITY_CONFIG_CAT, <<"url">>, Default).

-spec feature_e911(ne_binary()) -> ne_binary().
feature_e911(?MATCH_ACCOUNT_RAW(AccountId)) ->
    kapps_account_config:get_from_reseller(AccountId
                                          ,?KNM_CONFIG_CAT
                                          ,<<"e911_feature">>
                                          ,?DEFAULT_E911_FEATURE
                                          ).
