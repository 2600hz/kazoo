-module(knm_config).

-export([should_permanently_delete/0, should_permanently_delete/1
         ,released_state/0, released_state/1
         ,default_locality_url/0, default_locality_url/1
         ,locality_url/0, locality_url/1
        ]).

-include("knm.hrl").

-define(LOCALITY_CONFIG_CAT, <<"number_manager.locality">>).

-spec should_permanently_delete() -> boolean().
-spec should_permanently_delete(boolean()) -> boolean().
should_permanently_delete() ->
    should_permanently_delete('false').
should_permanently_delete(Default) ->
    whapps_config:get_is_true(?KNM_CONFIG_CAT
                              ,<<"should_permanently_delete">>
                              ,Default
                             ).

-spec released_state() -> ne_binary().
-spec released_state(ne_binary()) -> ne_binary().
released_state() ->
    released_state(?NUMBER_STATE_AVAILABLE).

-ifdef(TEST).
released_state(Default) -> Default.
-else.
released_state(Default) ->
    whapps_config:get_binary(?KNM_CONFIG_CAT
                             ,<<"released_state">>
                             ,Default
                            ).
-endif.

-spec default_locality_url() -> api_binary().
-spec default_locality_url(api_binary()) -> api_binary().
default_locality_url() ->
    default_locality_url('undefined').
default_locality_url(Default) ->
    whapps_config:get(<<"number_manager.other">>, <<"phonebook_url">>, Default).

-spec locality_url() -> api_binary().
-spec locality_url(api_binary()) -> api_binary().
locality_url() ->
    locality_url(default_locality_url()).
locality_url(Default) ->
    whapps_config:get(?LOCALITY_CONFIG_CAT, <<"url">>, Default).
