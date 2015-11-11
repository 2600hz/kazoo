-module(knm_config).

-export([should_permanently_delete/0, should_permanently_delete/1
         ,released_state/0, released_state/1
        ]).

-include("knm.hrl").

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
