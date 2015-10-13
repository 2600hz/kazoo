-module(knm_config).

-export([should_permanently_delete/0, should_permanently_delete/1
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
