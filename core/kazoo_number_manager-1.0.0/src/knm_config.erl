-module(knm_config).

-export([should_permanently_delete/1]).

-include("knm.hrl").

-spec should_permanently_delete(boolean()) -> boolean().
should_permanently_delete(Default) ->
    whapps_config:get_is_true(?CONFIG_CAT
                              ,<<"should_permanently_delete">>
                              ,'false'
                             ).
