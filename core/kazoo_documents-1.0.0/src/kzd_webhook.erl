%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% Webhook document accessors
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_webhook).

-export([is_enabled/1, is_enabled/2
         ,is_auto_disabled/1
         ,enable/1
         ,disable/1, disable/2
         ,disabled_message/1, disabled_message/2
        ]).

-include("kz_documents.hrl").

-type doc() :: wh_json:object().
-export_type([doc/0]).

-define(IS_ENABLED, <<"enabled">>).
-define(DISABLED_MESSAGE, <<"pvt_disabled_message">>).

-spec is_enabled(doc()) -> boolean().
-spec is_enabled(doc(), Default) -> boolean() | Default.
is_enabled(Hook) ->
    is_enabled(Hook, 'true').
is_enabled(Hook, Default) ->
    wh_json:is_true(?IS_ENABLED, Hook, Default).

-spec enable(doc()) -> doc().
enable(Hook) ->
    wh_json:set_value(?IS_ENABLED
                      ,'true'
                      ,wh_json:delete_key(?DISABLED_MESSAGE, Hook)
                     ).

-spec disable(doc()) -> doc().
-spec disable(doc(), api_binary()) -> doc().
disable(Hook) ->
    disable(Hook, 'undefined').
disable(Hook, Reason) ->
    wh_json:set_values(
      props:filter_undefined(
        [{?IS_ENABLED, 'false'}
         ,{?DISABLED_MESSAGE, Reason}
        ])
      ,Hook
     ).

-spec disabled_message(doc()) -> api_binary().
-spec disabled_message(doc(), Default) -> ne_binary() | Default.
disabled_message(Hook) ->
    disabled_message(Hook, 'undefined').
disabled_message(Hook, Default) ->
    wh_json:get_value(?DISABLED_MESSAGE, Hook, Default).

-spec is_auto_disabled(doc()) -> boolean().
is_auto_disabled(Hook) ->
    is_enabled(Hook) =:= 'false'
        andalso disabled_message(Hook) =/= 'undefined'.
