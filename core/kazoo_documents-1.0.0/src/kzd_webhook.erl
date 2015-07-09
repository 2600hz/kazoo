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
         ,type/0, type/1
         ,name/1, name/2, set_name/2
         ,uri/1, uri/2, set_uri/2
         ,event/1, event/2, set_event/2
        ]).

-include("kz_documents.hrl").

-type doc() :: wh_json:object().
-export_type([doc/0]).

-define(IS_ENABLED, <<"enabled">>).
-define(DISABLED_MESSAGE, <<"pvt_disabled_message">>).
-define(TYPE, <<"webhook">>).
-define(NAME, <<"name">>).
-define(URI, <<"uri">>).
-define(EVENT, <<"hook">>).

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

-spec type() -> ne_binary().
-spec type(doc()) -> api_binary().
type() -> ?TYPE.

type(Hook) ->
    wh_doc:type(Hook).

-spec name(doc()) -> api_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(Hook) ->
    name(Hook, 'undefined').
name(Hook, Default) ->
    wh_json:get_value(?NAME, Hook, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Hook, Name) ->
    wh_json:set_value(?NAME, Name, Hook).

-spec uri(doc()) -> api_binary().
-spec uri(doc(), Default) -> ne_binary() | Default.
uri(Hook) ->
    uri(Hook, 'undefined').
uri(Hook, Default) ->
    wh_json:get_value(?URI, Hook, Default).

-spec set_uri(doc(), ne_binary()) -> doc().
set_uri(Hook, Uri) ->
    wh_json:set_value(?URI, Uri, Hook).

-spec event(doc()) -> api_binary().
-spec event(doc(), Default) -> ne_binary() | Default.
event(Hook) ->
    event(Hook, 'undefined').
event(Hook, Default) ->
    wh_json:get_value(?EVENT, Hook, Default).

-spec set_event(doc(), ne_binary()) -> doc().
set_event(Hook, Event) ->
    wh_json:set_value(?EVENT, Event, Hook).
