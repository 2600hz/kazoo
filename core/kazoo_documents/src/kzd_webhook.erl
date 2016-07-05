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
        ,verb/1, verb/2, set_verb/2
        ,retries/1, retries/2, set_retries/2
        ,custom_data/1, custom_data/2, set_custom_data/2
        ,modifiers/1, modifiers/2, set_modifiers/2
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(IS_ENABLED, <<"enabled">>).
-define(DISABLED_MESSAGE, <<"pvt_disabled_message">>).
-define(TYPE, <<"webhook">>).
-define(NAME, <<"name">>).
-define(URI, <<"uri">>).
-define(EVENT, <<"hook">>).
-define(VERB, <<"http_verb">>).
-define(RETRIES, <<"retries">>).
-define(CUSTOM_DATA, <<"custom_data">>).
-define(MODIFIERS, <<"modifiers">>).

-spec is_enabled(doc()) -> boolean().
-spec is_enabled(doc(), Default) -> boolean() | Default.
is_enabled(Hook) ->
    is_enabled(Hook, 'true').
is_enabled(Hook, Default) ->
    kz_json:is_true(?IS_ENABLED, Hook, Default).

-spec enable(doc()) -> doc().
enable(Hook) ->
    kz_json:set_value(?IS_ENABLED
                     ,'true'
                     ,kz_json:delete_key(?DISABLED_MESSAGE, Hook)
                     ).

-spec disable(doc()) -> doc().
-spec disable(doc(), api_binary()) -> doc().
disable(Hook) ->
    disable(Hook, 'undefined').
disable(Hook, Reason) ->
    kz_json:set_values(
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
    kz_json:get_value(?DISABLED_MESSAGE, Hook, Default).

-spec is_auto_disabled(doc()) -> boolean().
is_auto_disabled(Hook) ->
    is_enabled(Hook) =:= 'false'
        andalso disabled_message(Hook) =/= 'undefined'.

-spec type() -> ne_binary().
-spec type(doc()) -> api_binary().
type() -> ?TYPE.

type(Hook) ->
    kz_doc:type(Hook).

-spec name(doc()) -> api_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(Hook) ->
    name(Hook, 'undefined').
name(Hook, Default) ->
    kz_json:get_value(?NAME, Hook, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Hook, Name) ->
    kz_json:set_value(?NAME, Name, Hook).

-spec uri(doc()) -> api_binary().
-spec uri(doc(), Default) -> ne_binary() | Default.
uri(Hook) ->
    uri(Hook, 'undefined').
uri(Hook, Default) ->
    kz_json:get_value(?URI, Hook, Default).

-spec set_uri(doc(), ne_binary()) -> doc().
set_uri(Hook, Uri) ->
    kz_json:set_value(?URI, Uri, Hook).

-spec event(doc()) -> api_binary().
-spec event(doc(), Default) -> ne_binary() | Default.
event(Hook) ->
    event(Hook, 'undefined').
event(Hook, Default) ->
    kz_json:get_value(?EVENT, Hook, Default).

-spec set_event(doc(), ne_binary()) -> doc().
set_event(Hook, Event) ->
    kz_json:set_value(?EVENT, Event, Hook).

-type http_verb() :: 'get' | 'post'.

-spec verb(doc()) -> http_verb().
-spec verb(doc(), Default) -> http_verb() | Default.
verb(Hook) ->
    verb(Hook, 'get').
verb(Hook, Default) ->
    case kz_json:get_value(?VERB, Hook) of
        'undefined' -> Default;
        Verb -> safe_verbs(kz_util:to_lower_binary(Verb), Default)
    end.

-spec safe_verbs(api_binary(), http_verb() | Default) ->
                        http_verb() | Default.
safe_verbs(<<"get">>, _Default) -> 'get';
safe_verbs(<<"post">>, _Default) -> 'post';
safe_verbs(_Verb, Default) -> Default.

-spec set_verb(doc(), ne_binary() | http_verb()) -> doc().
set_verb(Hook, <<_/binary>> = Verb) ->
    kz_json:set_value(?VERB, safe_verbs(Verb, 'get'), Hook);
set_verb(Hook, Verb) when Verb =:= 'get'
                          orelse Verb =:= 'post'
                          ->
    kz_json:set_value(?VERB, Verb, Hook).

-type retry_range() :: 1..5.

-spec retries(doc()) -> retry_range().
-spec retries(doc(), retry_range()) -> retry_range().
retries(Hook) ->
    retries(Hook, 3).
retries(Hook, Default) ->
    constrain_retries(
      kz_json:get_integer_value(?RETRIES, Hook, Default)
     ).

-spec constrain_retries(integer()) -> retry_range().
constrain_retries(N) when N < 1 -> 1;
constrain_retries(N) when N > 5 -> 5;
constrain_retries(N) when is_integer(N) -> N.

-spec set_retries(doc(), integer()) -> doc().
set_retries(Hook, Retries) when is_integer(Retries) ->
    kz_json:set_value(?RETRIES, constrain_retries(Retries), Hook).

-spec custom_data(doc()) -> api_object().
-spec custom_data(doc(), Default) -> kz_json:object() | Default.
custom_data(Hook) ->
    custom_data(Hook, 'undefined').
custom_data(Hook, Default) ->
    kz_json:get_ne_json_value(?CUSTOM_DATA, Hook, Default).

-spec set_custom_data(doc(), api_object()) -> doc().
set_custom_data(Hook, Custom) ->
    kz_json:set_value(?CUSTOM_DATA, Custom, Hook).

-spec modifiers(doc()) -> api_object().
-spec modifiers(doc(), Default) -> kz_json:object() | Default.
modifiers(Hook) ->
    modifiers(Hook, 'undefined').
modifiers(Hook, Default) ->
    kz_json:get_ne_json_value(?MODIFIERS, Hook, Default).

-spec set_modifiers(doc(), api_object()) -> doc().
set_modifiers(Hook, Modifiers) ->
    kz_json:set_value(?MODIFIERS, Modifiers, Hook).
