%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Webhook document accessors
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_webhook).

-export([is_enabled/1, is_enabled/2
        ,is_auto_disabled/1
        ,enable/1
        ,disable/1, disable/2
        ,disable_updates/1
        ,disabled_message/1, disabled_message/2
        ,type/0, type/1
        ,name/1, name/2, set_name/2
        ,uri/1, uri/2, set_uri/2
        ,event/1, event/2, set_event/2
        ,verb/1, verb/2, set_verb/2
        ,retries/1, retries/2, set_retries/2
        ,custom_data/1, custom_data/2, set_custom_data/2
        ,modifiers/1, modifiers/2, set_modifiers/2
        ,include_subaccounts/1, enable_subaccounts/1, disable_subaccounts/1
        ,include_internal_legs/1
        ,format/1, format/2, set_format/2
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-type hook_format() :: 'form-data' | 'json'.
-export_type([hook_format/0]).

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
-define(INCLUDE_SUBACCOUNTS, <<"include_subaccounts">>).
-define(INCLUDE_INTERNAL, <<"include_internal_legs">>).
-define(FORMAT, <<"format">>).

-spec is_enabled(doc()) -> boolean().
is_enabled(Hook) ->
    is_enabled(Hook, 'true').

-spec is_enabled(doc(), Default) -> boolean() | Default.
is_enabled(Hook, Default) ->
    kz_json:is_true(?IS_ENABLED, Hook, Default).

-spec enable(doc()) -> doc().
enable(Hook) ->
    kz_json:set_value(?IS_ENABLED
                     ,'true'
                     ,kz_json:delete_key(?DISABLED_MESSAGE, Hook)
                     ).

-spec disable(doc()) -> doc().
disable(Hook) ->
    disable(Hook, 'undefined').

-spec disable(doc(), kz_term:api_binary()) -> doc().
disable(Hook, Reason) ->
    kz_json:set_values(disable_updates(Reason), Hook).

-spec disable_updates(kz_term:api_binary()) -> kz_json:flat_proplist().
disable_updates(Reason) ->
    [{[?IS_ENABLED], 'false'}
    ,{[?DISABLED_MESSAGE], Reason}
    ].

-spec disabled_message(doc()) -> kz_term:api_binary().
disabled_message(Hook) ->
    disabled_message(Hook, 'undefined').

-spec disabled_message(doc(), Default) -> kz_term:ne_binary() | Default.
disabled_message(Hook, Default) ->
    kz_json:get_value(?DISABLED_MESSAGE, Hook, Default).

-spec is_auto_disabled(doc()) -> boolean().
is_auto_disabled(Hook) ->
    is_enabled(Hook) =:= 'false'
        andalso disabled_message(Hook) =/= 'undefined'.

-spec type() -> kz_term:ne_binary().
type() -> ?TYPE.

-spec type(doc()) -> kz_term:api_binary().
type(Hook) ->
    kz_doc:type(Hook).

-spec name(doc()) -> kz_term:api_binary().
name(Hook) ->
    name(Hook, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Hook, Default) ->
    kz_json:get_value(?NAME, Hook, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Hook, Name) ->
    kz_json:set_value(?NAME, Name, Hook).

-spec uri(doc()) -> kz_term:api_binary().
uri(Hook) ->
    uri(Hook, 'undefined').

-spec uri(doc(), Default) -> kz_term:ne_binary() | Default.
uri(Hook, Default) ->
    kz_json:get_value(?URI, Hook, Default).

-spec set_uri(doc(), kz_term:ne_binary()) -> doc().
set_uri(Hook, Uri) ->
    kz_json:set_value(?URI, Uri, Hook).

-spec event(doc()) -> kz_term:api_binary().
event(Hook) ->
    event(Hook, 'undefined').

-spec event(doc(), Default) -> kz_term:ne_binary() | Default.
event(Hook, Default) ->
    kz_json:get_value(?EVENT, Hook, Default).

-spec set_event(doc(), kz_term:ne_binary()) -> doc().
set_event(Hook, Event) ->
    kz_json:set_value(?EVENT, Event, Hook).

-type http_verb() :: 'get' | 'post' | 'put'.

-spec verb(doc()) -> http_verb().
verb(Hook) ->
    verb(Hook, 'get').

-spec verb(doc(), Default) -> http_verb() | Default.
verb(Hook, Default) ->
    case kz_json:get_value(?VERB, Hook) of
        'undefined' -> Default;
        Verb -> safe_verbs(kz_term:to_lower_binary(Verb), Default)
    end.

-spec safe_verbs(kz_term:api_binary(), http_verb() | Default) ->
                        http_verb() | Default.
safe_verbs(<<"get">>, _Default) -> <<"get">>;
safe_verbs(<<"post">>, _Default) -> <<"post">>;
safe_verbs(<<"put">>, _Default) -> <<"put">>;
safe_verbs(_Verb, Default) -> Default.

-spec set_verb(doc(), kz_term:ne_binary() | http_verb()) -> doc().
set_verb(Hook, <<_/binary>> = Verb) ->
    kz_json:set_value(?VERB, safe_verbs(Verb, <<"get">>), Hook);
set_verb(Hook, Verb) when Verb =:= 'get'
                          orelse Verb =:= 'post'
                          orelse Verb =:= 'put'
                          ->
    kz_json:set_value(?VERB, kz_term:to_binary(Verb), Hook).

-type retry_range() :: 1..5.

-spec retries(doc()) -> retry_range().
retries(Hook) ->
    retries(Hook, 3).

-spec retries(doc(), retry_range()) -> retry_range().
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

-spec custom_data(doc()) -> kz_term:api_object().
custom_data(Hook) ->
    custom_data(Hook, 'undefined').

-spec custom_data(doc(), Default) -> kz_json:object() | Default.
custom_data(Hook, Default) ->
    kz_json:get_ne_json_value(?CUSTOM_DATA, Hook, Default).

-spec set_custom_data(doc(), kz_term:api_object()) -> doc().
set_custom_data(Hook, Custom) ->
    kz_json:set_value(?CUSTOM_DATA, Custom, Hook).

-spec modifiers(doc()) -> kz_term:api_object().
modifiers(Hook) ->
    modifiers(Hook, 'undefined').

-spec modifiers(doc(), Default) -> kz_json:object() | Default.
modifiers(Hook, Default) ->
    kz_json:get_ne_json_value(?MODIFIERS, Hook, Default).

-spec set_modifiers(doc(), kz_term:api_object()) -> doc().
set_modifiers(Hook, Modifiers) ->
    kz_json:set_value(?MODIFIERS, Modifiers, Hook).

-spec include_subaccounts(doc()) -> boolean().
include_subaccounts(Hook) ->
    kz_json:is_true(?INCLUDE_SUBACCOUNTS, Hook, 'false').

-spec enable_subaccounts(doc()) -> doc().
enable_subaccounts(Hook) ->
    kz_json:set_value(?INCLUDE_SUBACCOUNTS
                     ,'true'
                     ,Hook
                     ).

-spec disable_subaccounts(doc()) -> doc().
disable_subaccounts(Hook) ->
    kz_json:set_value(?INCLUDE_SUBACCOUNTS
                     ,'false'
                     ,Hook
                     ).

-spec include_internal_legs(doc()) -> boolean().
include_internal_legs(Hook) ->
    kz_json:is_true(?INCLUDE_INTERNAL, Hook, 'true').

-spec format(doc()) -> hook_format().
format(Hook) ->
    format(Hook, 'form-data').

-spec format(doc(), Default) -> hook_format() | Default.
format(Hook, Default) ->
    case kz_json:get_value(?FORMAT, Hook) of
        'undefined' -> Default;
        Format -> safe_formats(kz_term:to_lower_binary(Format), Default)
    end.

-spec safe_formats(kz_term:api_binary(), hook_format()) -> hook_format().
safe_formats(<<"form-data">>, _Default) -> 'form-data';
safe_formats(<<"json">>, _Default) -> 'json';
safe_formats(_, Default) -> Default.

-spec set_format(doc(), kz_term:ne_binary() | hook_format()) -> doc().
set_format(Hook, <<_/binary>> = Format) ->
    set_format(Hook, safe_formats(Format, 'form-data'));
set_format(Hook, Format) when Format =:= 'form-data'
                              orelse Format =:= 'json'
                              ->
    kz_json:set_value(?FORMAT, kz_term:to_binary(Format), Hook).
