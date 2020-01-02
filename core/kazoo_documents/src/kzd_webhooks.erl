%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_webhooks).

-export([new/0]).
-export([custom_data/1, custom_data/2, set_custom_data/2]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([format/1, format/2, set_format/2]).
-export([hook/1, hook/2, set_hook/2]).
-export([http_verb/1, http_verb/2, set_http_verb/2]).
-export([include_internal_legs/1, include_internal_legs/2, set_include_internal_legs/2]).
-export([include_subaccounts/1, include_subaccounts/2, set_include_subaccounts/2]).
-export([name/1, name/2, set_name/2]).
-export([retries/1, retries/2, set_retries/2]).
-export([uri/1, uri/2, set_uri/2]).

-export([disable/1, disable/2
        ,disabled_message/1, disabled_message/2
        ,disable_updates/1
        ,enable/1
        ,is_auto_disabled/1
        ,type/0, type/1
        ,enable_subaccounts/1, disable_subaccounts/1
        ,modifiers/1, set_modifiers/2
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-type hook_format() :: 'form-data' | 'json'.

-export_type([doc/0
             ,hook_format/0
             ]).

-define(TYPE, <<"webhook">>).
-define(SCHEMA, <<"webhooks">>).
-define(DISABLED_MESSAGE, <<"pvt_disabled_message">>).

-spec new() -> doc().
new() ->
    kz_doc:set_type(kz_json_schema:default_object(?SCHEMA), type()).

-spec custom_data(doc()) -> kz_term:api_object().
custom_data(Doc) ->
    custom_data(Doc, 'undefined').

-spec custom_data(doc(), Default) -> kz_json:object() | Default.
custom_data(Doc, Default) ->
    kz_json:get_json_value([<<"custom_data">>], Doc, Default).

-spec set_custom_data(doc(), kz_json:object()) -> doc().
set_custom_data(Doc, CustomData) ->
    kz_json:set_value([<<"custom_data">>], CustomData, Doc).

-spec enabled(doc()) -> boolean().
enabled(Doc) ->
    enabled(Doc, true).

-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"enabled">>], Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value([<<"enabled">>], Enabled, Doc).

-spec format(doc()) -> hook_format().
format(Doc) ->
    format(Doc, <<"form-data">>).

-spec format(doc(), Default) -> hook_format() | Default.
format(Doc, Default) ->
    case kz_json:get_value([<<"format">>], Doc) of
        'undefined' -> Default;
        Format -> safe_formats(kz_term:to_lower_binary(Format), Default)
    end.

-spec safe_formats(kz_term:api_binary(), hook_format()) -> hook_format().
safe_formats(<<"form-data">>, _Default) -> 'form-data';
safe_formats(<<"json">>, _Default) -> 'json';
safe_formats(_, Default) -> Default.

-spec set_format(doc(), kz_term:ne_binary() | hook_format()) -> doc().
set_format(Hook, <<Format/binary>>) ->
    set_format(Hook, safe_formats(Format, 'form-data'));
set_format(Hook, Format) when Format =:= 'form-data'
                              orelse Format =:= 'json'
                              ->
    kz_json:set_value([<<"format">>], kz_term:to_binary(Format), Hook).

-spec hook(doc()) -> kz_term:api_binary().
hook(Doc) ->
    hook(Doc, 'undefined').

-spec hook(doc(), Default) -> binary() | Default.
hook(Doc, Default) ->
    kz_json:get_binary_value([<<"hook">>], Doc, Default).

-spec set_hook(doc(), binary()) -> doc().
set_hook(Doc, Hook) ->
    kz_json:set_value([<<"hook">>], Hook, Doc).

-spec http_verb(doc()) -> binary().
http_verb(Doc) ->
    http_verb(Doc, <<"post">>).

-spec http_verb(doc(), Default) -> binary() | Default.
http_verb(Doc, Default) ->
    kz_json:get_binary_value([<<"http_verb">>], Doc, Default).

-spec set_http_verb(doc(), binary()) -> doc().
set_http_verb(Doc, HttpVerb) ->
    kz_json:set_value([<<"http_verb">>], HttpVerb, Doc).

-spec include_internal_legs(doc()) -> boolean().
include_internal_legs(Doc) ->
    include_internal_legs(Doc, true).

-spec include_internal_legs(doc(), Default) -> boolean() | Default.
include_internal_legs(Doc, Default) ->
    kz_json:get_boolean_value([<<"include_internal_legs">>], Doc, Default).

-spec set_include_internal_legs(doc(), boolean()) -> doc().
set_include_internal_legs(Doc, IncludeInternalLegs) ->
    kz_json:set_value([<<"include_internal_legs">>], IncludeInternalLegs, Doc).

-spec include_subaccounts(doc()) -> kz_term:api_boolean().
include_subaccounts(Doc) ->
    include_subaccounts(Doc, 'undefined').

-spec include_subaccounts(doc(), Default) -> boolean() | Default.
include_subaccounts(Doc, Default) ->
    kz_json:get_boolean_value([<<"include_subaccounts">>], Doc, Default).

-spec set_include_subaccounts(doc(), boolean()) -> doc().
set_include_subaccounts(Doc, IncludeSubaccounts) ->
    kz_json:set_value([<<"include_subaccounts">>], IncludeSubaccounts, Doc).

-spec name(doc()) -> kz_term:api_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> binary() | Default.
name(Doc, Default) ->
    kz_json:get_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-type retry_range() :: 1..5.

-spec retries(doc()) -> retry_range().
retries(Doc) ->
    retries(Doc, 2).

-spec retries(doc(), Default) -> retry_range() | Default.
retries(Doc, Default) ->
    constrain_retries(kz_json:get_integer_value([<<"retries">>], Doc, Default)).

-spec constrain_retries(integer() | Default) -> retry_range() | Default.
constrain_retries(Default) when not is_integer(Default) -> Default;
constrain_retries(N) when N < 1 -> 1;
constrain_retries(N) when N > 5 -> 5;
constrain_retries(N) when is_integer(N) -> N.


-spec set_retries(doc(), integer()) -> doc().
set_retries(Doc, Retries) ->
    kz_json:set_value([<<"retries">>], Retries, Doc).

-spec uri(doc()) -> kz_term:api_binary().
uri(Doc) ->
    uri(Doc, 'undefined').

-spec uri(doc(), Default) -> binary() | Default.
uri(Doc, Default) ->
    kz_json:get_binary_value([<<"uri">>], Doc, Default).

-spec set_uri(doc(), binary()) -> doc().
set_uri(Doc, Uri) ->
    kz_json:set_value([<<"uri">>], Uri, Doc).

-spec enable(doc()) -> doc().
enable(Hook) ->
    kz_json:set_value([<<"enabled">>]
                     ,'true'
                     ,kz_json:delete_key(?DISABLED_MESSAGE, Hook)
                     ).

-spec disable(doc()) -> doc().
disable(Hook) ->
    disable(Hook, 'undefined').

-spec disable(doc(), kz_term:api_binary()) -> doc().
disable(Hook, Reason) ->
    kz_json:set_values(disable_updates(Reason), Hook).

-spec disable_updates(kz_term:api_ne_binary()) -> kz_json:flat_proplist().
disable_updates('undefined') ->
    [{[<<"enabled">>], 'false'}];
disable_updates(Reason) ->
    [{[<<"enabled">>], 'false'}
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
    enabled(Hook) =:= 'false'
        andalso disabled_message(Hook) =/= 'undefined'.

-spec type() -> kz_term:ne_binary().
type() -> ?TYPE.

-spec type(doc()) -> kz_term:api_binary().
type(Hook) ->
    kz_doc:type(Hook).


-spec enable_subaccounts(doc()) -> doc().
enable_subaccounts(Hook) ->
    kz_json:set_value(<<"include_subaccounts">>, 'true', Hook).

-spec disable_subaccounts(doc()) -> doc().
disable_subaccounts(Hook) ->
    kz_json:set_value(<<"include_subaccounts">>, 'false', Hook).

-spec modifiers(doc()) -> kz_term:api_object().
modifiers(Hook) ->
    kz_json:get_ne_json_value([<<"modifiers">>], Hook).

-spec set_modifiers(doc(), kz_json:object()) -> doc().
set_modifiers(Hook, Modifiers) ->
    kz_json:set_value([<<"modifiers">>], Modifiers, Hook).
