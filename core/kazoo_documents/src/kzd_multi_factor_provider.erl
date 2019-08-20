%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_multi_factor_provider).

-export([new/0]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([name/1, name/2, set_name/2]).
-export([provider_name/1, provider_name/2, set_provider_name/2]).
-export([settings/1, settings/2, set_settings/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"multi_factor_provider">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec enabled(doc()) -> kz_term:api_boolean().
enabled(Doc) ->
    enabled(Doc, 'undefined').

-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"enabled">>], Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value([<<"enabled">>], Enabled, Doc).

-spec name(doc()) -> kz_term:api_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> binary() | Default.
name(Doc, Default) ->
    kz_json:get_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec provider_name(doc()) -> kz_term:api_binary().
provider_name(Doc) ->
    provider_name(Doc, 'undefined').

-spec provider_name(doc(), Default) -> binary() | Default.
provider_name(Doc, Default) ->
    kz_json:get_binary_value([<<"provider_name">>], Doc, Default).

-spec set_provider_name(doc(), binary()) -> doc().
set_provider_name(Doc, ProviderName) ->
    kz_json:set_value([<<"provider_name">>], ProviderName, Doc).

-spec settings(doc()) -> kz_term:api_object().
settings(Doc) ->
    settings(Doc, 'undefined').

-spec settings(doc(), Default) -> kz_json:object() | Default.
settings(Doc, Default) ->
    kz_json:get_json_value([<<"settings">>], Doc, Default).

-spec set_settings(doc(), kz_json:object()) -> doc().
set_settings(Doc, Settings) ->
    kz_json:set_value([<<"settings">>], Settings, Doc).
