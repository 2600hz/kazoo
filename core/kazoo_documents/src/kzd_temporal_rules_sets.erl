%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_temporal_rules_sets).

-export([new/0]).
-export([flags/1, flags/2, set_flags/2]).
-export([name/1, name/2, set_name/2]).
-export([temporal_rules/1, temporal_rules/2, set_temporal_rules/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"temporal_rules_sets">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec flags(doc()) -> kz_term:api_ne_binaries().
flags(Doc) ->
    flags(Doc, 'undefined').

-spec flags(doc(), Default) -> kz_term:ne_binaries() | Default.
flags(Doc, Default) ->
    kz_json:get_list_value([<<"flags">>], Doc, Default).

-spec set_flags(doc(), kz_term:ne_binaries()) -> doc().
set_flags(Doc, Flags) ->
    kz_json:set_value([<<"flags">>], Flags, Doc).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec temporal_rules(doc()) -> kz_term:api_ne_binaries().
temporal_rules(Doc) ->
    temporal_rules(Doc, 'undefined').

-spec temporal_rules(doc(), Default) -> kz_term:ne_binaries() | Default.
temporal_rules(Doc, Default) ->
    kz_json:get_list_value([<<"temporal_rules">>], Doc, Default).

-spec set_temporal_rules(doc(), kz_term:ne_binaries()) -> doc().
set_temporal_rules(Doc, TemporalRules) ->
    kz_json:set_value([<<"temporal_rules">>], TemporalRules, Doc).
