%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc General schema manipulation
%%% @author Hesaam Farhang
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_schema).

-export([find_schema/1
        ,properties/2
        ,max_length/2
        ]).

-include("kz_documents.hrl").

-define(SCHEMA_KEYWORDS_MAXLENGTH, <<"maxLength">>).

%%% Load schema
-spec find_schema(kz_term:ne_binary()) -> kz_term:api_object().
find_schema(<<_/binary>> = Schema) ->
    case kz_json_schema:load(Schema) of
        {'ok', SchemaJObj} -> SchemaJObj;
        {'error', _E} ->
            lager:error("failed to find schema ~s: ~p", [Schema, _E]),
            'undefined'
    end.

%%% Meta keywords
%%% ===================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec properties(kz_json:path(), kz_term:ne_binary()) -> kz_json:object().
properties(Key, Schema) ->
    case find_schema(Schema) of
        'undefined' ->
            kz_json:new();
        SchemaJObj ->
            kz_json:get_value(Key, SchemaJObj, kz_json:new())
    end.

%%% Keywords
%%% ==================================
%% String
-spec max_length(kz_json:path(), kz_term:ne_binary()) -> kz_term:api_integer().
max_length(Key, Schema) ->
    Properties = properties(Key, Schema),
    kz_json:get_integer_value(?SCHEMA_KEYWORDS_MAXLENGTH, Properties).
