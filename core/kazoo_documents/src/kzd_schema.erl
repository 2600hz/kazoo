%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% General schema manipulation
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kzd_schema).

-export([find_schema/1
         ,properties/2
         ,max_length/2
        ]).

-include("kz_documents.hrl").

-define(SCHEMA_KEYWORDS_MAXLENGTH, <<"maxLength">>).

%%% Load schema
-spec find_schema(ne_binary()) -> api(kz_json:object()).
find_schema(<<_/binary>> = Schema) ->
    case kz_json_schema:load(Schema) of
        {'ok', SchemaJObj} -> SchemaJObj;
        {'error', _E} ->
            lager:error("failed to find schema ~s: ~p", [Schema, _E]),
            'undefined'
    end.

%%% Meta keywords
%%% ===================
-spec properties(kz_json:key(), ne_binary()) -> kz_json:object().
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
-spec max_length(kz_json:key(), ne_binary()) -> api(kz_json:object()).
max_length(Key, Schema) ->
    Properties = properties(Key, Schema),
    kz_json:get_value(?SCHEMA_KEYWORDS_MAXLENGTH, Properties).
