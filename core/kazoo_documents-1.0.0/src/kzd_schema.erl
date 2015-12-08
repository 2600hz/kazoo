%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% General schema manipulation
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kzd_schema).

-export([find_schema/1,
		 properties/2,
		 max_length/2]).

-include_lib("whistle/include/wh_types.hrl").

-define(SCHEMA_KEYWORDS_MAXLENGTH,            <<"maxLength">>).

%%% Load schema
-spec find_schema(ne_binary()) -> api_object().
find_schema(<<_/binary>> = Schema) ->
    case wh_json_schema:load(Schema) of
        {'ok', SchemaJObj} -> SchemaJObj;
        {'error', _E} ->
            lager:debug("failed to find schema ~s: ~p", [Schema, _E]),
            'undefined'
    end.

%%% Meta keywords
%%% ===================
-spec properties(wh_json:key(), api_object()) -> wh_json:object() | 'undefined'.
properties(Key, Schema) ->
	case find_schema(Schema) of
		'undefined' ->
			'undefined';
		SchemaJObj ->
			get_path(Key, SchemaJObj)
	end.

%%% Keywords
%%% ==================================
%% String
-spec max_length(wh_json:key(), api_object()) -> wh_json:object() | 'undefined'.
max_length(Key, Schema) ->
	Properties = properties(Key, Schema),
	wh_json:get_value(?SCHEMA_KEYWORDS_MAXLENGTH, Properties).

%%% Internal function
%%% =================
get_path(K, P) when not is_list(K) ->
	get_path([K], P);
get_path([], P) ->
	P;
get_path([K | Ks], P) ->
	get_path(Ks, get_value(K, P)).

get_value(Key, SchemaJObj) ->
	wh_json:get_value(Key, SchemaJObj).
