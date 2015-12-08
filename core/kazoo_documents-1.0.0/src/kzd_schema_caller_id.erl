%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% General schema manipulation
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kzd_schema_caller_id).

-define (SCHEMA_NAME, <<"caller_id">>).
-define (SCHEMA_PATH_PATTERN(K1, K2), [<<"properties">>, K1, <<"properties">>, K2]).
-define (SCHEMA_EXTERNAL_NAME, ?SCHEMA_PATH_PATTERN(<<"external">>, <<"name">>)).

-export([external_name_max_length/0]).

-spec external_name_max_length() -> wh_json:object() | 'undefined'.
external_name_max_length() ->
	kzd_schema:max_length(?SCHEMA_EXTERNAL_NAME, ?SCHEMA_NAME).
