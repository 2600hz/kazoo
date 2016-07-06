%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% General schema manipulation
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kzd_schema_caller_id).

-export([external_name_max_length/0]).

-include_lib("kazoo/include/kz_types.hrl").

-define(SCHEMA_NAME, <<"caller_id">>).
-define(SCHEMA_PATH_PATTERN(K1, K2), [<<"properties">>, K1, <<"properties">>, K2]).
-define(SCHEMA_EXTERNAL_NAME, ?SCHEMA_PATH_PATTERN(<<"external">>, <<"name">>)).

-spec external_name_max_length() -> api_object().
external_name_max_length() ->
    kzd_schema:max_length(?SCHEMA_EXTERNAL_NAME, ?SCHEMA_NAME).
