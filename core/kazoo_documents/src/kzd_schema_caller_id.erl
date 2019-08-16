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
-module(kzd_schema_caller_id).

-export([external_name_max_length/0]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(SCHEMA_NAME, <<"caller_id">>).
-define(SCHEMA_PATH_PATTERN(K1, K2), [<<"properties">>, K1, <<"properties">>, K2]).
-define(SCHEMA_EXTERNAL_NAME, ?SCHEMA_PATH_PATTERN(<<"external">>, <<"name">>)).

-spec external_name_max_length() -> kz_term:api_integer().
external_name_max_length() ->
    kzd_schema:max_length(?SCHEMA_EXTERNAL_NAME, ?SCHEMA_NAME).
