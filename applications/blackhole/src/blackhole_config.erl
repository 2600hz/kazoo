%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(blackhole_config).

-export([autoload_modules/0
        ,set_autoload_modules/1, set_default_autoload_modules/1

        ,flush/0
        ]).

-include("blackhole.hrl").

-spec flush() -> 'ok'.
flush() -> kapps_config:flush(?CONFIG_CAT).

-spec autoload_modules() -> kz_term:ne_binaries().
autoload_modules() ->
    kapps_config:get(?CONFIG_CAT, <<"autoload_modules">>, ?DEFAULT_MODULES ++ ?COMMAND_MODULES).

-spec set_autoload_modules(kz_term:ne_binaries() | kz_term:atoms()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
set_autoload_modules(Modules) ->
    kapps_config:set(?CONFIG_CAT, <<"autoload_modules">>, Modules).

-spec set_default_autoload_modules(kz_term:ne_binaries() | kz_term:atoms()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
set_default_autoload_modules(Modules) ->
    kapps_config:set_default(?CONFIG_CAT, <<"autoload_modules">>, Modules).
