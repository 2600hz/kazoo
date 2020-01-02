%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_config).

-export([autoload_modules/0, autoload_modules/1
        ,set_autoload_modules/1, set_default_autoload_modules/1

        ,flush/0
        ]).

-include("crossbar.hrl").

-spec flush() -> 'ok'.
flush() -> kapps_config:flush(?CONFIG_CAT).

-spec autoload_modules() -> kz_term:ne_binaries().
autoload_modules() ->
    autoload_modules(?DEFAULT_MODULES).

-spec autoload_modules(kz_term:ne_binaries() | kz_term:atoms()) -> kz_term:ne_binaries().
autoload_modules(Default) ->
    Modules = kapps_config:get(?CONFIG_CAT, <<"autoload_modules">>, Default),
    remove_versioned_modules(Modules).

-spec remove_versioned_modules(kz_term:binaries() | kz_term:atoms()) -> kz_term:binaries().
remove_versioned_modules(Modules) ->
    lists:usort(lists:map(fun remove_module_version/1, Modules)).

-spec remove_module_version(binary() | atom()) -> binary().
remove_module_version(Module)
  when is_atom(Module) ->
    remove_module_version(kz_term:to_binary(Module));
remove_module_version(Module) ->
    maybe_remove_module_version(lists:reverse(binary_to_list(Module))).

-spec maybe_remove_module_version(list()) -> binary().
maybe_remove_module_version("1v_" ++ Module) ->
    list_to_binary(lists:reverse(Module));
maybe_remove_module_version("2v_" ++ Module) ->
    list_to_binary(lists:reverse(Module));
maybe_remove_module_version(Module) ->
    list_to_binary(lists:reverse(Module)).

-spec set_autoload_modules(kz_term:ne_binaries() | kz_term:atoms()) -> {'ok', kz_json:object()}.
set_autoload_modules(Modules) ->
    kapps_config:set(?CONFIG_CAT, <<"autoload_modules">>, Modules).

-spec set_default_autoload_modules(kz_term:ne_binaries() | kz_term:atoms()) -> {'ok', kz_json:object()}.
set_default_autoload_modules(Modules) ->
    kapps_config:set_default(?CONFIG_CAT, <<"autoload_modules">>, Modules).
