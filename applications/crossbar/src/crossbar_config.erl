%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(crossbar_config).

-export([autoload_modules/0, autoload_modules/1
        ,set_autoload_modules/1, set_default_autoload_modules/1

        ,flush/0
        ]).

-include("crossbar.hrl").

-spec flush() -> 'ok'.
flush() -> kapps_config:flush(?CONFIG_CAT).

-spec autoload_modules() -> ne_binaries().
-spec autoload_modules(ne_binaries() | atoms()) -> ne_binaries().
autoload_modules() ->
    autoload_modules([]).

autoload_modules(Default) ->
    Modules = kapps_config:get(?CONFIG_CAT, <<"autoload_modules">>, Default),
    remove_versioned_modules(Modules).

-spec remove_versioned_modules(binaries()) -> binaries().
remove_versioned_modules(Modules) ->
    lists:usort(lists:map(fun remove_module_version/1, Modules)).

-spec remove_module_version(binary()) -> binary().
remove_module_version(Module) ->
    maybe_remove_module_version(lists:reverse(binary_to_list(Module))).

-spec maybe_remove_module_version(list()) -> binary().
maybe_remove_module_version("1v_" ++ Module) ->
    list_to_binary(lists:reverse(Module));
maybe_remove_module_version("2v_" ++ Module) ->
    list_to_binary(lists:reverse(Module));
maybe_remove_module_version(Module) ->
    list_to_binary(lists:reverse(Module)).

-spec set_autoload_modules(ne_binaries() | atoms()) -> {'ok', kz_json:object()}.
set_autoload_modules(Modules) ->
    kapps_config:set(?CONFIG_CAT, <<"autoload_modules">>, Modules).

-spec set_default_autoload_modules(ne_binaries() | atoms()) -> {'ok', kz_json:object()}.
set_default_autoload_modules(Modules) ->
    kapps_config:set_default(?CONFIG_CAT, <<"autoload_modules">>, Modules).
