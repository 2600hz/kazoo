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
flush() -> whapps_config:flush(?CONFIG_CAT).

-spec autoload_modules() -> ne_binaries().
-spec autoload_modules(ne_binaries() | atoms()) -> ne_binaries().
autoload_modules() ->
    autoload_modules([]).

autoload_modules(Default) ->
    whapps_config:get(?CONFIG_CAT, <<"autoload_modules">>, Default).

-spec set_autoload_modules(ne_binaries() | atoms()) -> {'ok', wh_json:object()}.
set_autoload_modules(Modules) ->
    whapps_config:set(?CONFIG_CAT, <<"autoload_modules">>, Modules).

-spec set_default_autoload_modules(ne_binaries() | atoms()) -> {'ok', wh_json:object()}.
set_default_autoload_modules(Modules) ->
    whapps_config:set_default(?CONFIG_CAT, <<"autoload_modules">>, Modules).
