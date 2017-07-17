%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%%
%%% When implementing template modules, these callbacks are a must!
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(teletype_gen_email_template).

-include_lib("teletype.hrl").


-callback id() -> ne_binary().

-callback init() -> ok.

-callback macros(kz_json:object()) -> kz_proplist().


-callback macros() -> kz_json:object().

-callback subject() -> ne_binary().

-callback category() -> ne_binary().

-callback friendly_name() -> ne_binary().
