%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz INC
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


-callback id() -> kz_term:ne_binary().

-callback init() -> ok.

-callback macros(kz_json:object()) -> kz_term:proplist().


-callback macros() -> kz_json:object().

-callback subject() -> kz_term:ne_binary().

-callback category() -> kz_term:ne_binary().

-callback friendly_name() -> kz_term:ne_binary().
