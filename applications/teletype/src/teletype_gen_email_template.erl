%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc When implementing template modules, these callbacks are a must!
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_gen_email_template).

-include_lib("teletype.hrl").


-callback id() -> kz_term:ne_binary().

-callback init() -> ok.

-callback macros(kz_json:object()) -> kz_term:proplist().


-callback macros() -> kz_json:object().

-callback subject() -> kz_term:ne_binary().

-callback category() -> kz_term:ne_binary().

-callback friendly_name() -> kz_term:ne_binary().
