%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(braintree).

-export([is_configured/0]).

-spec is_configured() -> boolean().
is_configured() ->
    case braintree_request:braintree_server_url() of
        [_|_] -> 'true';
        _ -> 'false'
    end.
