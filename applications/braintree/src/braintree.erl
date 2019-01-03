%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, 2600Hz
%%% @doc
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
