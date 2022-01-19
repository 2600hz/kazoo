%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2022, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_hooks).

-export([register/0, register/1, register/2
        ,deregister/0, deregister/1, deregister/2
        ,register_rr/0, register_rr/1, register_rr/2
        ,deregister_rr/0, deregister_rr/1, deregister_rr/2
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-spec register() -> 'true'.
register() -> kz_hooks_util:register().

-spec register(kz_term:ne_binary()) -> 'true'.
register(AccountId) -> kz_hooks_util:register(AccountId).

-spec register(kz_term:ne_binary(), kz_term:ne_binary()) -> 'true'.
register(AccountId, EventName) -> kz_hooks_util:register(AccountId, EventName).

-spec deregister() -> 'true'.
deregister() -> kz_hooks_util:deregister().

-spec deregister(kz_term:ne_binary()) -> 'true'.
deregister(AccountId) -> kz_hooks_util:deregister(AccountId).

-spec deregister(kz_term:ne_binary(), kz_term:ne_binary()) -> 'true'.
deregister(AccountId, EventName) -> kz_hooks_util:deregister(AccountId, EventName).

-spec register_rr() -> 'true'.
register_rr() -> kz_hooks_util:register_rr().

-spec register_rr(kz_term:ne_binary()) -> 'true'.
register_rr(AccountId) -> kz_hooks_util:register_rr(AccountId).

-spec register_rr(kz_term:ne_binary(), kz_term:ne_binary()) -> 'true'.
register_rr(AccountId, EventName) -> kz_hooks_util:register_rr(AccountId, EventName).

-spec deregister_rr() -> 'true'.
deregister_rr() -> kz_hooks_util:deregister_rr().

-spec deregister_rr(kz_term:ne_binary()) -> 'true'.
deregister_rr(AccountId) -> kz_hooks_util:deregister_rr(AccountId).

-spec deregister_rr(kz_term:ne_binary(), kz_term:ne_binary()) -> 'true'.
deregister_rr(AccountId, EventName) -> kz_hooks_util:deregister_rr(AccountId, EventName).
