%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_hooks).

-export([register/0, register/1, register/2
	,register_rr/0, register_rr/1, register_rr/2
        ]).

-include_lib("kazoo/include/kz_types.hrl").

-spec register() -> 'true'.
-spec register(ne_binary()) -> 'true'.
-spec register(ne_binary(), ne_binary()) -> 'true'.
register() -> kz_hooks_util:register().
register(AccountId) -> kz_hooks_util:register(AccountId).
register(AccountId, EventName) -> kz_hooks_util:register(AccountId, EventName).

-spec register_rr() -> 'true'.
-spec register_rr(ne_binary()) -> 'true'.
-spec register_rr(ne_binary(), ne_binary()) -> 'true'.
register_rr() -> kz_hooks_util:register_rr().
register_rr(AccountId) -> kz_hooks_util:register_rr(AccountId).
register_rr(AccountId, EventName) -> kz_hooks_util:register_rr(AccountId, EventName).
