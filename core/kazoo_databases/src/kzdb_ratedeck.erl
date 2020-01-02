%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzdb_ratedeck).

-export([prefix_keys/1]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-spec prefix_keys(kz_term:ne_binary()) -> [integer()].
prefix_keys(Number) ->
    case kz_binary:remove_non_numeric(Number) of
        <<>> -> [];
        <<D:1/binary, Rest/binary>> ->
            build_keys(Rest, D, [kz_term:to_integer(D)])
    end.

-spec build_keys(binary(), kz_term:ne_binary(), [integer()]) -> [integer()].
build_keys(<<D:1/binary, Rest/binary>>, Prefix, Acc) ->
    build_keys(Rest, <<Prefix/binary, D/binary>>, [kz_term:to_integer(<<Prefix/binary, D/binary>>) | Acc]);
build_keys(<<>>, _, Acc) -> Acc.
