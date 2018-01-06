%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz, INC
%%% @doc
%%% A generic Kazoo bookkeeper
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_gen_bookkeeper).

-include("services.hrl").

-callback is_good_standing(kz_term:ne_binary(), kz_term:ne_binary()) ->
    boolean().

-callback sync(kz_service_items:items(), kz_term:ne_binary()) ->
    bookkeeper_sync_result().

-callback transactions(kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()) ->
    {ok, kz_transaction:transactions()} |
    {error, atom()}.

-callback commit_transactions(kz_term:ne_binary(), kz_transactions:kz_transactions()) ->
    ok | error.

-callback charge_transactions(kz_term:ne_binary(), kz_json:objects()) ->
    kz_json:objects().
