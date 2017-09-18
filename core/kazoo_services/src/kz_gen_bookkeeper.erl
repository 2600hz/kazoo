%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz, INC
%%% @doc
%%% A generic Kazoo bookkeeper
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_gen_bookkeeper).

-include("services.hrl").

-callback is_good_standing(ne_binary(), ne_binary()) ->
    boolean().

-callback sync(kz_service_item:items(), ne_binary()) ->
    bookkeeper_sync_result().

-callback transactions(ne_binary(), gregorian_seconds(), gregorian_seconds()) ->
    {ok, kz_transaction:transactions()} |
    {error, atom()}.

-callback commit_transactions(ne_binary(), kz_transactions:kz_transactions()) ->
    ok | error.

-callback charge_transactions(ne_binary(), kz_json:objects()) ->
    kz_json:objects().
