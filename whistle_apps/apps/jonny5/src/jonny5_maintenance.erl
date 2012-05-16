%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(jonny5_maintenance).

-export([flush/0]).
-export([get_limits/1]).

-include("jonny5.hrl").

-spec flush/0 :: () -> 'ok'.
flush() ->
    wh_cache:flush_local(?JONNY5_CACHE).

-spec get_limits/1 :: (atom() | string() | ne_binary()) -> iolist().
get_limits(Account) when not is_binary(Account) ->
    get_limits(wh_util:to_binary(Account));
get_limits(Account) ->
    Limits = j5_util:get_limits(Account),
    lists:zip(record_info(fields, limits), tl(tuple_to_list(Limits))).
    
