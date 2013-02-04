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
-export([reconcile/1]).
-export([stop_reconciler/0]).
-export([start_reconciler/0]).

-include("jonny5.hrl").

-spec flush() -> 'ok'.
flush() ->
    wh_cache:flush_local(?JONNY5_CACHE).

-spec get_limits(atom() | string() | ne_binary()) -> iolist().
get_limits(Account) when not is_binary(Account) ->
    get_limits(wh_util:to_binary(Account));
get_limits(Account) ->
    Limits = j5_util:get_limits(Account),
    lists:zip(record_info(fields, limits), tl(tuple_to_list(Limits))).
    
-spec reconcile(ne_binary()) -> 'ok' | {'error', _}.
reconcile(Account) when not is_binary(Account) ->
    reconcile(wh_util:to_binary(Account));
reconcile(Account) ->
    j5_reconciler:process_account(Account).

-spec stop_reconciler() -> any().
stop_reconciler() ->
    supervisor:terminate_child(jonny5_sup, j5_reconciler).

-spec start_reconciler() -> any().
start_reconciler() ->
    supervisor:start_child(jonny5_sup, j5_reconciler).
