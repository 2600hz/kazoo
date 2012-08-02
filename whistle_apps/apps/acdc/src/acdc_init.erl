%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Iterate over each account, find configured queues and configured
%%% agents, and start the attendant processes
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_init).

-export([start_link/0, init_acdc/0]).

-include("acdc.hrl").

-spec start_link/0 :: () -> 'ignore'.
start_link() ->
    spawn(?MODULE, init_acdc, []),
    ignore.

-spec init_acdc/0 :: () -> any().
init_acdc() ->
    Accts = whapps_util:get_all_accounts(encoded),
    [init_account(Acct) || Acct <- Accts].

-spec init_account/1 :: (ne_binary()) -> 'ok'.
init_account(Acct) ->
    lager:debug("acdc_init: ~s", [Acct]),

    init_queues(Acct, couch_mgr:get_results(Acct, <<"queues/crossbar_listing">>, [include_docs])),
    init_agents(Acct, couch_mgr:get_results(Acct, <<"agents/crossbar_listing">>, [include_docs])).

-spec init_queues/2 :: (ne_binary(), {'ok', wh_json:json_objects()} | {'error', _}) -> any().
init_queues(Acct, {ok, []}) ->
    lager:debug("no queues in ~s", [Acct]);
init_queues(Acct, {error, _E}) ->
    lager:debug("error fetching queues in ~s: ~p", [Acct, _E]);
init_queues(Acct, {ok, Queues}) ->
    [acdc_queues_sup:new(Acct, wh_json:get_value(<<"doc">>, Q)) || Q <- Queues].

-spec init_agents/2 :: (ne_binary(), {'ok', wh_json:json_objects()} | {'error', _}) -> any().
init_agents(Acct, {ok, []}) ->
    lager:debug("no agents in ~s", [Acct]);
init_agents(Acct, {error, _E}) ->
    lager:debug("error fetching agents in ~s: ~p", [Acct, _E]);
init_agents(Acct, {ok, Agents}) ->
    [acdc_agents_sup:new(Acct, wh_json:get_value(<<"doc">>, A)) || A <- Agents].
