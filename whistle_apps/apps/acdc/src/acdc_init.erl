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
init_account(AcctDb) ->
    lager:debug("init account: ~s", [AcctDb]),

    init_queues(AcctDb, couch_mgr:get_results(AcctDb, <<"queues/crossbar_listing">>, [include_docs])),
    init_agents(AcctDb, couch_mgr:get_results(AcctDb, <<"agents/crossbar_listing">>, [include_docs])).

-spec init_queues/2 :: (ne_binary(), {'ok', wh_json:json_objects()} | {'error', _}) -> any().
init_queues(_AcctDb, {ok, []}) ->
    lager:debug("no queues in ~s", [_AcctDb]);
init_queues(_AcctDb, {error, _E}) ->
    lager:debug("error fetching queues in ~s: ~p", [_AcctDb, _E]);
init_queues(AcctDb, {ok, Queues}) ->
    [acdc_queues_sup:new(AcctDb, wh_json:get_value(<<"doc">>, Q)) || Q <- Queues].

-spec init_agents/2 :: (ne_binary(), {'ok', wh_json:json_objects()} | {'error', _}) -> any().
init_agents(_AcctDb, {ok, []}) ->
    lager:debug("no agents in ~s", [_AcctDb]);
init_agents(_AcctDb, {error, _E}) ->
    lager:debug("error fetching agents in ~s: ~p", [_AcctDb, _E]);
init_agents(AcctDb, {ok, Agents}) ->
    [init_agent(AcctDb, wh_json:get_value(<<"doc">>, A)) || A <- Agents].

init_agent(AcctDb, Agent) ->
    case acdc_util:agent_status(AcctDb, wh_json:get_value(<<"_id">>, Agent)) of
        <<"logout">> -> ok;
        _ -> acdc_agents_sup:new(AcctDb, Agent)
    end.
