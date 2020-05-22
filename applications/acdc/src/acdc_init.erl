%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Iterate over each account, find configured queues and configured
%%% agents, and start the attendant processes
%%%
%%% @author James Aimonetti
%%% @author Daniel Finke
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_init).

-export([start_link/0
        ,init_db/0
        ,init_acdc/0
        ,init_acct/1
        ,init_acct_queues/1
        ,init_acct_agents/1
        ]).

-include("acdc.hrl").

-spec start_link() -> 'ignore'.
start_link() ->
    _ = declare_exchanges(),
    _ = kz_process:spawn(fun init_acdc/0, []),
    'ignore'.

-spec init_acdc() -> any().
init_acdc() ->
    kz_log:put_callid(?MODULE),
    case kz_datamgr:get_all_results(?KZ_ACDC_DB, <<"acdc/accounts_listing">>) of
        {'ok', []} ->
            lager:debug("no accounts configured for acdc");
        {'ok', Accounts} ->
            [init_acct(kz_json:get_value(<<"key">>, Account)) || Account <- Accounts];
        {'error', 'not_found'} ->
            lager:debug("acdc db not found, initializing"),
            _ = init_db(),
            lager:debug("consider running acdc_maintenance:migrate() to enable acdc for already-configured accounts");
        {'error', _E} ->
            lager:debug("failed to query acdc db: ~p", [_E])
    end.

-spec init_db() -> any().
init_db() ->
    _ = kz_datamgr:db_create(?KZ_ACDC_DB),
    _ = kz_datamgr:revise_doc_from_file(?KZ_ACDC_DB, 'crossbar', <<"views/acdc.json">>).

-spec init_acct(kz_term:ne_binary()) -> 'ok'.
init_acct(Account) ->
    AccountDb = kzs_util:format_account_db(Account),
    AccountId = kzs_util:format_account_id(Account),

    lager:debug("init acdc account: ~s", [AccountId]),

    acdc_stats:init_db(AccountId),

    _ = init_acct_queues(AccountDb, AccountId),
    init_acct_agents(AccountDb, AccountId).

-spec init_acct_queues(kz_term:ne_binary()) -> any().
init_acct_queues(Account) ->
    AccountDb = kzs_util:format_account_db(Account),
    AccountId = kzs_util:format_account_id(Account),

    lager:debug("init acdc account queues: ~s", [AccountId]),
    init_acct_queues(AccountDb, AccountId).

-spec init_acct_agents(kz_term:ne_binary()) -> any().
init_acct_agents(Account) ->
    AccountDb = kzs_util:format_account_db(Account),
    AccountId = kzs_util:format_account_id(Account),

    lager:debug("init acdc account agents: ~s", [AccountId]),
    init_acct_agents(AccountDb, AccountId).

-spec init_acct_queues(kz_term:ne_binary(), kz_term:ne_binary()) -> any().
init_acct_queues(AccountDb, AccountId) ->
    init_queues(AccountId
               ,kz_datamgr:get_results(AccountDb, <<"queues/crossbar_listing">>, [])
               ).

-spec init_acct_agents(kz_term:ne_binary(), kz_term:ne_binary()) -> any().
init_acct_agents(AccountDb, AccountId) ->
    init_agents(AccountId
               ,kz_datamgr:get_results(AccountDb, <<"queues/agents_listing">>, [])
               ).

-spec init_queues(kz_term:ne_binary(), kazoo_data:get_results_return()) -> any().
init_queues(_, {'ok', []}) -> 'ok';
init_queues(AccountId, {'error', 'gateway_timeout'}) ->
    lager:debug("gateway timed out loading queues in account ~s, trying again in a moment", [AccountId]),
    try_queues_again(AccountId),
    wait_a_bit(),
    'ok';
init_queues(AccountId, {'error', 'not_found'}) ->
    lager:error("the queues view for ~s appears to be missing; you should probably fix that", [AccountId]);
init_queues(AccountId, {'error', _E}) ->
    lager:debug("error fetching queues: ~p", [_E]),
    try_queues_again(AccountId),
    wait_a_bit(),
    'ok';
init_queues(AccountId, {'ok', Qs}) ->
    acdc_stats:init_db(AccountId),
    [acdc_queues_sup:new(AccountId, kz_doc:id(Q)) || Q <- Qs].

-spec init_agents(kz_term:ne_binary(), kazoo_data:get_results_return()) -> any().
init_agents(_, {'ok', []}) -> 'ok';
init_agents(AccountId, {'error', 'gateway_timeout'}) ->
    lager:debug("gateway timed out loading agents in account ~s, trying again in a moment", [AccountId]),
    try_agents_again(AccountId),
    wait_a_bit(),
    'ok';
init_agents(AccountId, {'error', 'not_found'}) ->
    lager:error("the agents view for ~s appears to be missing; you should probably fix that", [AccountId]);
init_agents(AccountId, {'error', _E}) ->
    lager:debug("error fetching agents: ~p", [_E]),
    try_agents_again(AccountId),
    wait_a_bit(),
    'ok';
init_agents(AccountId, {'ok', As}) ->
    [spawn_previously_logged_in_agent(AccountId, kz_doc:id(A)) || A <- As].

wait_a_bit() -> timer:sleep(1000 + rand:uniform(500)).

try_queues_again(AccountId) ->
    try_again(AccountId, fun init_acct_queues/2).
try_agents_again(AccountId) ->
    try_again(AccountId, fun init_acct_agents/2).

try_again(AccountId, F) ->
    kz_process:spawn(
      fun() ->
              wait_a_bit(),
              AccountDb = kzs_util:format_account_db(AccountId),
              F(AccountDb, AccountId)
      end).

-spec spawn_previously_logged_in_agent(kz_term:ne_binary(), kz_term:ne_binary()) -> any().
spawn_previously_logged_in_agent(AccountId, AgentId) ->
    kz_process:spawn(
      fun() ->
              case acdc_agent_util:most_recent_status(AccountId, AgentId) of
                  {'ok', <<"logged_out">>} -> lager:debug("agent ~s in ~s is logged out, not starting", [AgentId, AccountId]);
                  {'ok', _Status} -> acdc_agents_sup:new(AccountId, AgentId)
              end
      end).

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_acdc_agent:declare_exchanges(),
    _ = kapi_acdc_queue:declare_exchanges(),
    _ = kapi_acdc_stats:declare_exchanges(),
    _ = kapi_call:declare_exchanges(),
    _ = kapi_conf:declare_exchanges(),
    _ = kapi_dialplan:declare_exchanges(),
    _ = kapi_notifications:declare_exchanges(),
    _ = kapi_resource:declare_exchanges(),
    _ = kapi_route:declare_exchanges(),
    _ = kapi_presence:declare_exchanges(),
    kapi_self:declare_exchanges().
