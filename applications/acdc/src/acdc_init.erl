%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%% Iterate over each account, find configured queues and configured
%%% agents, and start the attendant processes
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
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
start_link() -> spawn(?MODULE, 'init_acdc', []), 'ignore'.

-spec init_acdc() -> any().
init_acdc() ->
    put('callid', ?MODULE),
    case couch_mgr:get_all_results(?KZ_ACDC_DB, <<"acdc/accounts_listing">>) of
        {'ok', []} ->
            lager:debug("no accounts configured for acdc");
        {'ok', Accounts} ->
            [init_acct(wh_json:get_value(<<"key">>, Acct)) || Acct <- Accounts];
        {'error', 'not_found'} ->
            lager:debug("acdc db not found, initializing"),
            _ = init_db(),
            lager:debug("consider running acdc_maintenance:migrate() to enable acdc for already-configured accounts");
        {'error', _E} ->
            lager:debug("failed to query acdc db: ~p", [_E])
    end.

init_db() ->
    _ = couch_mgr:db_create(?KZ_ACDC_DB),
    _ = couch_mgr:revise_doc_from_file(?KZ_ACDC_DB, 'crossbar', <<"views/acdc.json">>).

-spec init_acct(ne_binary()) -> 'ok'.
init_acct(Acct) ->
    AcctDb = wh_util:format_account_id(Acct, 'encoded'),
    AcctId = wh_util:format_account_id(Acct, 'raw'),

    lager:debug("init acdc account: ~s", [AcctId]),

    acdc_stats:init_db(AcctId),

    init_queues(AcctId
                ,couch_mgr:get_results(AcctDb, <<"queues/crossbar_listing">>, [])
               ),
    init_agents(AcctId
                ,couch_mgr:get_results(AcctDb, <<"users/crossbar_listing">>, [])
               ).

-spec init_acct_queues(ne_binary()) -> any().
init_acct_queues(Acct) ->
    AcctDb = wh_util:format_account_id(Acct, 'encoded'),
    AcctId = wh_util:format_account_id(Acct, 'raw'),

    lager:debug("init acdc account queues: ~s", [AcctId]),
    init_agents(AcctId
                ,couch_mgr:get_results(AcctDb, <<"queues/crossbar_listing">>, [])
               ).

-spec init_acct_agents(ne_binary()) -> any().
init_acct_agents(Acct) ->
    AcctDb = wh_util:format_account_id(Acct, 'encoded'),
    AcctId = wh_util:format_account_id(Acct, 'raw'),

    lager:debug("init acdc account agents: ~s", [AcctId]),
    init_agents(AcctId
                ,couch_mgr:get_results(AcctDb, <<"users/crossbar_listing">>, [])
               ).

-spec init_queues(ne_binary(), couch_mgr:get_results_return()) -> any().
init_queues(_, {'ok', []}) -> 'ok';
init_queues(AcctId, {'error', 'gateway_timeout'}) ->
    lager:debug("gateway timed out loading queues in account ~s, trying again in a moment", [AcctId]),
    try_queues_again(AcctId),
    wait_a_bit(),
    'ok';
init_queues(AcctId, {'error', 'not_found'}) ->
    lager:error("the queues view for ~s appears to be missing; you should probably fix that", [AcctId]);
init_queues(AcctId, {'error', _E}) ->
    lager:debug("error fetching queues: ~p", [_E]),
    try_queues_again(AcctId),
    wait_a_bit(),
    'ok';
init_queues(AcctId, {'ok', Qs}) ->
    acdc_stats:init_db(AcctId),
    [acdc_queues_sup:new(AcctId, wh_json:get_value(<<"id">>, Q)) || Q <- Qs].

-spec init_agents(ne_binary(), couch_mgr:get_results_return()) -> any().
init_agents(_, {'ok', []}) -> 'ok';
init_agents(AcctId, {'error', 'gateway_timeout'}) ->
    lager:debug("gateway timed out loading agents in account ~s, trying again in a moment", [AcctId]),
    try_agents_again(AcctId),
    wait_a_bit(),
    'ok';
init_agents(AcctId, {'error', 'not_found'}) ->
    lager:error("the agents view for ~s appears to be missing; you should probably fix that", [AcctId]);
init_agents(AcctId, {'error', _E}) ->
    lager:debug("error fetching agents: ~p", [_E]),
    try_agents_again(AcctId),
    wait_a_bit(),
    'ok';
init_agents(AcctId, {'ok', As}) ->
    [acdc_agents_sup:new(AcctId, wh_json:get_value(<<"id">>, A)) || A <- As].

wait_a_bit() -> timer:sleep(1000 + random:uniform(500)).

try_queues_again(AcctId) ->
    try_again(AcctId, <<"queues/crossbar_listing">>, fun init_queues/2).
try_agents_again(AcctId) ->
    try_again(AcctId, <<"users/crossbar_listing">>, fun init_agents/2).

try_again(AcctId, View, F) ->
    spawn(fun() ->
                  put('callid', ?MODULE),
                  wait_a_bit(),
                  F(AcctId, couch_mgr:get_results(
                              wh_util:format_account_id(AcctId, 'encoded')
                              ,View, []
                             ))
          end).
