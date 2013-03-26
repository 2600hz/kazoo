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

-export([start_link/0
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
    [init_acct(Acct) || Acct <- whapps_util:get_all_accounts('encoded')].

-spec init_acct(ne_binary()) -> 'ok'.
init_acct(Acct) ->
    AcctDb = wh_util:format_account_id(Acct, 'encoded'),
    AcctId = wh_util:format_account_id(Acct, 'raw'),

    lager:debug("init acdc account: ~s", [AcctId]),

    init_queues(AcctId
                ,couch_mgr:get_results(AcctDb, <<"queues/crossbar_listing">>, [])
               ),
    init_agents(AcctId
                ,couch_mgr:get_results(AcctDb, <<"users/crossbar_listing">>, [])
               ).

init_acct_queues(Acct) ->
    AcctDb = wh_util:format_account_id(Acct, 'encoded'),
    AcctId = wh_util:format_account_id(Acct, 'raw'),

    lager:debug("init acdc account queues: ~s", [AcctId]),
    init_agents(AcctId
                ,couch_mgr:get_results(AcctDb, <<"queues/crossbar_listing">>, [])
               ).

init_acct_agents(Acct) ->
    AcctDb = wh_util:format_account_id(Acct, 'encoded'),
    AcctId = wh_util:format_account_id(Acct, 'raw'),

    lager:debug("init acdc account agents: ~s", [AcctId]),
    init_agents(AcctId
                ,couch_mgr:get_results(AcctDb, <<"users/crossbar_listing">>, [])
               ).

-spec init_queues(ne_binary(), {'ok', wh_json:objects()} | {'error', _}) -> any().
init_queues(_, {'ok', []}) -> 'ok';
init_queues(AcctId, {'error', 'gateway_timeout'}) ->
    lager:debug("gateway timed out loading queues in account ~s, trying again in a moment", [AcctId]),
    try_queues_again(AcctId),
    wait_a_bit(),
    'ok';
init_queues(AcctId, {'error', _E}) ->
    lager:debug("error fetching queues: ~p", [_E]),
    try_queues_again(AcctId),
    wait_a_bit(),
    'ok';
init_queues(AcctId, {'ok', Qs}) ->
    acdc_stats:init_db(AcctId),
    [acdc_queues_sup:new(AcctId, wh_json:get_value(<<"id">>, Q)) || Q <- Qs].

init_agents(_, {'ok', []}) -> 'ok';
init_agents(AcctId, {'error', 'gateway_timeout'}) ->
    lager:debug("gateway timed out loading agents in account ~s, trying again in a moment", [AcctId]),
    try_agents_again(AcctId),
    wait_a_bit(),
    'ok';
init_agents(AcctId, {'error', _E}) ->
    lager:debug("error fetching agents: ~p", [_E]),
    try_agents_again(AcctId),
    wait_a_bit(),
    'ok';
init_agents(AcctId, {'ok', As}) ->
    [acdc_agents_sup:new(AcctId, wh_json:get_value(<<"id">>, A)) || A <- As].

wait_a_bit() -> timer:sleep(1000 + random:uniform(500)).

try_queues_again(AcctId) -> try_again(AcctId, <<"queues/crossbar_listing">>).
try_agents_again(AcctId) -> try_again(AcctId, <<"users/crossbar_listing">>).

try_again(AcctId, View) ->
    spawn(fun() ->
                  put('callid', ?MODULE),
                  wait_a_bit(),
                  init_queues(AcctId, couch_mgr:get_results(wh_util:format_account_id(AcctId, 'encoded')
                                                            ,View, []
                                                           ))
          end).
