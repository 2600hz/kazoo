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

-spec start_link() -> 'ignore'.
start_link() ->
    spawn(?MODULE, init_acdc, []),
    ignore.

-spec init_acdc() -> any().
init_acdc() ->
    [init_account(Acct) || Acct <- whapps_util:get_all_accounts(encoded)].

-spec init_account(ne_binary()) -> 'ok'.
init_account(AcctDb) ->
    lager:debug("init account: ~s", [AcctDb]),

    init_queues((AcctId = wh_util:format_account_id(AcctDb, raw))
                ,couch_mgr:get_results(AcctDb, <<"queues/crossbar_listing">>, [])
               ),
    init_agents(AcctId
                ,couch_mgr:get_results(AcctDb, <<"agents/crossbar_listing">>, [])
               ).

-spec init_queues(ne_binary(), {'ok', wh_json:objects()} | {'error', _}) -> any().
init_queues(_, {ok, []}) -> ok;
init_queues(_, {error, _E}) -> lager:debug("error fetching queues: ~p", [_E]);
init_queues(AcctId, {ok, Qs}) ->
    acdc_stats:init_db(AcctId),
    [acdc_queues_sup:new(AcctId, wh_json:get_value(<<"id">>, Q)) || Q <- Qs].

init_agents(_, {ok, []}) -> ok;
init_agents(_, {error, _E}) -> lager:debug("error fetching agents: ~p", [_E]);
init_agents(AcctId, {ok, As}) ->
    [acdc_agents_sup:new(AcctId, wh_json:get_value(<<"id">>, A)) || A <- As].
