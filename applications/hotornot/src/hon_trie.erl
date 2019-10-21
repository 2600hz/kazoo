%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Rater whapp; send me a DID, get a rate back.
%%% ```
%%% (Words * Bytes/Word) div (Prefixes) ~= Bytes per Prefix
%%% (1127494 * 8) div 78009 = 115
%%% '''
%%%
%%% Processing:
%%%
%%% ```
%%% timer:tc(hon_trie, match_did, [<<"53341341354">>]).
%%%  {132,...}
%%% timer:tc(hon_util, candidate_rates, [<<"53341341354">>]).
%%%  {16989,...}
%%% '''
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(hon_trie).
-behaviour(gen_server).

-export([start_link/1
        ,match_did/2, match_did/3
        ,rebuild/0
        ,trie_proc_name/1
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-export([handle_db_update/2]).
-export([build_trie/2]).

-include("hotornot.hrl").

-define(BUILT_TRIE(Builder, Trie)
       ,{'trie', Builder, Trie}
       ).

-define(STATE_READY(Trie, RatedeckDb), {'ready', Trie, RatedeckDb}).
-define(STATE_BUILDING(Trie, RatedeckDb, PidRef), {'building', Trie, RatedeckDb, PidRef}).

-type state() :: ?STATE_READY(trie:trie(), kz_term:ne_binary()) |
                 ?STATE_BUILDING(trie:trie() | 'undefined', kz_term:ne_binary(), kz_term:pid_ref()).

-spec start_link(kz_term:ne_binary()) -> {'ok', pid()}.
start_link(RatedeckDb) ->
    case hotornot_config:trie_module() of
        ?MODULE ->
            ProcName = trie_proc_name(RatedeckDb),
            gen_server:start_link({'local', ProcName}, ?MODULE, [RatedeckDb], []);
        'hon_trie_lru' ->
            hon_trie_lru:start_link(RatedeckDb)
    end.

-spec trie_proc_name(kz_term:ne_binary()) -> atom().
trie_proc_name(Ratedeck) ->
    RatedeckDb = kzd_ratedeck:format_ratedeck_db(Ratedeck),
    kz_term:to_atom(<<"hon_trie_", RatedeckDb/binary>>, 'true').

-ifdef(TEST).

-spec match_did(kz_term:ne_binary(), kz_term:api_ne_binary()) -> match_return().
match_did(ToDID, AccountId) ->
    match_did(ToDID, AccountId, ?KZ_RATES_DB).

-spec match_did(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> match_return().
match_did(ToDID, _AccountId, RatedeckId) ->
    ProcName = trie_proc_name(RatedeckId),

    case gen_server:call(ProcName, {'match_did', kz_term:to_list(ToDID)}) of
        {'error', _}=Error -> Error;
        {'ok', {_Prefix, RateIds}} -> {'ok', RateIds}
    end.
-else.

-spec match_did(kz_term:ne_binary(), kz_term:api_ne_binary()) -> match_return().
match_did(ToDID, AccountId) ->
    match_did(ToDID, AccountId, 'undefined').

-spec match_did(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> match_return().
match_did(ToDID, AccountId, RatedeckId) ->
    Ratedeck = hon_util:account_ratedeck(AccountId, RatedeckId),
    ProcName = trie_proc_name(Ratedeck),

    case gen_server:call(ProcName, {'match_did', kz_term:to_list(ToDID)}) of
        {'error', _}=Error -> Error;
        {'ok', {_Prefix, RateIds}} -> load_rates(Ratedeck, RateIds)
    end.

-spec load_rates(kz_term:ne_binary(), kz_term:ne_binaries()) -> {'ok', kzd_rates:docs()}.
load_rates(Ratedeck, RateIds) ->
    RatedeckDb = kzd_ratedeck:format_ratedeck_db(Ratedeck),
    {'ok', lists:foldl(fun(R, Acc) -> load_rate(R, Acc, RatedeckDb) end, [], RateIds)}.

-spec load_rate(kz_term:ne_binary(), kz_json:objects(), kz_term:ne_binary()) -> kzd_rates:docs().
load_rate(RateId, Acc, RatedeckDb) ->
    case kz_datamgr:open_cache_doc(RatedeckDb, RateId) of
        {'error', _} -> Acc;
        {'ok', RateDoc} ->
            [kzd_rates:set_ratedeck_id(RateDoc, kzd_ratedeck:format_ratedeck_id(RatedeckDb)) | Acc]
    end.

-endif.

-spec rebuild() -> {'ok', pid()}.
rebuild() ->
    case gen_server:call(?MODULE, 'rebuild') of
        {'ok', Pid} ->
            lager:debug("rebuilding trie in ~p", [Pid]),
            {'ok', Pid};
        {'error', E} ->
            lager:warning("error rebuilding trie ~p", [E]),
            {'error', E}
    end.

-spec init([kz_term:ne_binary()]) -> {'ok', state()}.
init([RatedeckDb]) ->
    kz_log:put_callid(trie_proc_name(RatedeckDb)),
    PidRef = start_builder(RatedeckDb),
    lager:debug("building trie for ~s in ~p", [RatedeckDb, PidRef]),
    {'ok', ?STATE_BUILDING('undefined', RatedeckDb, PidRef)}.

-spec start_builder(kz_term:ne_binary()) -> kz_term:pid_ref().
start_builder(RatedeckDb) ->
    PidRef = spawn_monitor(?MODULE, 'build_trie', [self(), RatedeckDb]),
    _ = erlang:send_after(hotornot_config:trie_build_timeout_ms(), self(), {'build_timeout', PidRef}),
    PidRef.

-spec handle_call(any(), kz_term:pid_ref(), state()) ->
                         {'noreply', state()} |
                         {'reply', match_return(), state()}.
handle_call({'match_did', _DID}, _From, ?STATE_BUILDING('undefined', _RatedeckDb, _PidRef)=State) ->
    {'reply', {'error', 'no_trie'}, State};
handle_call({'match_did', DID}, _From, ?STATE_BUILDING(Trie, _RatedeckDb, {_Pid, _Ref})=State) ->
    Resp = match_did_in_trie(DID, Trie),
    {'reply', Resp, State};
handle_call({'match_did', DID}, _From, ?STATE_READY(Trie, _RatedeckDb)=State) ->
    Resp = match_did_in_trie(DID, Trie),
    {'reply', Resp, State};

handle_call('rebuild', _From, ?STATE_BUILDING(_Trie, _RatedeckDb, {Pid, _Ref})=State) ->
    {'reply', {'error', {'rebuilding', Pid}}, State};
handle_call('rebuild', _From, ?STATE_READY(Trie, RatedeckDb)) ->
    {Pid, _} = PidRef = start_builder(RatedeckDb),
    {'reply', {'ok', Pid}, ?STATE_BUILDING(Trie, RatedeckDb, PidRef)};
handle_call(_Req, _From, State) ->
    {'noreply', State}.

-spec handle_cast(any(), state()) -> {'noreply', state()}.
handle_cast(?BUILT_TRIE(Pid, Trie), ?STATE_BUILDING(_OldTrie, RatedeckDb, {Pid, Ref})) ->
    lager:debug("trie with ~p prefixes built by ~p", [trie:size(Trie), Pid]),
    erlang:demonitor(Ref, ['flush']),
    {'noreply', ?STATE_READY(Trie, RatedeckDb)};
handle_cast(_Req, State) ->
    lager:info("unhandled cast ~p", [_Req]),
    {'noreply', State}.

-spec handle_info(any(), state()) -> {'noreply', state()}.
handle_info({'build_timeout', PidRef}, ?STATE_BUILDING('undefined', RatedeckDb, {Pid, Ref}=PidRef)) ->
    lager:error("building the initial trie took too long, killing pid ~p", [Pid]),
    erlang:demonitor(Ref, ['flush']),
    erlang:exit(Pid, 'build_timeout'),

    {NewPid, _} = NewPidRef = start_builder(RatedeckDb),
    lager:debug("starting new builder at ~p", [NewPid]),

    {'noreply', ?STATE_BUILDING('undefined', RatedeckDb, NewPidRef)};
handle_info({'build_timeout', PidRef}, ?STATE_BUILDING(Trie, RatedeckDb, {Pid, Ref}=PidRef)) ->
    lager:error("building a new trie took too long, killing pid ~p", [Pid]),
    erlang:demonitor(Ref, ['flush']),
    erlang:exit(Pid, 'build_timeout'),
    {'noreply', ?STATE_READY(Trie, RatedeckDb)};
handle_info({'build_timeout', _PidRef}, ?STATE_READY(_Trie, _RatedeckDb)=State) ->
    %% It's ok, build completed already
    {'noreply', State};
handle_info({'DOWN', Ref, 'process', Pid, _Reason}, ?STATE_BUILDING(Trie, RatedeckDb, {Pid, Ref})) ->
    lager:debug("rebuild proc ~p:~p down: ~p", [Pid, Ref, _Reason]),
    {'noreply', ?STATE_READY(Trie, RatedeckDb)};
handle_info({'DOWN', _Ref, 'process', _Pid, _Reason}, ?STATE_READY(_Trie, _RatedeckDb)=State) ->
    {'norply', State};
handle_info(Msg, State) ->
    lager:info("unhandled message ~p",[Msg]),
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, ?STATE_BUILDING('undefined', _, _)) ->
    lager:info("terminating while building initial trie: ~p", [_Reason]);
terminate(_Reason, ?STATE_BUILDING(_, _, _)) ->
    lager:info("terminating while building new trie: ~p", [_Reason]);
terminate(_Reason, ?STATE_READY(_, _)) ->
    lager:info("terminating: ~p", [_Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_Vsn, State, _Extra) ->
    {'ok', State}.

-spec handle_db_update(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_db_update(ConfUpdate, _Props) ->
    'true' = kapi_conf:doc_update_v(ConfUpdate),

    process_conf_update(ConfUpdate, kapi_conf:get_type(ConfUpdate)).

process_conf_update(ConfUpdate, <<"database">>) ->
    lager:debug("conf update: ~p", [ConfUpdate]),
    process_db_update(kapi_conf:get_database(ConfUpdate)
                     ,kz_api:event_name(ConfUpdate)
                     );
process_conf_update(_ConfUpdate, _Type) ->
    lager:debug("ignoring conf update to ~s: ~p", [_Type, _ConfUpdate]).

-spec process_db_update(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
process_db_update(?KZ_RATES_DB=RatedeckId, ?DB_EDITED) ->
    {'ok', Pid} = gen_server:call(trie_proc_name(RatedeckId), 'rebuild'),
    lager:info("ratedeck ~s changed, rebuilding trie in ~p", [Pid]);
process_db_update(?KZ_RATES_DB=RatedeckId, ?DB_DELETED) ->
    Proc = trie_proc_name(RatedeckId),
    _ = hon_tries_sup:stop_trie(Proc),
    lager:info("ratedeck ~s deleted, stopping the trie at ~p", [RatedeckId, Proc]);
process_db_update(?MATCH_RATEDECK_DB_ENCODED(_)=RatedeckDb, ?DB_CREATED) ->
    maybe_start_trie_server(RatedeckDb);
process_db_update(?MATCH_RATEDECK_DB_ENCODED(_)=RatedeckDb, ?DB_EDITED) ->
    {'ok', Pid} = gen_server:call(trie_proc_name(RatedeckDb), 'rebuild'),
    lager:info("ratedeck ~s changed, rebuilding trie in ~p", [RatedeckDb, Pid]);
process_db_update(?MATCH_RATEDECK_DB_ENCODED(_)=RatedeckDb, ?DB_DELETED) ->
    Pid = trie_proc_name(RatedeckDb),
    _ = hon_tries_sup:stop_trie(Pid),
    lager:info("ratedeck ~s deleted, stopping the trie at ~p", [RatedeckDb, Pid]);
process_db_update(_Db, _Action) ->
    lager:debug("ignoring ~s: ~s", [_Db, _Action]).

-spec maybe_start_trie_server(kz_term:ne_binary()) -> 'ok'.
maybe_start_trie_server(RatedeckDb) ->
    case hon_tries_sup:start_trie(RatedeckDb) of
        {'ok', Pid} ->
            lager:info("starting new trie for ratedeck ~s in ~p", [RatedeckDb, Pid]);
        {'error', {'already_started', Pid}} ->
            lager:info("trie running for ratedeck ~s in ~p", [RatedeckDb, Pid]);
        {'error', 'already_present'} ->
            {'ok', Pid} = hon_tries_sup:restart_trie(RatedeckDb),
            lager:info("restarted trie for ratedeck ~s in ~p", [RatedeckDb, Pid]);
        {'error', _E} ->
            lager:debug("failed to start trie for ~s: ~p", [RatedeckDb, _E])
    end.

-spec build_trie(pid(), kz_term:ne_binary()) -> 'ok'.
build_trie(Server, Database) ->
    build_trie(Server, Database, trie:new(), fetch_rates(Database, 0)).

-define(LIMIT, 5000).

-spec build_trie(pid(), kz_term:ne_binary(), trie:trie(), kazoo_data:get_results_return()) ->
                        'ok'.
build_trie(Server, _Database, Trie, {'ok', []}) ->
    gen_server:cast(Server, ?BUILT_TRIE(self(), Trie));
build_trie(Server, Database, Trie, {'ok', JObjs}) ->
    try lists:split(?LIMIT, JObjs) of
        {Results, []} ->
            build_trie(Server, Database, add_results(Trie, Results), {'ok', []});
        {Results, [Next]} ->
            build_trie(Server, Database, add_results(Trie, Results), fetch_rates(Database, next_key(Next)))
    catch
        'error':'badarg' ->
            build_trie(Server, Database, add_results(Trie, JObjs), {'ok', []})
    end.

-spec next_key(kz_json:object()) -> non_neg_integer().
next_key(Result) ->
    kz_json:get_integer_value(<<"key">>, Result).

-spec add_results(trie:trie(), kz_json:objects()) -> trie:trie().
add_results(Trie, Results) ->
    lists:foldl(fun add_result/2, Trie, Results).

-spec add_result(kz_json:object(), trie:trie()) -> trie:trie().
add_result(Result, Trie) ->
    Id = kz_doc:id(Result),
    Prefix = kz_term:to_list(kz_json:get_value(<<"key">>, Result)),
    trie:append(Prefix, Id, Trie).

-spec fetch_rates(kz_term:ne_binary(), non_neg_integer()) ->
                         kazoo_data:get_results_return().
fetch_rates(Database, StartKey) ->
    Options = props:filter_undefined([{'startkey', StartKey}
                                     ,{'limit', ?LIMIT+1}
                                     ]
                                    ),
    kz_datamgr:get_results(Database, <<"rates/lookup">>, Options).

-spec match_did_in_trie(string(), trie:trie()) -> match_return().
match_did_in_trie(DID, Trie) ->
    case trie:find_prefix_longest(DID, Trie) of
        'error' -> {'error', 'not_found'};
        {'ok', Prefix, RateIds} -> {'ok', {Prefix, RateIds}}
    end.
