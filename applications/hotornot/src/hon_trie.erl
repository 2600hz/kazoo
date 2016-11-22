%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Rater whapp; send me a DID, get a rate back
%%%
%%% (Words * Bytes/Word) div (Prefixes) ~= Bytes per Prefix
%%% (1127494 * 8) div 78009 = 115
%%%
%%% Processing:
%%% timer:tc(hon_trie, match_did, [<<"53341341354">>]).
%%%  {132,...}
%%% timer:tc(hon_util, candidate_rates, [<<"53341341354">>]).
%%%  {16989,...}
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(hon_trie).
-behaviour(gen_server).

-export([start_link/0
        ,match_did/1
        ,rebuild/0
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-export([handle_db_update/2]).
-export([build_trie/1, build_trie/2]).

-include("hotornot.hrl").

-define(BUILD_TIMEOUT, kapps_config:get_integer(?APP_NAME, <<"trie_build_timeout_ms">>, ?MILLISECONDS_IN_MINUTE)).

-type state() :: {trie:trie() | 'undefined', api_pid()}.

-spec start_link() -> {'ok', pid()}.
start_link() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

-type match_return() :: {'error', any()} |
                        {'ok', {string(), ne_binaries()}}.
-spec match_did(ne_binary()) -> match_return().
match_did(ToDID) ->
    case gen_server:call(?MODULE, {'match_did', kz_util:to_list(ToDID)}) of
        {'error', 'not_found'} ->
            lager:debug("failed to find rate for ~s", [ToDID]),
            {'ok', []};
        {'error', E} ->
            lager:warning("failed to find rate for ~s, got error ~p", [ToDID, E]),
            {'error', E};
        {'ok', {_Prefix, RateIds}} ->
            lager:info("candidate rates for ~s: ~s ~p", [ToDID, _Prefix, RateIds]),
            load_rates(RateIds)
    end.

-spec rebuild() -> {'ok', pid()}.
rebuild() ->
    case gen_server:call(?MODULE, 'rebuild') of
        {'ok', Pid} ->
            lager:debug("rebuilding trie in ~p", [Pid]),
            {'ok', Pid};
        {'error', E} ->
            lager:warning("error rebilding trie ~p", [E]),
            {'error', E}
    end.

-spec load_rates(ne_binaries()) -> {'ok', kz_json:objects()}.
load_rates(RateIds) ->
    {'ok', lists:foldl(fun load_rate/2, [], RateIds)}.

-spec load_rate(ne_binary(), kz_json:objects()) -> kz_json:objects().
load_rate(RateId, Acc) ->
    case kz_datamgr:open_cache_doc(?KZ_RATES_DB, RateId) of
        {'ok', RateDoc} -> [RateDoc | Acc];
        {'error', _} -> Acc
    end.

-spec init([]) -> {'ok', state()}.
init([]) ->
    {Pid, _Ref} = spawn_monitor(?MODULE, 'build_trie', [self()]),
    _ = erlang:send_after(?BUILD_TIMEOUT, self(), {'build_timeout', Pid}),
    lager:debug("building trie in ~p", [Pid]),
    {'ok', {'undefined', Pid}}.

-spec handle_call(any(), pid_ref(), state()) ->
                         {'noreply', state()} |
                         {'reply', match_return(), state()}.
handle_call({'match_did', _DID}, _From, {'undefined', _Pid}=State) ->
    {'reply', {'error', 'no_trie'}, State};
handle_call({'match_did', DID}, _From, {Trie, _Pid}=State) ->
    case trie:find_prefix_longest(DID, Trie) of
        'error' -> {'reply', {'error', 'not_found'}, State};
        {'ok', Prefix, RateIds} -> {'reply', {'ok', {Prefix, RateIds}}, State}
    end;
handle_call('rebuild', _From, {Trie, 'undefined'}) ->
    {Pid, _Ref} = spawn_monitor(?MODULE, 'build_trie', [self()]),
    _ = erlang:send_after(?BUILD_TIMEOUT, self(), {'build_timeout', Pid}),
    {'reply', {'ok', Pid}, {Trie, Pid}};
handle_call('rebuild', _From, {_Trie, Pid}=State) ->
    {'reply', {'error', {'already_rebuild', Pid}}, State};
handle_call(_Req, _From, State) ->
    {'noreply', State}.

-spec handle_cast(any(), state()) -> {'noreply', state()}.
handle_cast({'trie', Pid, Trie}, {_, Pid}) ->
    lager:debug("trie built by ~p", [Pid]),
    {'noreply', {Trie, Pid}};
handle_cast(_Req, State) ->
    lager:info("unhandled cast ~p", [_Req]),
    {'noreply', State}.

-spec handle_info(any(), state()) -> {'noreply', state()}.
handle_info({'build_timeout', Pid}, {_, Pid}=State) ->
    lager:error("building trie took too long, killing pid ~p",[Pid]),
    erlang:exit(Pid, 'build_timeout'),
    {'noreply', State};
handle_info({'build_timeout', _Pid}, State) ->
    %% It's ok, build completed already
    {'noreply', State};
handle_info({'DOWN', _Ref, 'process', Pid, _Reason}, {Trie, Pid}) ->
    lager:debug("~p:~p down: ~p", [Pid, _Ref, _Reason]),
    {'noreply', {Trie, 'undefined'}};
handle_info(Msg, State) ->
    lager:info("unhandled message ~p",[Msg]),
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:info("terminating: ~p", [_Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_Vsn, State, _Extra) ->
    {'ok', State}.

-spec handle_db_update(kz_json:object(), kz_proplist()) -> 'ok'.
handle_db_update(JObj, _Props) ->
    'true' = kapi_conf:doc_update_v(JObj),
    {'ok', Pid} = gen_server:call(?MODULE, 'rebuild'),
    lager:debug("ratedeck DB changed, rebuilding trie in ~p", [Pid]).

-spec build_trie(pid()) -> 'ok'.
-spec build_trie(pid(), ne_binary()) -> 'ok'.
build_trie(Server) ->
    build_trie(Server, ?KZ_RATES_DB).

build_trie(Server, Database) ->
    build_trie(Server, Database, trie:new(), fetch_rates(Database, 0)).

-define(LIMIT, 5000).

-spec build_trie(pid(), ne_binary(), trie:trie(), kazoo_data:get_results_return()) ->
                        'ok'.
build_trie(Server, _Database, Trie, {'ok', []}) ->
    gen_server:cast(Server, {'trie', self(), Trie});
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
    Prefix = kz_util:to_list(kz_json:get_value(<<"key">>, Result)),
    trie:append(Prefix, Id, Trie).

-spec fetch_rates(ne_binary(), non_neg_integer()) ->
                         kazoo_data:get_results_return().
fetch_rates(Database, StartKey) ->
    Options = props:filter_undefined([{'startkey', StartKey}
                                     ,{'limit', ?LIMIT+1}
                                     ]
                                    ),
    kz_datamgr:get_results(Database, <<"rates/lookup">>, Options).
