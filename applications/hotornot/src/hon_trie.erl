%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
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
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-export([build_trie/1, build_trie/2]).

-include("hotornot.hrl").

-type state() :: trie:trie() | pid_ref().

-spec start_link() -> {'ok', pid()}.
start_link() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

-type match_return() :: {'error', 'not_found'} |
                        {'ok', {string(), ne_binaries()}}.
-spec match_did(ne_binary()) -> {'ok', kz_json:objects()}.
match_did(ToDID) ->
    case gen_server:call(?MODULE, {'match_did', kz_term:to_list(ToDID)}) of
        {'error', 'not_found'} ->
            lager:debug("failed to find rate for ~s", [ToDID]),
            {'ok', []};
        {'ok', {_Prefix, RateIds}} ->
            lager:info("candidate rates for ~s: ~s ~p", [ToDID, _Prefix, RateIds]),
            load_rates(RateIds)
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
    PidRef = spawn_monitor(?MODULE, 'build_trie', [self()]),
    lager:debug("building trie in ~p", [PidRef]),
    {'ok', PidRef}.

-spec handle_call(any(), pid_ref(), state()) ->
                         {'noreply', state()} |
                         {'reply', match_return(), state()}.
handle_call({'match_did', DID}, _From, State) ->
    case trie:find_prefix_longest(DID, State) of
        'error' -> {'reply', {'error', 'not_found'}, State};
        {'ok', Prefix, RateIds} -> {'reply', {'ok', {Prefix, RateIds}}, State}
    end;
handle_call(_Req, _From, State) ->
    {'noreply', State}.

-spec handle_cast(any(), state()) -> {'noreply', state()}.
handle_cast({'trie', Pid, Trie}, {Pid, _Ref}) ->
    lager:debug("trie built by ~p", [Pid]),
    {'noreply', Trie};
handle_cast(_Req, State) ->
    {'noreply', State}.

-spec handle_info(any(), state()) -> {'noreply', state()}.
handle_info({'DOWN', _Ref, 'process', _Pid, _Reason}
           ,State
           ) ->
    lager:debug("~p:~p down: ~p", [_Pid, _Ref, _Reason]),
    {'noreply', State};
handle_info(_Msg, State) ->
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:info("terminating: ~p", [_Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_Vsn, State, _Extra) ->
    {'ok', State}.

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
    Prefix = kz_term:to_list(kz_json:get_value(<<"key">>, Result)),
    trie:append(Prefix, Id, Trie).

-spec fetch_rates(ne_binary(), non_neg_integer()) ->
                         kazoo_data:get_results_return().
fetch_rates(Database, StartKey) ->
    Options = props:filter_undefined([{'startkey', StartKey}
                                     ,{'limit', ?LIMIT+1}
                                     ]
                                    ),
    kz_datamgr:get_results(Database, <<"rates/lookup">>, Options).
