%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Trie LRU.
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
%%% Progressively load rates instead of seeding from the database.
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(hon_trie_lru).
-behaviour(gen_server).

-export([start_link/1, start_link/2
        ,stop/1
        ,cache_rates/2
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("hotornot.hrl").

-record(state, {trie = trie:new() :: trie:trie()
               ,ratedeck_db :: kz_term:api_ne_binary()
               ,check_ref :: reference()
               ,start_time = kz_time:start_time() :: kz_time:start_time()
               ,expires_s :: pos_integer()
               }).
-define(STATE_READY(Trie, RatedeckDb, CheckRef)
       ,#state{trie=Trie
              ,ratedeck_db=RatedeckDb
              ,check_ref=CheckRef
              }
       ).

-define(CHECK_MSG(ExpiresS), {'check_trie', ExpiresS}).

-type state() :: #state{}.
-type rate_entry() :: {kz_term:ne_binary(), kz_time:gregorian_seconds()}.
-type rate_entries() :: [rate_entry()].

-spec start_link(kz_term:ne_binary()) -> {'ok', pid()}.
start_link(RatedeckDb) ->
    start_link(RatedeckDb, hotornot_config:lru_expires_s()).

-spec start_link(kz_term:ne_binary(), pos_integer()) -> {'ok', pid()}.
start_link(RatedeckDb, ExpiresS) ->
    ProcName = hon_trie:trie_proc_name(RatedeckDb),
    gen_server:start_link({'local', ProcName}, ?MODULE, [RatedeckDb, ExpiresS], []).

-spec stop(kz_term:ne_binary()) -> 'ok'.
stop(<<RatedeckId/binary>>) ->
    ProcName = hon_trie:trie_proc_name(RatedeckId),
    case whereis(ProcName) of
        'undefined' -> 'ok';
        Pid -> catch gen_server:cast(Pid, 'stop')
    end.

-spec cache_rates(kz_term:ne_binary(), kzd_rates:docs()) -> 'ok'.
cache_rates(RatedeckId, Rates) ->
    ProcName = hon_trie:trie_proc_name(RatedeckId),
    gen_server:call(ProcName, {'cache_rates', Rates}).

-spec init([kz_term:ne_binary() | pos_integer()]) -> {'ok', state()}.
init([RatedeckDb, ExpiresS]) ->
    kz_log:put_callid(hon_trie:trie_proc_name(RatedeckDb)),
    lager:debug("starting LRU for ~s", [RatedeckDb]),
    {'ok', #state{trie=trie:new()
                 ,ratedeck_db=RatedeckDb
                 ,check_ref=start_expires_check_timer(ExpiresS)
                 ,expires_s=ExpiresS
                 }
    }.

-spec start_expires_check_timer(pos_integer()) -> reference().
start_expires_check_timer(ExpiresS) ->
    Check = (ExpiresS * ?MILLISECONDS_IN_SECOND) div 3,
    erlang:start_timer(Check, self(), ?CHECK_MSG(ExpiresS)).

-spec handle_call(any(), kz_term:pid_ref(), state()) ->
          {'noreply', state()} |
          {'reply', 'ok' | match_return(), state()}.
handle_call({'match_did', DID}, _From, ?STATE_READY(Trie, RatedeckDb, CheckRef)=State) ->
    io:format("~pms elapsed~n", [kz_time:elapsed_ms(State#state.start_time)]),
    {UpdatedTrie, Resp} = match_did_in_trie(DID, Trie, State#state.expires_s),
    io:format("  matched ~p: ~p~n  trie: ~p~n", [DID, Resp, trie:to_list(UpdatedTrie)]),
    {'reply', Resp, State?STATE_READY(UpdatedTrie, RatedeckDb, CheckRef)};
handle_call({'cache_rates', Rates}, _From, ?STATE_READY(Trie, RatedeckDb, CheckRef)=State) ->
    io:format("~pms elapsed~n", [kz_time:elapsed_ms(State#state.start_time)]),
    UpdatedTrie = handle_caching_of_rates(Trie, Rates),
    io:format("  after caching rates: ~p~n", [trie:to_list(UpdatedTrie)]),
    {'reply', 'ok', State?STATE_READY(UpdatedTrie, RatedeckDb, CheckRef)};
handle_call(_Req, _From, State) ->
    {'noreply', State}.

-spec handle_cast(any(), state()) -> {'noreply', state()}.
handle_cast('stop', State) ->
    {'stop', 'normal', State};
handle_cast(_Req, State) ->
    lager:debug("unhandled cast ~p", [_Req]),
    {'noreply', State}.

-spec handle_info(any(), state()) -> {'noreply', state()}.
handle_info({'timeout', CheckRef, ?CHECK_MSG(ExpiresS)}
           ,?STATE_READY(Trie, RatedeckDb, CheckRef)=State
           ) ->
    io:format("~pms elapsed~n", [kz_time:elapsed_ms(State#state.start_time)]),
    UpdatedTrie = check_expired_entries(Trie, ExpiresS),
    io:format("  updated trie: ~p~n", [trie:to_list(UpdatedTrie)]),
    {'noreply', State?STATE_READY(UpdatedTrie, RatedeckDb, start_expires_check_timer(ExpiresS))};
handle_info(_Msg, State) ->
    lager:debug("unhandled message ~p: ~p",[_Msg, State]),
    {'noreply', State}.

-spec check_expired_entries(trie:trie(), pos_integer()) -> trie:trie().
check_expired_entries(Trie, ExpiresS) ->
    {UpdatedTrie, _OldestMs} =
        trie:foldl(fun check_if_expired/3
                  ,{Trie, oldest_ms(ExpiresS)}
                  ,Trie
                  ),
    UpdatedTrie.

-spec oldest_ms(pos_integer()) -> pos_integer().
oldest_ms(ExpiresS) ->
    kz_time:now_ms() - (ExpiresS * ?MILLISECONDS_IN_SECOND).

-spec check_if_expired(prefix(), [{kz_term:ne_binary(), pos_integer()}], {pid(), pos_integer()}) ->
          {pid(), pos_integer()}.
check_if_expired(Prefix, Rates, {Trie, OldestTimestamp}=Acc) ->
    case has_expired_rates(Rates, OldestTimestamp) of
        'false' -> Acc;
        'true' ->
            {trie:erase(Prefix, Trie), OldestTimestamp}
    end.

-spec has_expired_rates([{kz_term:ne_binary(), pos_integer()}], pos_integer()) ->
          boolean().
has_expired_rates([], _) -> 'false';
has_expired_rates([{_RateId, LastUsed}|Rates], OldestTimestamp) ->
    case LastUsed < OldestTimestamp of
        'true' ->
            io:format("  rate ~s last used ~p after cutoff~n", [_RateId, OldestTimestamp-LastUsed]),
            'true';
        'false' ->
            has_expired_rates(Rates, OldestTimestamp)
    end.

-spec terminate(any(), state()) -> 'ok'.
terminate('normal', _State) -> 'ok';
terminate('shutdown', _State) -> 'ok';
terminate(_Reason, _State) ->
    lager:info("terminating: ~p", [_Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_Vsn, State, _Extra) ->
    {'ok', State}.

-spec match_did_in_trie(string(), trie:trie(), pos_integer()) -> {trie:trie(), match_return()}.
match_did_in_trie(DID, Trie, ExpiresS) ->
    case trie:find_prefix_longest(DID, Trie) of
        'error' ->
            {Trie, {'error', 'not_found'}};
        {'ok', Prefix, RateIds} ->
            OldestMs = oldest_ms(ExpiresS),
            UpdatedTrie = bump_prefix_timestamp(Trie, Prefix, RateIds, OldestMs),

            case build_resp(Prefix, RateIds, OldestMs) of
                {'error', 'not_found'} -> match_did_in_trie(DID, UpdatedTrie, ExpiresS);
                Resp -> {UpdatedTrie, Resp}
            end
    end.

build_resp(Prefix, RateIds, OldestMs) ->
    case [Id || {Id, LastUsed} <- RateIds, LastUsed >= OldestMs] of
        [] ->  {'error', 'not_found'};
        IDs -> {'ok', {Prefix, IDs}}
    end.

-spec bump_prefix_timestamp(trie:trie(), string(), rate_entries(), pos_integer()) -> trie:trie().
bump_prefix_timestamp(Trie, Prefix, RateIds, OldestMs) ->
    case [{Id, kz_time:now_ms()}
          || {Id, LastAccessed} <- RateIds,
             LastAccessed >= OldestMs
         ]
    of
        [] ->
            io:format("  erasing prefix ~p from trie~n", [Prefix]),
            trie:erase(Prefix, Trie);
        BumpedRateIds ->
            trie:store(Prefix, BumpedRateIds, Trie)
    end.

-spec handle_caching_of_rates(trie:trie(), kzd_rates:docs()) -> trie:trie().
handle_caching_of_rates(Trie, Rates) ->
    lists:foldl(fun cache_rate/2, Trie, Rates).

-spec cache_rate(kzd_rates:doc(), trie:trie()) -> trie:trie().
cache_rate(Rate, Trie) ->
    Id = kz_doc:id(Rate),
    Prefix = kz_term:to_list(kzd_rates:prefix(Rate)),
    NowMs = kz_time:now_ms(),
    lager:debug("caching ~s for prefix ~s at ~p into ~p~n", [Id, Prefix, NowMs, Trie]),
    trie:update(Prefix
               ,fun(Rates) -> props:set_value(Id, NowMs, Rates) end
               ,[{Id, NowMs}]
               ,Trie
               ).
