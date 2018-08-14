%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
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

-define(STATE_READY(Trie, RatedeckDb, CheckRef), {'ready', Trie, RatedeckDb, CheckRef}).

-define(CHECK_MSG(ExpiresS), {'check_trie', ExpiresS}).

-type state() :: ?STATE_READY(trie:trie(), kz_term:ne_binary(), reference()).
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
stop(<<_/binary>>=RatedeckId) ->
    ProcName = hon_trie:trie_proc_name(RatedeckId),
    case whereis(ProcName) of
        'undefined' -> 'ok';
        Pid -> catch gen_server:call(Pid, 'stop')
    end.

-spec cache_rates(kz_term:ne_binary(), kzd_rates:docs()) -> 'ok'.
cache_rates(RatedeckId, Rates) ->
    ProcName = hon_trie:trie_proc_name(RatedeckId),
    gen_server:call(ProcName, {'cache_rates', Rates}).

-spec init([kz_term:ne_binary() | pos_integer()]) -> {'ok', state()}.
init([RatedeckDb, ExpiresS]) ->
    kz_util:put_callid(hon_trie:trie_proc_name(RatedeckDb)),
    lager:info("starting LRU for ~s", [RatedeckDb]),
    {'ok', ?STATE_READY(trie:new(), RatedeckDb, start_expires_check_timer(ExpiresS))}.

-spec start_expires_check_timer(pos_integer()) -> reference().
-ifdef(PROPER).
start_expires_check_timer(ExpiresS) ->
    erlang:start_timer(1 * ?MILLISECONDS_IN_SECOND, self(), ?CHECK_MSG(ExpiresS)).
-else.
start_expires_check_timer(ExpiresS) ->
    Check = (ExpiresS div 2) * ?MILLISECONDS_IN_SECOND,
    erlang:start_timer(Check, self(), ?CHECK_MSG(ExpiresS)).
-endif.

-spec handle_call(any(), kz_term:pid_ref(), state()) ->
                         {'noreply', state()} |
                         {'reply', match_return(), state()}.
handle_call({'match_did', DID}, _From, ?STATE_READY(Trie, RatedeckDb, CheckRef)) ->
    {UpdatedTrie, Resp} = match_did_in_trie(DID, Trie),
    {'reply', Resp, ?STATE_READY(UpdatedTrie, RatedeckDb, CheckRef)};
handle_call({'cache_rates', Rates}, _From, ?STATE_READY(Trie, RatedeckDb, CheckRef)) ->
    UpdatedTrie = handle_caching_of_rates(Trie, Rates),
    {'reply', 'ok', ?STATE_READY(UpdatedTrie, RatedeckDb, CheckRef)};
handle_call('stop', _From, State) ->
    lager:debug("requested to stop by ~p", [_From]),
    {'stop', 'normal', State};
handle_call(_Req, _From, State) ->
    {'noreply', State}.

-spec handle_cast(any(), state()) -> {'noreply', state()}.
handle_cast(_Req, State) ->
    lager:debug("unhandled cast ~p", [_Req]),
    {'noreply', State}.

-spec handle_info(any(), state()) -> {'noreply', state()}.
handle_info({'timeout', CheckRef, ?CHECK_MSG(ExpiresS)}
           ,?STATE_READY(Trie, RatedeckDb, CheckRef)
           ) ->
    UpdatedTrie = check_expired_entries(Trie, ExpiresS),
    {'noreply', ?STATE_READY(UpdatedTrie, RatedeckDb, start_expires_check_timer(ExpiresS))};
handle_info(_Msg, State) ->
    lager:debug("unhandled message ~p: ~p",[_Msg, State]),
    {'noreply', State}.

-spec check_expired_entries(trie:trie(), pos_integer()) -> trie:trie().
check_expired_entries(Trie, ExpiresS) ->
    OldestMs = kz_time:now_ms() - (ExpiresS * 1000),
    {UpdatedTrie, OldestMs} =
        trie:foldl(fun check_if_expired/3
                  ,{Trie, OldestMs}
                  ,Trie
                  ),
    UpdatedTrie.

-spec check_if_expired(prefix(), [{kz_term:ne_binary(), pos_integer()}], {pid(), pos_integer()}) ->
                              {pid(), pos_integer()}.
check_if_expired(Prefix, Rates, {Trie, OldestTimestamp}=Acc) ->
    case expired_rates(Rates, OldestTimestamp) of
        [] -> Acc;
        [_|_]=_OldRates ->
            {trie:erase(Prefix, Trie), OldestTimestamp}
    end.

-spec expired_rates([{kz_term:ne_binary(), pos_integer()}], pos_integer()) ->
                           [] | [{kz_term:ne_binary(), pos_integer()}].
expired_rates(Rates, OldestTimestamp) ->
    [RateId
     || {RateId, LastUsed} <- Rates,
        LastUsed < OldestTimestamp
    ].

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, ?STATE_READY(_, _, _)) ->
    lager:info("terminating: ~p", [_Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_Vsn, State, _Extra) ->
    {'ok', State}.

-spec match_did_in_trie(string(), trie:trie()) -> {trie:trie(), match_return()}.
match_did_in_trie(DID, Trie) ->
    case trie:find_prefix_longest(DID, Trie) of
        'error' ->
            {Trie, {'error', 'not_found'}};
        {'ok', Prefix, RateIds} ->
            UpdatedTrie = bump_prefix_timestamp(Trie, Prefix, RateIds),
            {UpdatedTrie, {'ok', {Prefix, [Id || {Id, _Created} <- RateIds]}}}
    end.

-spec bump_prefix_timestamp(trie:trie(), string(), rate_entries()) -> trie:trie().
bump_prefix_timestamp(Trie, Prefix, RateIds) ->
    BumpedRateIds = [{Id, kz_time:now_ms()}
                     || {Id, _LastAccessed} <- RateIds
                    ],
    trie:store(Prefix, BumpedRateIds, Trie).

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
               ,fun(Rates) -> props:insert_value(Id, NowMs, Rates) end
               ,[{Id, NowMs}]
               ,Trie
               ).
