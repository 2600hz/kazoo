-module(eradius_counter_aggregator).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, reset/0, pull/0, read/0]).

-include("eradius_lib.hrl").

-define(INIT_HB, 1000).
-define(INTERVAL_HB, 5000).

-record(state, {
          me    :: reference(),
          reset :: erlang:timestamp()
        }).

%% @doc reset all counters to zero
reset() ->
    gen_server:call(?MODULE, reset).
%% @doc read counters and reset to zero
-spec pull() -> eradius_counter:stats().
pull() ->
    gen_server:call(?MODULE, pull).
%% @doc read counters
-spec read() -> eradius_counter:stats().
read() ->
    gen_server:call(?MODULE, read).

%% @private
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------------------------------
%% -- gen_server Callbacks
%% @private
init([]) ->
    ets:new(?MODULE, [ordered_set, protected, named_table, {keypos, #nas_counter.key}, {write_concurrency,true}]),
    eradius:modules_ready([?MODULE]),
    erlang:send_after(?INIT_HB, self(), heartbeat),
    {ok, #state{me = make_ref(), reset = now()}}.

%% @private
handle_call(pull, _From, State) ->
    Nass = read_stats(State),
    Servers = server_stats(pull),
    ets:delete_all_objects(?MODULE),
    {reply, {Servers, Nass}, State#state{reset = now()}};
handle_call(read, _From, State) ->
    Nass = read_stats(State),
    Servers = server_stats(read),
    {reply, {Servers, Nass}, State};
handle_call(reset, _From, State) ->
    server_stats(reset),
    ets:delete_all_objects(?MODULE),
    {reply, ok, State#state{reset = now()}}.

%% @private
handle_info(heartbeat, State) ->
    eradius_counter:collect(State#state.me, self()),
    erlang:send_after(?INTERVAL_HB, self(), heartbeat),
    {noreply, State};
handle_info({collect, Ref, Stats}, State = #state{me = Ref}) ->
    lists:foreach(fun update_stats/1, Stats),
    {noreply, State};
handle_info({collect, Ref, Stats}, State) ->
    io:format("invalid stats answer: ~p~n", [{collect, Ref, Stats}]),
    {noreply, State}.

%% -- unused callbacks
%% @private
handle_cast(_Msg, State)            -> {noreply, State}.
%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.
%% @private
terminate(_Reason, _State)           -> ok.

%% ------------------------------------------------------------------------------------------
%% -- helper functions
%% @private

read_stats(State) ->
    {State#state.reset, ets:tab2list(?MODULE)}.

server_stats(Func) ->
    lists:foldl(fun(S, Acc) -> [eradius_server:stats(S, Func)|Acc] end, [], eradius_server_sup:all()).

update_stats(Rec = #nas_counter{key = Key}) ->
    Cnt0 = case ets:lookup(?MODULE, Key) of
               [] -> #nas_counter{key = Key};
               [Cnt] -> Cnt
    end,
    ets:insert(?MODULE, add_counter(Cnt0, Rec)).

add_counter(Cnt1 = #nas_counter{}, Cnt2 = #nas_counter{}) ->
    #nas_counter{
             key                      = Cnt1#nas_counter.key,
             requests                 = Cnt1#nas_counter.requests                  + Cnt2#nas_counter.requests,
             replies                  = Cnt1#nas_counter.replies                   + Cnt2#nas_counter.replies,
             dupRequests              = Cnt1#nas_counter.dupRequests               + Cnt2#nas_counter.dupRequests,
             malformedRequests        = Cnt1#nas_counter.malformedRequests         + Cnt2#nas_counter.malformedRequests,
             accessRequests           = Cnt1#nas_counter.accessRequests            + Cnt2#nas_counter.accessRequests,
             accessAccepts            = Cnt1#nas_counter.accessAccepts             + Cnt2#nas_counter.accessAccepts,
             accessRejects            = Cnt1#nas_counter.accessRejects             + Cnt2#nas_counter.accessRejects,
             accessChallenges         = Cnt1#nas_counter.accessChallenges          + Cnt2#nas_counter.accessChallenges,
             accountRequests          = Cnt1#nas_counter.accountRequests           + Cnt2#nas_counter.accountRequests,
             accountResponses         = Cnt1#nas_counter.accountResponses          + Cnt2#nas_counter.accountResponses,
             noRecords                = Cnt1#nas_counter.noRecords                 + Cnt2#nas_counter.noRecords,
             badAuthenticators        = Cnt1#nas_counter.badAuthenticators         + Cnt2#nas_counter.badAuthenticators,
             packetsDropped           = Cnt1#nas_counter.packetsDropped            + Cnt2#nas_counter.packetsDropped,
             unknownTypes             = Cnt1#nas_counter.unknownTypes              + Cnt2#nas_counter.unknownTypes,
             handlerFailure           = Cnt1#nas_counter.handlerFailure            + Cnt2#nas_counter.handlerFailure,
             coaRequests              = Cnt1#nas_counter.coaRequests               + Cnt2#nas_counter.coaRequests,
             coaAcks                  = Cnt1#nas_counter.coaAcks                   + Cnt2#nas_counter.coaAcks,
             coaNaks                  = Cnt1#nas_counter.coaNaks                   + Cnt2#nas_counter.coaNaks,
             discRequests             = Cnt1#nas_counter.discRequests              + Cnt2#nas_counter.discRequests,
             discAcks                 = Cnt1#nas_counter.discAcks                  + Cnt2#nas_counter.discAcks,
             discNaks                 = Cnt1#nas_counter.discNaks                  + Cnt2#nas_counter.discNaks
            }.
