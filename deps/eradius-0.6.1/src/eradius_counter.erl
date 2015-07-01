%% @doc
%%  This module implements the statitics counter for RADIUS servers and clients

-module(eradius_counter).
-export([init_counter/1, inc_counter/2, reset_counter/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, reset/0, pull/0, read/0, aggregate/1]).
-export([collect/2]).

-include("eradius_lib.hrl").

-record(state, {
         reset :: erlang:timestamp()
        }).

-type srv_counters() :: [#server_counter{}].
-type nas_counters() :: {erlang:timestamp(), [#nas_counter{}]}.
-type stats() :: {srv_counters(), nas_counters()}.

%% ------------------------------------------------------------------------------------------
%% API

%% @doc initialize a counter structure
init_counter(Key = {_ServerIP, ServerPort}) when is_integer(ServerPort) ->
	#server_counter{key = Key, upTime = now(), resetTime = now()};
init_counter(#nas_prop{server_ip = ServerIP, server_port = ServerPort, nas_ip = NasIP}) ->
    #nas_counter{key = {{ServerIP, ServerPort}, NasIP}};
init_counter({{ServerIP, ServerPort}, NasIP})
  when is_tuple(ServerIP), is_integer(ServerPort), is_tuple(NasIP) ->
    #nas_counter{key = {{ServerIP, ServerPort}, NasIP}}.

%% @doc reset counters
reset_counter(#server_counter{upTime = Up}) -> #server_counter{upTime = Up, resetTime = now()};
reset_counter(Nas = #nas_prop{}) ->
    init_counter(Nas).

%% @doc increment a specific counter value
inc_counter(invalidRequests,  Counters = #server_counter{invalidRequests  = Value}) -> Counters#server_counter{invalidRequests  = Value + 1};
inc_counter(discardNoHandler, Counters = #server_counter{discardNoHandler = Value}) -> Counters#server_counter{discardNoHandler = Value + 1};

inc_counter(Counter, Nas = #nas_prop{}) ->
    gen_server:cast(?MODULE, {inc_counter, Counter, Nas}).

%% @doc reset all counters to zero
reset() ->
    gen_server:call(?MODULE, reset).
%% @doc read counters and reset to zero
-spec pull() -> stats().
pull() ->
    gen_server:call(?MODULE, pull).
%% @doc read counters
-spec read() -> stats().
read() ->
    gen_server:call(?MODULE, read).

%% @doc calculate the per server sum of all counters of a per NAS list of counters
-spec aggregate(stats()) -> stats().
aggregate({Servers, {ResetTS, Nass}}) ->
    NSums = lists:foldl(fun(Nas = #nas_counter{key = {ServerId, _}}, Acc) ->
                                orddict:update(ServerId, fun(Value) -> add_counter(Value, Nas) end, Nas#nas_counter{key = ServerId}, Acc)
                        end,
                        orddict:new(), Nass),
    NSum1 = [Value || {_Key, Value} <- orddict:to_list(NSums)],
    {Servers, {ResetTS, NSum1}}.

%% helper to be called from the aggregator to fetch this nodes values
%% @private
collect(Ref, Process) ->
    lists:foreach(fun(Node) -> gen_server:cast({?MODULE, Node}, {collect, Ref, Process}) end,
                  eradius_node_mon:get_module_nodes(?MODULE)).

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
    {ok, #state{reset = now()}}.

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
handle_cast({inc_counter, Counter, Nas = #nas_prop{server_ip = ServerIP, server_port = ServerPort, nas_ip = NasIP}}, State) ->
    Key = {{ServerIP, ServerPort}, NasIP},
    Cnt0 = case ets:lookup(?MODULE, Key) of
               [] -> init_counter(Nas);
               [Cnt] -> Cnt
    end,
    ets:insert(?MODULE, do_inc_counter(Counter, Cnt0)),
    {noreply, State};
handle_cast({collect, Ref, Process}, State) ->
    Process ! {collect, Ref, ets:tab2list(?MODULE)},
    ets:delete_all_objects(?MODULE),
    {noreply, State#state{reset = now()}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% -- unused callbacks
%% @private
handle_info(_Info, State)           -> {noreply, State}.
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

%% @private
do_inc_counter(requests,                Counters = #nas_counter{requests = Value})                -> Counters#nas_counter{requests = Value + 1};
do_inc_counter(replies,                 Counters = #nas_counter{replies = Value})                 -> Counters#nas_counter{replies = Value + 1};
do_inc_counter(dupRequests,             Counters = #nas_counter{dupRequests = Value})             -> Counters#nas_counter{dupRequests = Value + 1};
do_inc_counter(malformedRequests,       Counters = #nas_counter{malformedRequests = Value})       -> Counters#nas_counter{malformedRequests = Value + 1};
do_inc_counter(accessRequests,          Counters = #nas_counter{accessRequests = Value})          -> Counters#nas_counter{accessRequests = Value + 1};
do_inc_counter(accessAccepts,           Counters = #nas_counter{accessAccepts = Value})           -> Counters#nas_counter{accessAccepts = Value + 1};
do_inc_counter(accessRejects,           Counters = #nas_counter{accessRejects = Value})           -> Counters#nas_counter{accessRejects = Value + 1};
do_inc_counter(accessChallenges,        Counters = #nas_counter{accessChallenges = Value})        -> Counters#nas_counter{accessChallenges = Value + 1};
do_inc_counter(accountRequests,         Counters = #nas_counter{accountRequests = Value})         -> Counters#nas_counter{accountRequests = Value + 1};
do_inc_counter(accountResponses,        Counters = #nas_counter{accountResponses = Value})        -> Counters#nas_counter{accountResponses = Value + 1};
do_inc_counter(noRecords,               Counters = #nas_counter{noRecords = Value})               -> Counters#nas_counter{noRecords = Value + 1};
do_inc_counter(badAuthenticators,       Counters = #nas_counter{badAuthenticators = Value})       -> Counters#nas_counter{badAuthenticators = Value + 1};
do_inc_counter(packetsDropped,          Counters = #nas_counter{packetsDropped = Value})          -> Counters#nas_counter{packetsDropped = Value + 1};
do_inc_counter(unknownTypes,            Counters = #nas_counter{unknownTypes = Value})            -> Counters#nas_counter{unknownTypes = Value + 1};
do_inc_counter(handlerFailure,          Counters = #nas_counter{handlerFailure = Value})          -> Counters#nas_counter{handlerFailure = Value + 1};
do_inc_counter(coaRequests,             Counters = #nas_counter{coaRequests = Value})             -> Counters#nas_counter{coaRequests = Value + 1};
do_inc_counter(coaAcks,                 Counters = #nas_counter{coaAcks = Value})                 -> Counters#nas_counter{coaAcks = Value + 1};
do_inc_counter(coaNaks,                 Counters = #nas_counter{coaNaks = Value})                 -> Counters#nas_counter{coaNaks = Value + 1};
do_inc_counter(discRequests,            Counters = #nas_counter{discRequests = Value})            -> Counters#nas_counter{discRequests = Value + 1};
do_inc_counter(discAcks,                Counters = #nas_counter{discAcks = Value})                -> Counters#nas_counter{discAcks = Value + 1};
do_inc_counter(discNaks,                Counters = #nas_counter{discNaks = Value})                -> Counters#nas_counter{discNaks = Value + 1}.


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
