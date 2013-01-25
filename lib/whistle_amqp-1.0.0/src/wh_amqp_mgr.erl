%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wh_amqp_mgr).

-behaviour(gen_server).

-export([start_link/0
         ,get_connection/0
         ,publish/2
         ,consume/1
         ,misc_req/1
         ,register_return_handler/0
         ,is_available/0
         ,available_brokers/0
         ,brokers/0
         ,wait_for_available_host/0
         ,notify_return_handlers/1
        ]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("amqp_util.hrl").

-define(SERVER, ?MODULE).
-define(START_TIMEOUT, 500).
-define(MAX_TIMEOUT, 5000).

-type strategy() :: 'priority'.

-record(state, {brokers = dict:new() :: dict()
                ,available_brokers = dict:new() :: dict()
                ,current_broker :: wh_amqp_broker:broker()
                ,strategy = 'priority' :: strategy()
                ,return_handlers = dict:new() :: dict() %% ref, pid() - list of PIDs that are interested in returned messages
               }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get_connection/0 :: () -> {'ok', atom()} |
                                {'error', 'amqp_down'}.
get_connection() ->
    gen_server:call(?SERVER, get_connection).

-spec is_available/0 :: () -> boolean().
is_available() ->
    gen_server:call(?SERVER, is_available).

available_brokers() ->
    gen_server:call(?SERVER, available_brokers).
brokers() ->
    gen_server:call(?SERVER, brokers).

-spec wait_for_available_host/0 :: () -> 'ok'.
wait_for_available_host() ->
    case is_available() of
        true -> ok;
        false ->
            timer:sleep(random:uniform(1000) + 100),
            wait_for_available_host()
    end.

-spec consume/1 :: (wh_amqp_connection:consume_records()) -> wh_amqp_connection:consume_ret().
consume(BC) ->
    case get_connection() of
        {ok, Connection} -> wh_amqp_connection:consume(Connection, BC);
        {error, _}=E -> E
    end.

-spec publish/2 :: (#'basic.publish'{}, #'amqp_msg'{}) -> 'ok'.
publish(BP, AM) ->
    case get_connection() of
        {ok, Connection} -> wh_amqp_connection:publish(Connection, BP, AM);
        {error, _}=E -> E
    end.

-spec misc_req/1 :: (wh_amqp_connection:misc_records()) -> 'ok' | {'error', _}.
misc_req(Req) ->
    case get_connection() of
        {ok, Connection} -> wh_amqp_connection:misc_req(Connection, Req);
        {error, _}=E -> E
    end.

-spec register_return_handler/0 :: () -> 'ok'.
register_return_handler() ->
    gen_server:cast(?SERVER, {register_return_handler, self()}).

-spec notify_return_handlers/1 :: ({#'basic.return'{}, #amqp_msg{}}) -> pid().
notify_return_handlers(ReturnMsg) ->
    spawn(fun() ->
                  put(callid, ?LOG_SYSTEM_ID),
                  RHDict = gen_server:call(?SERVER, get_return_handler_dict),
                  lager:debug("recieved notification a message couldnt be delivered, forwarding to registered return handlers"),
                  dict:map(fun(_, Pid) -> Pid ! ReturnMsg end, RHDict)
          end).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
-spec init/1 :: ([]) -> {'ok', #state{}}.
init([]) ->
    put(callid, ?MODULE),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%
%%--------------------------------------------------------------------
handle_call(get_return_handler_dict, _, #state{return_handlers=RHDict}=State) ->
    {reply, RHDict, State};

handle_call(is_available, _, #state{available_brokers=Brokers}=State) ->
    {reply, dict:size(Brokers) > 0, State};

handle_call(available_brokers, _, #state{available_brokers=Brokers}=State) ->
    {reply, [N || {N, _} <- dict:to_list(Brokers)], State};

handle_call(brokers, _, #state{brokers=Bs}=State) ->
    {reply, [{N, P} || {N, {_, P}} <- dict:to_list(Bs)], State};

handle_call(get_connection, _, #state{current_broker=undefined
                                      ,strategy=Strategy
                                      ,available_brokers=Available
                                      ,brokers=Brokers
                                      }=State) ->
    case next_available_broker(Strategy, Available, Brokers) of
        undefined -> {reply, {error, amqp_down}, State, hibernate};
        Broker ->
            {reply, {ok, wh_amqp_broker:name(Broker)}, State#state{current_broker=Broker}, hibernate}
    end;
handle_call(get_connection, _, #state{current_broker=Broker}=State) ->
    {reply, {ok, wh_amqp_broker:name(Broker)}, State};
handle_call(_, _, State) ->
    {reply, {error, not_implemented}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({register_return_handler, FromPid}, #state{return_handlers=RHDict}=State) ->
    lager:debug("adding ~p as a return handler", [FromPid]),
    {noreply, State#state{return_handlers=dict:store(erlang:monitor(process, FromPid), FromPid, RHDict)}, hibernate};
handle_cast({add_broker, URI, UseFederation}, #state{brokers=Brokers}=State) ->
    Builders = [fun(B) -> wh_amqp_broker:set_uri(URI, B) end
                ,fun(B) -> wh_amqp_broker:set_use_federation(UseFederation, B) end
               ],

    case catch lists:foldl(fun(F, B) -> F(B) end, wh_amqp_broker:new(), Builders) of
        {'EXIT', _} ->
            lager:error("failed to parse amqp URI '~p', dropping", [URI]),
            {noreply, State};
        Broker ->
            Name = wh_amqp_broker:name(Broker),
            Priority = dict:size(Brokers) + 1,

            case wh_amqp_connection_sup:add(Broker) of
                {ok, _} ->
                    _Ref = erlang:monitor(process, Name),
                    {noreply, State#state{brokers=dict:store(Name, {Broker, Priority}, Brokers)}, hibernate};
                {error, {already_started, _}} ->
                    lager:info("connection to broker ~s already started(~p)", [Name, Priority]),
                    _Ref = erlang:monitor(process, Name),
                    {noreply, State#state{brokers=dict:store(Name, {Broker, Priority}, Brokers)}, hibernate};
                {error, {Reason, _}} ->
                    lager:warning("unable to start amqp connection to '~s': ~p", [Name, Reason]),
                    _ = wh_amqp_connection_sup:remove(Broker),
                    {noreply, State}
            end
    end;
handle_cast({broker_unavailable, Name}, #state{available_brokers=Available
                                               ,brokers=Brokers
                                               ,strategy=Strategy
                                              }=State) ->
    Available1 = dict:erase(Name, Available),

    NewBroker = next_available_broker(Strategy, Available1, Brokers),
    {noreply, State#state{available_brokers=Available1
                          ,current_broker=NewBroker
                         }};
handle_cast({broker_available, Name}, #state{brokers=Brokers
                                             ,available_brokers=AvailableBrokers
                                             ,current_broker=CurrBroker
                                            }=State) ->
    case dict:find(Name, Brokers) of
        error ->
            lager:debug("received notice that unknown AMQP broker '~s' is available, ignoring", [Name]),
            {noreply, State};
        {ok, {Broker, P}} ->
            lager:debug("received notice that AMQP broker '~s'(~p) is available", [Name, P]),

            NewCurrBroker = maybe_update_broker(CurrBroker, Broker, P, Brokers),

            {noreply, State#state{available_brokers=dict:store(Name, Broker, AvailableBrokers)
                                  ,current_broker=NewCurrBroker
                                 }, hibernate}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', Ref, process, _Pid, _Reason}, #state{return_handlers=RHDict}=State) ->
    lager:debug("recieved notification monitored process ~p  died ~p, searching for reference", [_Pid, _Reason]),
    erlang:demonitor(Ref, [flush]),
    {noreply, State#state{return_handlers=dict:erase(Ref, RHDict)}, hibernate};
handle_info({'DOWN', Ref, process, {Name, _}, _R}, #state{available_brokers=Brokers}=State) ->
    erlang:demonitor(Ref, [flush]),
    lager:info("lost connection to AMQP broker '~s'", [Name]),
    {noreply, State#state{available_brokers=dict:erase(Name, Brokers)}, hibernate};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
-spec terminate/2 :: (term(), #state{}) -> 'ok'.
terminate(_Reason, _) ->
    lager:debug("amqp manager terminated: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec next_available_broker/3 :: (strategy(), dict(), dict()) -> 'undefined' | wh_amqp_broker:broker().
next_available_broker(Strategy, Available, Brokers) ->
    case dict:size(Available) of
        0 -> undefined;
        1 -> [{_, Broker}] = dict:to_list(Available), Broker;
        _More -> find_available_broker(Available, prioritize_brokers(Strategy, Brokers))
    end.

prioritize_brokers('priority', Bs) ->
    lists:keysort(1, [ {P, N} || {N, {_B, P}} <- dict:to_list(Bs)]).

find_available_broker(_Available, []) -> undefined;
find_available_broker(Available, [{_,N}|Bs]) ->
    case dict:find(N, Available) of
        error -> find_available_broker(Available, Bs);
        {ok, Broker} -> Broker
    end.

maybe_update_broker(undefined, NewBroker, _, _) -> NewBroker;
maybe_update_broker(CurrBroker, NewBroker, P, Brokers) ->
    case dict:find(wh_amqp_broker:name(CurrBroker), Brokers) of
        {ok, {_, CurrP}} when P < CurrP ->
            lager:debug("new broker is higher priority(~p) than current broker(~p), chaos-monkey time!", [P, CurrP]),
            wh_amqp_connection:teardown_channels(CurrBroker),
            NewBroker;
        {ok, {_, CurrP}} ->
            lager:debug("current broker is higher priority(~p) than new broker(~p)", [CurrP, P]),
            CurrBroker;
        error ->
            lager:debug("somehow curr broker is not found: ~p", [CurrBroker]),
            NewBroker
    end.
