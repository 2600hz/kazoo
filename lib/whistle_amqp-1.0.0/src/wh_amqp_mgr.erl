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

-export([start_link/0]).
-export([get_connection/0]).
-export([publish/2]).
-export([consume/1]).
-export([misc_req/1]).
-export([register_return_handler/0]).
-export([is_available/0]).
-export([wait_for_available_host/0]).
-export([notify_return_handlers/1]).
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

-record(state, {brokers=dict:new() :: dict()
                ,available_brokers=dict:new() :: dict()
                ,strategy=priority :: 'priority'
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

-spec get_connection/0 :: () -> {'ok', atom()} | {'error', 'amqp_down'}.
get_connection() ->
    gen_server:call(?SERVER, get_connection).

-spec is_available/0 :: () -> boolean().
is_available() ->
    gen_server:call(?SERVER, is_available).

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
    put(callid, ?LOG_SYSTEM_ID),
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
handle_call(get_connection, _, State) ->
    case next_available_broker(State) of 
        {undefined, S} ->
            {reply, {error, amqp_down}, S, hibernate};
        {Broker, S} ->
            Name = wh_amqp_broker:name(Broker),
            {reply, {ok, Name}, S, hibernate}
    end;
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
            lager:error("failed to parse AMQP broker URI '~p', dropping", [URI]),
            {noreply, State};
        Broker ->
            Name = wh_amqp_broker:name(Broker),
            case wh_amqp_connection_sup:add(Broker) of
                {ok, _} ->
                    _Ref = erlang:monitor(process, Name),
                    {noreply, State#state{brokers=dict:store(Name, Broker, Brokers)}, hibernate};
                {error, {already_started, _}} ->
                    _Ref = erlang:monitor(process, Name),
                    {noreply, State#state{brokers=dict:store(Name, Broker, Brokers)}, hibernate};
                {error, {Reason, _}} ->
                    lager:info("unable to start AMQP connection to '~s': ~p", [Name, Reason]),
                    _ = wh_amqp_connection_sup:remove(Broker),
                    {noreply, State}
            end
    end;
handle_cast({broker_unavailable, Name}, #state{available_brokers=Brokers}=State) ->
    {noreply, State#state{available_brokers=dict:erase(Name, Brokers)}};
handle_cast({broker_available, Name}, #state{brokers=Brokers, available_brokers=AvailableBrokers}=State) ->
    case dict:find(Name, Brokers) of 
        error -> 
            lager:debug("received notice that unknown AMQP broker '~s' is available, ignoring", [Name]),
            {noreply, State};
        {ok, Broker} ->
            lager:debug("received notice that AMQP broker '~s' is available", [Name]),
            {noreply, State#state{available_brokers=dict:store(Name, Broker, AvailableBrokers)}, hibernate}
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
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
-spec terminate/2 :: (term(), #state{}) -> no_return().
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
-spec next_available_broker/1 :: (#state{}) -> {'undefined' | wh_amqp_broker:broker(), #state{}}.
-spec next_available_broker/3 :: ('priority', [{atom(), wh_amqp_broker:broker()},...] | [], #state{}) -> {'undefined' | wh_amqp_broker:broker(), #state{}}.

next_available_broker(#state{strategy=Strategy,  available_brokers=Brokers}=State) ->
    case dict:to_list(Brokers) of
        [] -> {undefined, State};
        [{_, Broker}] -> {Broker, State};
        Else -> next_available_broker(Strategy, Else, State)
    end.

next_available_broker(_, [], State) ->
    {undefined, State};
next_available_broker(priority, [{_, Broker}|Brokers], State) ->
    case wh_amqp_broker:is_available(Broker) of
        true -> {Broker, State};
        false -> next_available_broker(priority, Brokers, State)
    end.
