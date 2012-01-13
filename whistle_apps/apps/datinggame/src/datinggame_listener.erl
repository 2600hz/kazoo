%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2012, James Aimonetti
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2012 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(datinggame_listener).

-behaviour(gen_listener).

%% API
-export([start_link/0, add_agent/2, rm_agent/2, free_agent/2, connect_agent/2]).

%% Data inspection
-export([agents_available/0, agents_busy/0, customers_waiting/0]).
-export([agents_available/1, agents_busy/1, customers_waiting/1]).

%% Internal AMQP handlers
-export([handle_channel_status_resp/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2,
         terminate/2, code_change/3]).

-include("datinggame.hrl").

-define(SERVER, ?MODULE).

-define(BINDINGS, [{acd, []}
                   ,{self, []} % for channel status responses
                  ]).
-define(RESPONDERS, [{{dg_agent, handle_online}, [{<<"acd">>, <<"agent_online">>}]}
                     ,{{dg_agent, handle_offline}, [{<<"acd">>, <<"agent_offline">>}]}
                     ,{dg_customer, [{<<"acd">>, <<"agent_connect">>}]}
                     ,{{?MODULE, handle_channel_status_resp}, [{<<"call_event">>, <<"channel_status_resp">>}]}
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(state, {
          agents_available = queue:new() :: queue()
         ,agents_busy = dict:new() :: dict() %% [ {Agent-Call-Id, {#dg_agent{}, #dg_cust{}, GamePid}} ]
         ,agents_pending = dict:new() :: dict() %% [ {Agent-Call-Id, #dg_agent{}} ] - holds agents who's channel_status_req hasn't been responded to
         ,customers_waiting = queue:new() :: queue()
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_listener:start_link(?MODULE, [{bindings, ?BINDINGS}
                                      ,{responders, ?RESPONDERS}
                                      ,{queue_name, ?QUEUE_NAME}
                                      ,{queue_options, ?QUEUE_OPTIONS}
                                      ,{consume_options, ?CONSUME_OPTIONS}
                                      ]
                                      , []).

-spec add_agent/2 :: (pid(), #dg_agent{}) -> 'ok'.
add_agent(Srv, #dg_agent{}=Agent) when is_pid(Srv) ->
    gen_listener:cast(Srv, {add_agent, Agent}).

-spec rm_agent/2 :: (pid(), #dg_agent{}) -> 'ok'.
rm_agent(Srv, #dg_agent{}=Agent) when is_pid(Srv) ->
    gen_listener:cast(Srv, {rm_agent, Agent}).

-spec connect_agent/2 :: (pid(), #dg_customer{}) -> 'ok'.
connect_agent(Srv, #dg_customer{}=Customer) when is_pid(Srv) ->
    gen_listener:cast(Srv, {connect, Customer}).

-spec free_agent/2 :: (pid(), #dg_agent{}) -> 'ok'.
free_agent(Srv, #dg_agent{}=Agent) when is_pid(Srv) ->
    gen_listener:cast(Srv, {free_agent, Agent}).

agents_available() ->
    {ok, Srv} = datinggame_sup:listener_proc(),
    agents_available(Srv).
agents_available(Srv) ->
    gen_listener:call(Srv, agents_available).

agents_busy() ->
    {ok, Srv} = datinggame_sup:listener_proc(),
    agents_busy(Srv).
agents_busy(Srv) ->
    gen_listener:call(Srv, agents_busy).

customers_waiting() ->
    {ok, Srv} = datinggame_sup:listener_proc(),
    customers_waiting(Srv).
customers_waiting(Srv) ->
    gen_listener:call(Srv, customers_waiting).

-spec handle_channel_status_resp/2 :: (json_object(), proplist()) -> 'ok'.
handle_channel_status_resp(JObj, Props) ->
    Srv = props:get_value(server, Props),
    gen_listener:cast(Srv, {update_agent, JObj}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(agents_available, From, #state{agents_available=Available}=State) ->
    spawn(fun() ->
                  Resp = queue:to_list(Available),
                  gen_server:reply(From, Resp)
          end),
    {noreply, State};
handle_call(agents_busy, From, #state{agents_busy=Busy}=State) ->
    spawn(fun() ->
                  Resp = dict:to_list(Busy),
                  gen_server:reply(From, Resp)
          end),
    {noreply, State};
handle_call(customers_waiting, From, #state{customers_waiting=Waiting}=State) ->
    spawn(fun() ->
                  Resp = queue:to_list(Waiting),
                  gen_server:reply(From, Resp)
          end),
    {noreply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(connect_agent, #state{agents_available=Available
                                  ,agents_busy=Busy
                                  ,customers_waiting=Waiting
                                 }=State) ->
    case queue:is_empty(Waiting) of
        true ->
            ?LOG("no customers waiting"),
            {noreply, State};
        false ->
            case queue:is_empty(Available) of
                true ->
                    ?LOG("no agents available at the moment"),
                    {noreply, State};
                false ->
                    {{value, #dg_agent{call_id=CallID}=Agent}, Available1} = queue:out(Available),
                    {{value, #dg_customer{call_id=_CCallID}=Customer}, Waiting1} = queue:out(Waiting),

                    {ok, Pid} = dg_game_sup:start_game(self(), Agent, Customer),
                    ?LOG(CallID, "the game has started in ~p for agent ~s and customer ~s", [Pid, CallID, _CCallID]),
                    erlang:monitor(process, Pid),

                    {noreply, State#state{agents_available=Available1
                                          ,agents_busy=dict:store(CallID, {Agent, Customer, Pid}, Busy)
                                          ,customers_waiting=Waiting1
                                         }}
            end
    end;

handle_cast({update_agent, JObj}, #state{agents_available=Available, agents_pending=Pending}=State) ->
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    case dict:find(CallID, Pending) of
        {ok, Agent} ->
            ?LOG(CallID, "updating agent with call id ~s", [CallID]),
            gen_listener:cast(self(), connect_agent),

            {noreply, State#state{agents_available=queue:in(update_agent(Agent, JObj), Available)
                                  ,agents_pending=dict:erase(CallID, Pending)
                                 }};
        error ->
            ?LOG(CallID, "no agent with call-id ~s found pending", [CallID]),
            {noreply, State}
    end;

handle_cast({connect, #dg_customer{call_id=_CustCallID}=Customer}, #state{customers_waiting=Waiting}=State) ->
    ?LOG(_CustCallID, "customer connect recevied, putting in waiting queue", []),
    dg_util:hold_call(Customer),
    gen_listener:cast(self(), connect_agent),
    {noreply, State#state{customers_waiting=queue:in(Customer, Waiting)}};

handle_cast({add_agent, #dg_agent{call_id=CallID}=Agent}, #state{agents_pending=Pending}=State) ->
    ?LOG(CallID, "new agent added, waiting on channel_status_req: ~s", [CallID]),
    {noreply, State#state{agents_pending=dict:store(CallID, Agent, Pending)}};

handle_cast({free_agent, #dg_agent{call_id=CallID}=Agent}, #state{agents_available=Available, agents_busy=Busy}=State) ->
    {_, Busy1} = rm_agent_from_dict(Agent, Busy),
    ?LOG(CallID, "free agent: rm from busy dict", []),
    gen_listener:cast(self(), connect_agent),
    dg_util:hold_call(Agent),
    ?LOG(CallID, "free agent: putting agent back on hold", []),
    {noreply, State#state{agents_busy=Busy1, agents_available=queue:in(Agent, Available)}};

handle_cast({rm_agent, #dg_agent{call_id=_CallID}=Agent}, #state{agents_available=Available, agents_busy=Busy, agents_pending=Pending}=State) ->
    ?LOG(_CallID, "rm agent: ~s", [_CallID]),
    {_, Busy1} = rm_agent_from_dict(Agent, Busy),
    {_, Pending1} = rm_agent_from_dict(Agent, Pending),

    {noreply, State#state{agents_available=rm_agent_from_online(Agent, Available)
                          ,agents_busy=Busy1
                          ,agents_pending=Pending1
                         }};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', _Ref, process, Pid, Reason}, #state{agents_busy=Busy}=State) ->
    ?LOG("the game has gone: ~p", [Reason]),
    case rm_agent_from_dict(Pid, Busy) of
        {undefined, _} ->
            ?LOG("dg_game pid not found in busy dict"),
            {noreply, State};
        {#dg_agent{call_id=CallID}=Agent, Busy1} ->
            ?LOG(CallID, "agent being freed", []),
            datinggame_listener:free_agent(self(), Agent),
            {noreply, State#state{agents_busy=Busy1}}
    end;
handle_info(_Info, State) ->
    ?LOG("unhandled message: ~p", [_Info]),
    {noreply, State}.

handle_event(_JObj, _State) ->
    {reply, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec rm_agent_from_online/2 :: (#dg_agent{}, queue()) -> queue().
rm_agent_from_online(#dg_agent{call_id=CallId}, Online) ->
    queue:filter(fun(#dg_agent{call_id=Acall_id}) ->
                         CallId =/= Acall_id
                 end, Online).

-spec rm_agent_from_dict/2 :: (#dg_agent{} | ne_binary() | pid(), dict()) -> {#dg_agent{} | 'undefined', dict()}.
rm_agent_from_dict(#dg_agent{call_id=CallID}=Agent, Busy) ->
    {Agent, dict:erase(CallID, Busy)};
rm_agent_from_dict(CallID, Busy) when is_binary(CallID) ->
    case dict:find(CallID, Busy) of
        {ok, {Agent, _, _}} ->
            {Agent, dict:erase(CallID, Busy)};
        _ ->
            {undefined, Busy}
    end;
rm_agent_from_dict(GamePid, Busy) when is_pid(GamePid) ->
    EndGame = dict:filter(fun(_, {_,_,Pid}) ->
                                  GamePid =:= Pid
                          end, Busy),
    case dict:size(EndGame) of
        0 ->
            ?LOG("can't rm agent from busy by pid ~p", [GamePid]),
            {undefined, Busy};
        1 ->
            [{CallID, {Agent, _, _}}] = dict:to_list(EndGame),
            ?LOG(CallID, "rm agent from busy by pid ~p: ~s", [GamePid, CallID]),
            {Agent, dict:erase(CallID, Busy)}
    end.

-spec update_agent/2 :: (#dg_agent{}, json_object()) -> #dg_agent{}.
update_agent(#dg_agent{call_id=_CallID}=Agent, JObj) ->
    Hostname = wh_json:get_ne_value(<<"Switch-Hostname">>, JObj),

    ?LOG(_CallID, "update agent:"),
    ?LOG(_CallID, "switch hostname: ~s", [Hostname]),

    Agent#dg_agent{
      switch_hostname=Hostname
     }.
