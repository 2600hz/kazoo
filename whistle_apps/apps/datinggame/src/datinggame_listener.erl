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

-export([agents_available/0, agents_busy/0, customers_waiting/0]).
-export([agents_available/1, agents_busy/1, customers_waiting/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2,
         terminate/2, code_change/3]).

-include("datinggame.hrl").

-define(SERVER, ?MODULE).

-define(BINDINGS, [{acd, []}
                  ]).
-define(RESPONDERS, [{{dg_agent, handle_online}, [{<<"acd">>, <<"agent_online">>}]}
                     ,{{dg_agent, handle_offline}, [{<<"acd">>, <<"agent_offline">>}]}
                     ,{dg_customer, [{<<"acd">>, <<"agent_connect">>}]}
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(state, {
          agents_available = queue:new() :: queue()
         ,agents_busy = dict:new() :: dict() %% [ {Agent-Call-Id, {#dg_agent{}, #dg_cust{}, GamePid}} ]
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
free_agent(Srv, #dg_agent{}=Customer) when is_pid(Srv) ->
    gen_listener:cast(Srv, {free, Customer}).

agents_available() ->
    agents_available(datinggame_sup:listener_proc()).
agents_busy() ->
    agents_busy(datinggame_sup:listener_proc()).
customers_waiting() ->
    customers_waiting(datinggame_sup:listener_proc()).

agents_available(Srv) ->
    gen_listener:call(Srv, agents_available).
agents_busy(Srv) ->
    gen_listener:call(Srv, agents_busy).
customers_waiting(Srv) ->
    gen_listener:call(Srv, customers_waiting).

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
                    ?LOG("the game has started in ~p for agent ~s and customer ~s", [Pid, CallID, _CCallID]),
                    erlang:monitor(process, Pid),

                    {noreply, State#state{agents_available=Available1
                                          ,agents_busy=dict:store(CallID, {Agent, Customer, Pid}, Busy)
                                          ,customers_waiting=Waiting1
                                         }}
            end
    end;

handle_cast({connect, #dg_customer{call_id=_CustCallID}=Customer}, #state{customers_waiting=Waiting}=State) ->
    play_hold_music(Customer),
    gen_listener:cast(self(), connect_agent),
    {noreply, State#state{customers_waiting=queue:in(Customer, Waiting)}};

handle_cast({add_agent, #dg_agent{call_id=_CallID}=Agent}, #state{agents_available=Online}=State) ->
    ?LOG("new agent added: ~s", [_CallID]),
    gen_listener:cast(self(), connect_agent),
    {noreply, State#state{agents_available=queue:in(Agent, Online)}};

handle_cast({free_agent, #dg_agent{}=Agent}, #state{agents_busy=Busy}=State) ->
    {_, Busy1} = rm_agent_from_busy(Agent, Busy),
    datinggame_listener:add_agent(self(), Agent),
    {noreply, State#state{agents_busy=Busy1}};

handle_cast({rm_agent, #dg_agent{call_id=_CallID}=Agent}, #state{agents_available=Online, agents_busy=Busy}=State) ->
    ?LOG("rm agent: ~s", [_CallID]),
    Online1 = rm_agent_from_online(Agent, Online),
    {_, Busy1} = rm_agent_from_busy(Agent, Busy),
    {noreply, State#state{agents_available=Online1, agents_busy=Busy1}};

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
handle_info({'DOWN', _Ref, process, _Pid, normal}, State) ->
    ?LOG("the game is gone: ~p", [_Pid]),
    {noreply, State};
handle_info({'DOWN', _Ref, process, Pid, Reason}, #state{agents_busy=Busy}=State) ->
    ?LOG("the game has gone awry: ~p", [Reason]),
    case rm_agent_from_busy(Pid, Busy) of
        {undefined, _} -> {noreply, State};
        {Agent, Busy1} ->
            datinggame_listener:add_agent(self(), Agent),
            {noreply, State#state{agents_busy=Busy1}}
    end;
handle_info(_Info, State) ->
    ?LOG("unhandled message: ~p", [_Info]),
    {noreply, State}.

handle_event(_JObj, #state{agents_available=Online, agents_busy=Busy}) ->
    {reply, [{available, Online}, {busy, Busy}]}.

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

-spec rm_agent_from_busy/2 :: (#dg_agent{} | ne_binary() | pid(), dict()) -> {#dg_agent{} | 'undefined', dict()}.
rm_agent_from_busy(#dg_agent{call_id=CallID}=Agent, Busy) ->
    {Agent, dict:erase(CallID, Busy)};
rm_agent_from_busy(CallID, Busy) when is_binary(CallID) ->
    case dict:find(CallID, Busy) of
        {ok, {Agent, _, _}} ->
            {Agent, dict:erase(CallID, Busy)};
        _ ->
            {undefined, Busy}
    end;
rm_agent_from_busy(GamePid, Busy) when is_pid(GamePid) ->
    EndGame = dict:filter(fun(_, {_,_,Pid}) ->
                                  GamePid =:= Pid
                          end, Busy),
    case dict:size(EndGame) of
        0 ->
            ?LOG("can't rm agent from busy by pid ~p", [GamePid]),
            {undefined, Busy};
        1 ->
            [{CallID, {Agent, _, _}}] = dict:to_list(EndGame),
            ?LOG("rm agent from busy by pid ~p: ~s", [GamePid, CallID]),
            {Agent, dict:erase(CallID, Busy)}
    end.
                             

play_hold_music(#dg_customer{call_id=CallID, control_queue=CtlQ}) ->
    Command = [{<<"Application-Name">>, <<"hold">>}
               ,{<<"Insert-At">>, <<"now">>}
              ],
    dg_game:send_command(Command, CallID, CtlQ).
