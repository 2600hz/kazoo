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
-export([start_link/0, add_agent/2, rm_agent/2]).

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
         ,agents_busy = dict:new() :: dict() %% [ {Agent-Call-Id, {#dg_agent{}, #dg_cust{}}} ]
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

add_agent(Srv, #dg_agent{}=Agent) when is_pid(Srv) ->
    gen_listener:cast(Srv, {add_agent, Agent}).

rm_agent(Srv, #dg_agent{}=Agent) when is_pid(Srv) ->
    gen_listener:cast(Srv, {rm_agent, Agent}).

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
handle_cast({add_agent, #dg_agent{call_id=_CallID}=Agent}, #state{agents_available=Online}=State) ->
    ?LOG("new agent added: ~s", [_CallID]),
    {norepy, State#state{agents_available=queue:in(Agent, Online)}};

handle_cast({rm_agent, #dg_agent{call_id=_CallID}=Agent}, #state{agents_available=Online, agents_busy=Busy}=State) ->
    ?LOG("rm agent: ~s", [_CallID]),
    Online1 = rm_agent_from_online(Agent, Online),
    Busy1 = rm_agent_from_busy(Agent, Busy),
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
handle_info(_Info, State) ->
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

-spec rm_agent_from_busy/2 :: (#dg_agent{}, dict()) -> dict().
rm_agent_from_busy(#dg_agent{call_id=CallID}, Busy) ->
    dict:erase(CallID, Busy).
