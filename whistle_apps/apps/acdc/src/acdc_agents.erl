%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(acdc_agents).

-behaviour(gen_server).

-export([start_link/0]).
-export([next_agent/0
         ,update_agent/1
        ]).
-export([reload_agents/0]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("acdc.hrl").

-type mi_list() :: [{pid(), pos_integer()},...] | [].
-type state() :: {'rr', queue()} | {'mi', mi_list(), mi_list()}.

%% rr - round robin: no information about the agent's last call is kept
%% mi - most idle: track agent's last call; agent with lowest last call
%%      is given first shot at the next caller

-define(SERVER, ?MODULE).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec reload_agents/0 :: () -> 'ok'.
reload_agents() ->
    gen_server:cast(?SERVER, reload_agents).

-spec next_agent/0 :: () -> {'ok', pid()} | {'error', 'no_agents'}.
next_agent() ->
    gen_server:call(?SERVER, next_agent).

-spec update_agent/1 :: (pid()) -> 'ok'.
update_agent(Agent) ->
    gen_server:cast(?SERVER, {update_agent, Agent, wh_util:current_tstamp()}).

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
-spec init/1 :: ([]) -> {ok, state()}.
init([]) ->
    put(callid, ?LOG_SYSTEM_ID),

    case whapps_config:get(?APP_NAME, <<"queue_strategy">>, <<"longest_idle">>) of
        <<"round_robin">> ->
            lager:debug("starting acdc agents with round-robin"),
            {ok, {rr, queue:new()}};
        <<"longest_idle">> ->
            lager:debug("starting acdc agents with most-idle"),
            {ok, {mi, [], []}} % {Agent, idle_time}
    end.

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
handle_call(next_agent, _From, Agents) ->
    case next_agent_please(Agents) of
        undefined -> {reply, {error, no_agent}, Agents};
        {Agent, NewAgents} -> {reply, {ok, Agent}, NewAgents}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

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
handle_cast(reload_agents, As) ->
    lager:debug("reloading list of agent workers"),
    {noreply, reload(As, acdc_agent_sup:workers())};
handle_cast({update_agent, Agent, Time}, Agents) ->
    Agents1 = update(Agents, Agent, Time),
    {noreply, Agents1};
handle_cast(_, State) ->
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
    lager:debug("acdc agents terminating: ~p", [_Reason]).

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
-spec next_agent_please/1 :: (state()) -> {pid(), state()} | 'undefined'.
next_agent_please({rr, As}) ->
    case queue:out(As) of
        {empty, _} -> undefined;
        {{value, A}, As1} -> {A, {rr, queue:in(A, As1)}}
    end;
next_agent_please({mi, [A|As]=Agents, Out}) ->
    {NextAgent, _}=Next = lists:foldr(fun({_, T}=NewAcc, {_, T1}) when T < T1 -> NewAcc;
                                         (_, Acc) -> Acc
                                      end, A, As),
    {NextAgent, {mi, lists:keydelete(NextAgent, 1, Agents), [Next | Out]}};
next_agent_please({mi, [], _}) ->
    undefined.

-spec update/3 :: (state(), pid(), pos_integer()) -> state().
update({rr, _}=RR, _, _) ->
    RR;
update({mi, As, Out}=MI, A, T) ->
    case lists:keytake(A, 1, As) of
        false ->
            case lists:keytake(A, 1, Out) of
                false -> MI;
                {value, {A, _}, Out1} -> {mi, [{A, T}|As], Out1}
            end;
        {value, {A, _}, As1} -> {mi, [{A, T} | As1], Out}
    end.

-spec reload/2 :: (state(), [pid(),...]) -> state().
reload({rr, _}, Ws) ->
    {rr, queue:from_list(Ws)};
reload({mi, As, _Out}, Ws) ->
    {mi, [{W, idle_time(lists:keyfind(W, 1, As))} || W <- Ws], []}.

-spec idle_time/1 :: ('false' | {pid(),pos_integer()}) -> pos_integer().
idle_time(false) ->
    wh_util:current_tstamp();
idle_time({value, {_,T}}) ->
    T.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec agent_count/1 :: (state()) -> non_neg_integer().
agent_count({rr, As}) ->
    queue:len(As);
agent_count({mi, As, _}) ->
    length(As).

rr_next_agent_please_test() ->
    State0 = {rr, queue:new()},
    State1 = reload(State0, [pid1, pid2, pid3]),
    ?assertEqual(agent_count(State1), 3),

    {A1, State2} = next_agent_please(State1),
    {A2, State3} = next_agent_please(State2),
    {A3, State4} = next_agent_please(State3),
    ?assertEqual(agent_count(State4), 3),
    ?assertEqual(A1, pid1),
    ?assertEqual(A2, pid2),
    ?assertEqual(A3, pid3),

    {A4, State5} = next_agent_please(State4),
    {A5, State6} = next_agent_please(State5),
    {A6, State7} = next_agent_please(State6),
    ?assertEqual(agent_count(State7), 3),
    ?assertEqual(A1, A4),
    ?assertEqual(A2, A5),
    ?assertEqual(A3, A6).

li_next_agent_please_test() ->
    State0 = {mi, [], []},
    State1 = reload(State0, [pid1, pid2, pid3]),
    ?assertEqual(agent_count(State1), 3),

    State2 = update(State1, pid1, 1),
    State3 = update(State2, pid2, 5),
    State4 = update(State3, pid3, 3),

    {A1, State5} = next_agent_please(State4),
    {A2, State6} = next_agent_please(State5),
    {A3, State7} = next_agent_please(State6),

    ?assertEqual(agent_count(State7), 0),
    ?assertEqual(undefined, next_agent_please(State7)),

    ?assertEqual(pid1, A1),
    ?assertEqual(pid3, A2),
    ?assertEqual(pid2, A3),

    State8 = update(State7, pid1, 9),
    State9 = update(State8, pid2, 11),
    State10 = update(State9, pid3, 3),

    {A4, State11} = next_agent_please(State10),
    {A5, State12} = next_agent_please(State11),
    {A6, State13} = next_agent_please(State12),

    ?assertEqual(agent_count(State13), 0),
    ?assertEqual(undefined, next_agent_please(State13)),

    ?assertEqual(pid3, A4),
    ?assertEqual(pid1, A5),
    ?assertEqual(pid2, A6).
    
-endif.
