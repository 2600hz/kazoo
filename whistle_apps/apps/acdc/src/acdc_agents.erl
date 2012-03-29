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
         ,get_agents/0
         ,update_agent/1
         ,reload_agents/0
         ,next_agent_please/1
        ]).

-export([summary/0]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("acdc.hrl").

-type mi_list() :: [{pid(), pos_integer(), reference()},...] | [].

-opaque agents() :: {'rr', queue()} |             % queue({pid(), reference()})
                    {'mi', mi_list()}. % [{pid(), last_call_timestamp, reference()}]
-export_type([agents/0]).

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

-spec get_agents/0 :: () -> agents().
get_agents() ->
    gen_server:call(?SERVER, get_agents).

-spec update_agent/1 :: (pid()) -> 'ok'.
update_agent(Agent) ->
    gen_server:cast(?SERVER, {update_agent, Agent, wh_util:current_tstamp()}).

summary() ->
    gen_server:call(?SERVER, summary).

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
-spec init/1 :: ([]) -> {'ok', agents()}.
init([]) ->
    put(callid, ?LOG_SYSTEM_ID),

    case whapps_config:get(?APP_NAME, <<"queue_strategy">>, <<"most_idle">>) of
        <<"round_robin">> ->
            lager:debug("starting acdc agents with round-robin"),
            {ok, {rr, queue:new()}};
        <<"longest_idle">> ->
            whapps_config:set(?APP_NAME, <<"queue_strategy">>, <<"most_idle">>),
            lager:debug("starting acdc agents with most-idle"),
            {ok, {mi, []}}; % {Agent, idle_time}
        <<"most_idle">> ->
            lager:debug("starting acdc agents with most-idle"),
            {ok, {mi, []}} % {Agent, idle_time}
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
handle_call(summary, _From, Agents) ->
    {reply, summary(Agents), Agents};
handle_call(get_agents, _From, Agents) ->
    {reply, Agents, Agents};
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
    {noreply, update(Agents, Agent, Time)}.

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
handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    lager:debug("agent ~p maybe went down: ~p", [Pid, _Reason]),
    gen_server:cast(self(), reload_agents),
    {noreply, remove(State, Pid, Ref)};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
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
-spec next_agent_please/1 :: (agents()) -> {pid(), agents()} | 'undefined'.
next_agent_please({rr, As}) ->
    case queue:out(As) of
        {empty, _} -> undefined;
        {{value, {A, _}=Agent}, As1} -> {A, {rr, queue:in(Agent, As1)}}
    end;
next_agent_please({mi, [A|As]=Agents}) ->
    {NextAgent, _, _} = lists:foldr(fun({_, T, _}=NewAcc, {_, T1, _}) when T < T1 -> NewAcc;
                                       (_, Acc) -> Acc
                                    end, A, As),
    {NextAgent, {mi, lists:keydelete(NextAgent, 1, Agents)}};
next_agent_please({mi, []}) ->
    undefined.

-spec update/3 :: (agents(), pid(), pos_integer()) -> agents().
update({rr, As}, A, _) ->
    %% forward queue to just past A
    {End, [Agent|Start]} = lists:splitwith(fun({P,_}) -> P =/= A end, queue:to_list(As)),
    {rr, queue:from_list(Start ++ End ++ [Agent])};
update({mi, As}, A, T) ->
    case lists:keytake(A, 1, As) of
        false -> {mi, [{A, T, erlang:monitor(process, A)} | As]};
        {value, {A, _, Ref}, As1} -> {mi, [{A, T, Ref} | As1]}
    end.

-spec remove/3 :: (agents(), pid(), reference()) -> agents().
remove({rr, Q}, P, R) ->
    {rr, queue:filter(fun({Pid, Ref}) ->
                              not (Pid =:= P andalso Ref =:= R)
                      end, Q)};
remove({mi, As}, P, R) ->
    {mi
     ,[Agent || {Pid, _, Ref}=Agent <- As,
                not (Pid =:= P andalso Ref =:= R)
      ]}.

-type agent_summary_list() :: [{pid(), non_neg_integer()},...] | [].
-spec summary/1 :: (agents()) -> [{'strategy', ne_binary()} |
                                 {'agents', agent_summary_list()}
                                 ,...
                                ].
summary({rr, As}) ->
    [{strategy, <<"round robin">>}
     ,{agents, [{P, 0} || {P, _Ref} <- queue:to_list(As)]}
    ];
summary({mi, As}) ->
    [{strategy, <<"most idle">>}
     ,{agents, list_summary(As)}
    ].

-spec list_summary/1 :: (mi_list()) -> agent_summary_list().
list_summary(L) when is_list(L) ->
    F = fun({P, T, _}, Acc) -> [{P, T} | Acc] end,
    lists:foldl(F, [], L).

-spec reload/2 :: (agents(), [pid(),...]) -> agents().
reload({rr, _}, Ws) ->
    {rr, queue:from_list([begin {W, erlang:monitor(process, W)} end || W <- Ws])};
reload({mi, As}, Ws) ->
    {mi, [{W, idle_time(lists:keyfind(W, 1, As)), erlang:monitor(process, W)} || W <- Ws]}.

-spec idle_time/1 :: ('false' | {pid(),pos_integer(), reference()}) -> pos_integer().
idle_time(false) ->
    wh_util:current_tstamp();
idle_time({_,T, Ref}) ->
    erlang:demonitor(Ref, [flush]),
    T.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec agent_count/1 :: (agents()) -> non_neg_integer().
agent_count({rr, As}) ->
    queue:len(As);
agent_count({mi, As}) ->
    length(As).

rr_next_agent_please_test() ->
    State0 = {rr, queue:new()},
    State1 = reload(State0, [pid1, pid2, pid3]),
    ?assertEqual(agent_count(State1), 3),

    {A1, State2} = next_agent_please(State1),
    {A2, State3} = next_agent_please(State2),
    {A3, State4} = next_agent_please(State3),
    ?assertEqual(agent_count(State4), 3),
    ?assertEqual(pid1, A1),
    ?assertEqual(pid2, A2),
    ?assertEqual(pid3, A3),

    {A4, State5} = next_agent_please(State4),
    {A5, State6} = next_agent_please(State5),
    {A6, State7} = next_agent_please(State6),
    ?assertEqual(agent_count(State7), 3),
    ?assertEqual(A1, A4),
    ?assertEqual(A2, A5),
    ?assertEqual(A3, A6).

mi_next_agent_please_test() ->
    State0 = {mi, []},
    State1 = reload(State0, [pid1, pid2, pid3]),
    ?assertEqual(3, agent_count(State1)),

    State2 = update(State1, pid1, 1),
    State3 = update(State2, pid2, 5),
    State4 = update(State3, pid3, 3),

    {A1, State5} = next_agent_please(State4),
    {A2, State6} = next_agent_please(State5),
    {A3, State7} = next_agent_please(State6),

    ?assertEqual(0, agent_count(State7)),
    ?assertEqual(undefined, next_agent_please(State7)),

    ?assertEqual(pid1, A1),
    ?assertEqual(pid3, A2),
    ?assertEqual(pid2, A3),

    State8 = update(State7, pid1, 9),
    State9 = update(State8, pid2, 11),
    State10 = update(State9, pid3, 3),

    ?assertEqual(3, agent_count(State10)),

    {A4, State11} = next_agent_please(State10),
    {A5, State12} = next_agent_please(State11),
    {A6, State13} = next_agent_please(State12),
    ?assertEqual(0, agent_count(State13)),

    ?assertEqual(undefined, next_agent_please(State13)),

    ?assertEqual(pid3, A4),
    ?assertEqual(pid1, A5),
    ?assertEqual(pid2, A6).



-endif.
