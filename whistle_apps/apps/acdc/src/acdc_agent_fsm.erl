%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Tracks the agent's state, responds to messages from the corresponding
%%% acdc_agent gen_listener process.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_agent_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/3
         ,call_event/4
         ,member_connect_req/2
         ,member_connect_win/2
         ,member_connect_monitor/2
        ]).

%% gen_fsm callbacks
-export([init/1
         ,handle_event/3
         ,handle_sync_event/4
         ,handle_info/3
         ,terminate/3
         ,code_change/4
        ]).

%% Agent states
-export([sync/2
         ,ready/2
         ,ringing/2
         ,answered/2
         ,wrapup/2
         ,paused/2
        ]).

-include("acdc.hrl").

-record(state, {
          acct_id :: ne_binary()
         ,agent_id :: ne_binary()
         ,agent_proc :: pid()
         }).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%%   When a queue receives a call and needs an agent, it will send a 
%%   member_connect_req. The agent will respond (if possible) with a
%%   member_connect_resp payload or ignore the request
%% @end
%%--------------------------------------------------------------------
-spec member_connect_req/2 :: (pid(), wh_json:json_object()) -> 'ok'.
member_connect_req(FSM, JObj) ->
    gen_fsm:send_event(FSM, {member_connect_req, JObj}).

%%--------------------------------------------------------------------
%% @doc
%%   When a queue receives a call and needs an agent, it will send a 
%%   member_connect_req. The agent will respond (if possible) with a
%%   member_connect_resp payload or ignore the request
%% @end
%%--------------------------------------------------------------------
-spec member_connect_win/2 :: (pid(), wh_json:json_object()) -> 'ok'.
member_connect_win(FSM, JObj) ->
    gen_fsm:send_event(FSM, {member_connect_win, JObj}).

%%--------------------------------------------------------------------
%% @doc
%%   When a queue receives a call and needs an agent, it will send a 
%%   member_connect_req. The agent will respond (if possible) with a
%%   member_connect_resp payload or ignore the request
%% @end
%%--------------------------------------------------------------------
-spec member_connect_monitor/2 :: (pid(), wh_json:json_object()) -> 'ok'.
member_connect_monitor(FSM, JObj) ->
    gen_fsm:send_event(FSM, {member_connect_monitor, JObj}).

%%--------------------------------------------------------------------
%% @doc
%%   When an agent is involved in a call, it will receive call events.
%%   Pass the call event to the FSM to see if action is needed (usually
%%   for bridge and hangup events).
%% @end
%%--------------------------------------------------------------------
-spec call_event/4 :: (pid(), ne_binary(), ne_binary(), wh_json:json_object()) -> 'ok'.
call_event(FSM, Cat, Name, JObj) ->
    gen_fsm:send_event(FSM, {call_event, Cat, Name, JObj}).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link/3 :: (ne_binary(), ne_binary(), pid()) -> startlink_ret().
start_link(AcctId, AgentId, AgentProc) ->
    gen_fsm:start_link(?MODULE, [AcctId, AgentId, AgentProc], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([AcctId, AgentId, AgentProc]) ->
    put(callid, <<"fsm_", AcctId/binary, "_", AgentId/binary>>),
    lager:debug("started acdc agent fsm"),
    {ok, sync, #state{acct_id=AcctId
                      ,agent_id=AgentId
                      ,agent_proc=AgentProc
                     }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
sync(_Evt, State) ->
    {next_state, sync, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
ready({member_connect_win, JObj}, #state{agent_proc=Srv}=State) ->
    lager:debug("we won us a member!"),
    acdc_agent:bridge_to_member(Srv, JObj),
    {next_state, ringing, State};
ready({member_connect_req, JObj}, #state{agent_proc=Srv}=State) ->
    acdc_agent:member_connect_resp(Srv, JObj),
    {next_state, ready, State};
ready(_Evt, State) ->
    {next_state, ready, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
ringing({member_connect_req, _}, State) ->
    {next_state, ringing, State};
ringing({member_connect_win, JObj}, #state{agent_proc=Srv}=State) ->
    lager:debug("we won, but can't process this right now"),
    acdc_agent:member_connect_retry(Srv, JObj),
    {next_state, ringing, State};
ringing(_Evt, State) ->
    {next_state, ringing, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
answered({member_connect_req, _}, State) ->
    {next_state, answered, State};
answered({member_connect_win, JObj}, #state{agent_proc=Srv}=State) ->
    lager:debug("we won, but can't process this right now"),
    acdc_agent:member_connect_retry(Srv, JObj),
    {next_state, answered, State};
answered(_Evt, State) ->
    {next_state, answered, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
wrapup({member_connect_req, _}, State) ->
    {next_state, wrapup, State};
wrapup({member_connect_win, JObj}, #state{agent_proc=Srv}=State) ->
    lager:debug("we won, but can't process this right now"),
    acdc_agent:member_connect_retry(Srv, JObj),
    {next_state, wrapup, State};
wrapup(_Evt, State) ->
    {next_state, wrapup, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
paused({member_connect_req, _}, State) ->
    {next_state, paused, State};
paused({member_connect_win, JObj}, #state{agent_proc=Srv}=State) ->
    lager:debug("we won, but can't process this right now"),
    acdc_agent:member_connect_retry(Srv, JObj),
    {next_state, paused, State};
paused(_Evt, State) ->
    {next_state, paused, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    lager:debug("unhandled message in state ~s: ~p", [StateName, _Info]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    lager:debug("acdc agent fsm terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
