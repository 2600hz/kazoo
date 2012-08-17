%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Controls how a queue process progresses a member_call
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_queue_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% Event injectors
-export([member_call/2
         ,member_connect_resp/2
         ,member_accepted/2
         ,member_connect_retry/2
         ,call_event/4
        ]).

%% State handlers
-export([ready/2
         ,connect_req/2
         ,connecting/2
        ]).

%% gen_fsm callbacks
-export([init/1
         ,handle_event/3
         ,handle_sync_event/4
         ,handle_info/3
         ,terminate/3
         ,code_change/4
        ]).

-include("acdc.hrl").

-record(state, {
          queue_proc :: pid()
         ,connect_resps = [] :: wh_json:json_objects()
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
member_call(FSM, CallJObj) ->
    gen_fsm:send_event(FSM, {member_call, CallJObj}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
member_connect_resp(FSM, Resp) ->
    gen_fsm:send_event(FSM, {agent_resp, Resp}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
member_accepted(FSM, AcceptJObj) ->
    gen_fsm:send_event(FSM, {accepted, AcceptJObj}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
member_connect_retry(FSM, RetryJObj) ->
    gen_fsm:send_event(FSM, {retry, RetryJObj}).

%%--------------------------------------------------------------------
%% @doc
%%   When a queue is processing a call, it will receive call events.
%%   Pass the call event to the FSM to see if action is needed (usually
%%   for hangup events).
%% @end
%%--------------------------------------------------------------------
-spec call_event/4 :: (pid(), ne_binary(), ne_binary(), wh_json:json_object()) -> 'ok'.
call_event(FSM, <<"call_event">>, <<"CHANNEL_DESTROY">>, EvtJObj) ->
    gen_fsm:send_event(FSM, {member_hungup, EvtJObj});
call_event(_, _, _, _) -> ok.

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
init([]) ->
    {ok, ready, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
ready(_Event, State) ->
    lager:debug("unhandled event: ~p", [_Event]),
    {next_state, ready, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
connect_req(_Event, State) ->
    lager:debug("unhandled event: ~p", [_Event]),
    {next_state, connect_req, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
connecting(_Event, State) ->
    lager:debug("unhandled event: ~p", [_Event]),
    {next_state, connecting, State}.

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
    lager:debug("unhandled event in state ~s: ~p", [StateName, _Event]),
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
    lager:debug("unhandled sync event in ~s: ~p", [StateName, _Event]),
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
    lager:debug("acdc queue fsm terminating: ~p", [_Reason]).

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
