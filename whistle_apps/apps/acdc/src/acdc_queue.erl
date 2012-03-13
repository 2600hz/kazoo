%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Our connection to AMQP and how we handle what payloads we want to
%%% receive, and what module/functions should handle those payloads
%%% when received.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_queue).

-behaviour(gen_listener).

%% API
-export([start_link/2, handle_new_member/2, handle_call_event/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
         ,terminate/2, code_change/3]).

-include("acdc.hrl").

-record(state, {
          acct_db :: ne_binary()
         ,queue_id :: ne_binary()
         ,queue :: wh_json:json_object()
         ,agents :: queue()
         }).

%% By convention, we put the options here in macros, but not required.
-define(RESPONDERS, [
                     {{?MODULE, handle_call_event}, [{<<"call_event">>, <<"*">>}]}
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(ROUTE_OPTIONS, []).

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

start_link(AcctDb, QueueId) ->
    gen_listener:start_link(?MODULE, [
                                      {bindings, []}
                                      ,{responders, ?RESPONDERS}
                                      ,{queue_name, ?QUEUE_NAME}       % optional to include
                                      ,{queue_options, ?QUEUE_OPTIONS} % optional to include
                                      ,{route_options, ?ROUTE_OPTIONS} % optional to include
                                      ,{basic_qos, 1}                % only needed if prefetch controls
                                     ], [AcctDb, QueueId]).

handle_new_member(Srv, JObj) ->
    gen_listener:cast(Srv, {new_member, JObj}).

handle_call_event(JObj, Props) ->
    gen_listener:cast(props:get_value(server, Props), {call_event, JObj}).

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
init([AcctDb, QueueId]) ->
    put(callid, ?LOG_SYSTEM_ID),
    lager:debug("acdc listener starting for ~s/~s", [AcctDb, QueueId]),

    gen_listener:cast(self(), reload_agents),

    {ok, Queue} = couch_mgr:open_doc(AcctDb, QueueId),

    {ok, #state{
       acct_db=AcctDb
       ,queue_id=QueueId
       ,queue=Queue
      }}.

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
handle_cast(reload_agents, #state{acct_db=AcctDb, queue_id=QueueId}=State) ->
    case acdc_util:get_agents(AcctDb, QueueId) of
        {ok, Agents} -> {noreply, State#state{agents=queue:from_list(Agents)}};
        {error, _} -> {noreply, State}
    end;
handle_cast({new_member, JObj}, #state{agents=Agents}=State) ->
    gen_listener:add_binding(self(), call, [{restrict_to, [events, cdr]}
                                            ,{callid, wh_json:get_value(<<"Call-ID">>, JObj)}
                                           ]),
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj),
    Agents1 = connect_member(Call, Agents),
    {noreply, State#state{agents=Agents1}}.

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
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
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
    lager:debug("listener terminating: ~p", [_Reason]).

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
connect_member(Call, Agents) ->
    {{value, Agent}, Agents1} = queue:out(Agents),
    case try_connect_agent(Call, Agent) of
        {connected, AgentId} ->
            queue:in({AgentId, Call}, Agents1);
        {failure, AgentId, Status} ->
            queue:in({AgentId, Status}, Agents1)
    end.

try_connect_agent(Call, {AgentId, <<"login">>}) ->
    Endpoints = acdc_util:get_endpoints(AgentId),
    case whapps_call_command:b_bridge(Endpoints, Call) of
        ok ->
            ok
    end.
