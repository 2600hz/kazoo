%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.com>
%%% @copyright (C) 2010, Karl Anderson
%%% @doc
%%% Responsible for runnning the call server monitoring tasks
%%% @end
%%% Created : 30 Nov 2010 by Karl Anderson <karl@2600hz.com>
%%%-------------------------------------------------------------------
-module(monitor_agent_call).

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([set_amqp_host/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-import(logger, [format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

-include("../include/monitor_amqp.hrl").

-record(state, {
        amqp_host = "" :: string()
        ,agent_q = <<>> :: binary()
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
start_link(AHost) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [AHost], []).

set_amqp_host(AHost) ->
    gen_server:call(?SERVER, {set_amqp_host, AHost}, infinity).

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
init([AHost]) ->
    format_log(info, "MONITOR_AGENT_CALL(~p): Starting server with amqp host ~p~n", [self(), AHost]),
    {ok, Agent_Q} = start_amqp(AHost),
    {ok, #state{amqp_host=AHost, agent_q=Agent_Q}}.

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
handle_call({set_amqp_host, AHost}, _From, #state{amqp_host=CurrentAHost, agent_q=CurrentAgentQ}=State) ->
    format_log(info, "MONITOR_AGENT_CALL(~p): Updating amqp host from ~p to ~p~n", [self(), CurrentAHost, AHost]),
    amqp_util:queue_delete(CurrentAHost, CurrentAgentQ),
    amqp_manager:close_channel(self(), CurrentAHost),
    {ok, Agent_Q} = start_amqp(AHost),
    {reply, ok, State#state{amqp_host=AHost, agent_q=Agent_Q}};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

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
handle_info({'EXIT', _Pid, Reason}, State) ->
    format_log(error, "MONITOR_AGENT_CALL(~p): Received EXIT(~p) from ~p...~n", [self(), Reason, _Pid]),
    {noreply, Reason, State};

%% Spawn tasks to process the incomming requests
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) ->
    spawn(fun() -> handle_req(Props#'P_basic'.content_type, Payload, State) end),
    {noreply, State};

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
terminate(_Reason, #state{amqp_host=AHost, agent_q=Agent_Q}) ->
    amqp_util:queue_delete(AHost, Agent_Q),
    format_log(error, "MONITOR_AGENT_CALL(~p): Killed queue, going down(~p)...~n", [self(), _Reason]),
    ok;

terminate(_Reason, _State) ->
    format_log(error, "MONITOR_AGENT_CALL(~p): Going down(~p)...~n", [self(), _Reason]),
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
start_amqp(AHost) ->
    amqp_util:monitor_exchange(AHost),
    amqp_util:targeted_exchange(AHost),

    Agent_Q = amqp_util:new_monitor_queue(AHost),

    %% Bind the queue to the targeted exchange
    format_log(info, "MONITOR_AGENT_CALL(~p): Bind ~p as a targeted queue~n", [self(), Agent_Q]),
    amqp_util:bind_q_to_targeted(AHost, Agent_Q),

    %% Bind the queue to the topic exchange
    format_log(info, "MONITOR_AGENT_CALL(~p): Bind ~p for ~p~n", [self(), Agent_Q, ?KEY_AGENT_CALL_REQ]),
    amqp_util:bind_q_to_monitor(AHost, Agent_Q, ?KEY_AGENT_CALL_REQ),

    %% Register a consumer to listen to the queue
    format_log(info, "MONITOR_AGENT_CALL(~p): Consume on ~p~n", [self(), Agent_Q]),
    amqp_util:basic_consume(AHost, Agent_Q),

    {ok, Agent_Q}.

create_task_q(AHost) ->
    Q = amqp_util:new_monitor_queue(AHost),

    %% Bind the queue to the targeted exchange
    format_log(info, "MONITOR_AGENT_CALL(~p): Bind ~p as a targeted queue for task~n", [self(), Q]),
    amqp_util:bind_q_to_targeted(AHost, Q),

    %% Register a consumer to listen to the queue
    format_log(info, "MONITOR_AGENT_CALL(~p): Consume on ~p for task~n", [self(), Q]),
    amqp_util:basic_consume(AHost, Q),

    {ok, Q}.

get_msg_type(Prop) ->
    { get_value(<<"Event-Category">>, Prop), get_value(<<"Event-Name">>, Prop) }.

handle_req(ContentType, Payload, State) ->
    case ContentType of
    <<"application/json">> ->
        {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
        format_log(info, "MONITOR_AGENT_CALL(~p): Recv CT: ~p~nPayload: ~p~n", [self(), ContentType, Prop]),
        process_req(get_msg_type(Prop), Prop, State);
    _ ->
        format_log(info, "MONITOR_AGENT_CALL(~p): recieved unknown msg type: ~p~n", [self(), ContentType])
    end.

process_req({<<"task">>, <<"basic_call_req">>}, Prop, #state{amqp_host = AHost}=State) ->
    case monitor_api:basic_call_req_v(Prop) of
    true ->
        Route = [<<"user:2600pbx">>, <<"4158867903">>],
        basic_call(AHost, Prop, Route);
    _ ->
        format_log(error, "MONITOR_AGENT_CALL.basic(~p): Failed to validate basic_call_req~n", [self()])
    end,
    State;

process_req(_MsgType, _Prop, _State) ->
    format_log(info, "MONITOR_AGENT_CALL(~p): Unhandled Msg ~p~nJSON: ~p~n", [self(), _MsgType, _Prop]).

send_resp(JSON, RespQ, AHost) ->
    format_log(info, "MONITOR_AGENT_CALL(~p): Sending reply to ~p~n", [self(), RespQ]),
    amqp_util:targeted_publish(AHost, RespQ, JSON, <<"application/json">>).
    
originate_call_req(Agent_Q, Msg_ID, Route) ->
    Req = [
        {<<"Event-Category">>, <<"originate">>}
       ,{<<"Event-Name">>, <<"resource_req">>}
       ,{<<"Server-ID">>, Agent_Q}
       ,{<<"Msg-ID">>, Msg_ID}
       ,{<<"App-Name">>, <<"monitor">>}
       ,{<<"App-Version">>, <<"0.1.0">>}    
       ,{<<"Resource-Type">>, <<"audio">>}
       ,{<<"Route">>, Route}
    ],
    format_log(info, "MONITOR_AGENT_CALL(~p): Generated originate req~nPayload: ~p~n", [self(), Req]),
    whistle_api:resource_req(Req).

basic_call(AHost, Prop, Route) ->
    Msg_ID = get_value(<<"Msg-ID">>, Prop),
    {ok, Task_Q} = create_task_q(AHost),
    {ok, JSON} = originate_call_req(Task_Q, Msg_ID, Route),
    amqp_util:callmgr_publish(AHost, JSON, <<"application/json">>, ?KEY_RESOURCE_REQ),
    basic_call_loop(AHost, Task_Q).

basic_call_loop(AHost, Task_Q) ->
    receive
        {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->
            {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
            format_log(info, "MONITOR_AGENT_CALL(~p).basic: Recv message~nPayload: ~p~n", [self(), Prop]),
            case get_msg_type(Prop) of
                {<<"originate">>, <<"resource_resp">>} ->
                    CQ = get_value(<<"Control-Queue">>, Prop),
                    Call_ID = get_value(<<"Call-ID">>, Prop),
                    basic_call_dialplan(AHost, Task_Q, CQ, Call_ID),
                    basic_call_loop(AHost, Task_Q);
                _ ->
                    format_log(error, "MONITOR_AGENT_CALL.basic(~p): Failed to validate basic_call_req~n", [self()]),    
                    basic_call_loop(AHost, Task_Q)
            end;
        _ ->
            basic_call_loop(AHost, Task_Q)
    after
        10000 -> {error, timed_out}
    end.

basic_call_dialplan(AHost, Task_Q, CQ, Call_ID) ->
    receive after 1000 -> ok end,
    Req = [
        {<<"Event-Category">>, <<"call_control">>}
       ,{<<"Event-Name">>, <<"command">>}
       ,{<<"Server-ID">>, Task_Q}
       ,{<<"App-Name">>, <<"monitor">>}
       ,{<<"App-Version">>, <<"0.1.0">>}   
       ,{<<"Application-Name">>, <<"tones">>}
       ,{<<"Call-ID">>, Call_ID}
       ,{<<"Tones">>, [
                         {<<"Frequencies">>, <<"500">>} 
                        ,{<<"Duration-ON">>, <<"2000">>}
                        ,{<<"Duration-OFF">>, <<"1000">>}
                      ]
        }
    ],
    format_log(info, "MONITOR_AGENT_CALL.basic(~p): Basic call dialplan~nPayload: ~p~n", [self(), Req]),
    {ok, JSON} = whistle_api:tones_req(Req),
    amqp_util:targeted_publish(AHost, CQ, JSON, <<"application/json">>).
