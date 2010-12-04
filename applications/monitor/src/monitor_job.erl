%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.com>
%%% @copyright (C) 2010, Karl Anderson
%%% @doc
%%% Responsible for managing the monitoring application
%%% @end
%%% Created : 11 Nov 2010 by Karl Anderson <karl@2600hz.com>
%%%-------------------------------------------------------------------
-module(monitor_job).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_INTERVAL, 10000).

-import(logger, [format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

-include("../include/monitor_amqp.hrl").

-record(state, {
        amqp_host = "" :: string()
        ,job_id = "" :: string()
        ,job_q = <<>> :: binary()
        ,tref
        ,tasks = []
        ,iteration = 0
        ,interval = ?DEFAULT_INTERVAL
    }).

-record(task, {
        type = "" :: string()
        ,options = []
        ,iteration = 0 
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
start_link(Job_ID, Tasks, AHost) ->
    gen_server:start_link(?MODULE, [monitor_util:to_list(Job_ID), Tasks, AHost], []).

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
init([Job_ID, Tasks, AHost]) ->
    format_log(info, "MONITOR_JOB(~p): Starting new job with id ~p and amqp host ~p~n", [self(), Job_ID, AHost]),
    {ok, Job_Q} = start_amqp(AHost, Job_ID),
    {ok, TRef} = timer:send_interval(?DEFAULT_INTERVAL, {heartbeat}),
    {ok, #state{amqp_host=AHost, job_id=Job_ID, job_q=Job_Q, tref=TRef, tasks=Tasks}}.

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
handle_call({set_amqp_host, AHost}, _From, #state{amqp_host=CurrentAHost, job_q=CurrentMonitorQ, job_id=Job_ID}=State) ->
    format_log(info, "MONITOR_JOB(~p): Updating amqp host from ~p to ~p~n", [self(), CurrentAHost, AHost]),
    amqp_util:queue_delete(CurrentAHost, CurrentMonitorQ),
    amqp_manager:close_channel(self(), CurrentAHost),
    {ok, Job_Q} = start_amqp(AHost, Job_ID),
    {reply, amqp_host_updated, State#state{amqp_host=AHost, job_q=Job_Q}};

handle_call({set_interval, Interval}, _From, #state{tref=CurrentTRef, job_id=Job_ID}=State) ->
    timer:cancel(CurrentTRef),
    {ok, TRef} = timer:send_interval(Interval, {heartbeat}), 
    format_log(info, "MONITOR_JOB(~p): Job ~p updated the interval to ~p~n", [self(), Job_ID, Interval]), 
    {reply, interval_set, State#state{tref=TRef, interval=Interval}};

handle_call({pause}, _From, #state{tref=TRef, job_id=Job_ID}=State) ->
    timer:cancel(TRef),
    format_log(info, "MONITOR_JOB(~p): Job ~p has been paused~n", [self(), Job_ID]), 
    {reply, paused, State#state{tref=""}};

handle_call({resume}, _From, #state{tref=CurrentTRef, interval=Interval, job_id=Job_ID}=State) ->
    timer:cancel(CurrentTRef),
    {ok, TRef} = timer:send_interval(Interval, {heartbeat}),
    format_log(info, "MONITOR_JOB(~p): Job ~p has been resumed with an interval of ~p~n", [self(), Job_ID, Interval]), 
    {reply, resumed, State#state{tref=TRef}};

handle_call({add_task, Name, Type, Options}, _From, #state{tasks=Tasks, job_id=Job_ID}=State) ->
    Task = #task{type=Type, options=Options},
    format_log(info, "MONITOR_JOB(~p): Job ~p added a new task~n~p~n", [self(), Job_ID, Task]), 
    {reply, task_added, State#state{tasks=[{Name, Task}|Tasks]}};

handle_call({rm_task, Name}, _From, #state{tasks=Tasks, job_id=Job_ID}=State) ->
    NewTasks = proplists:delete(Name, Tasks),
    format_log(info, "MONITOR_JOB(~p): Job ~p removed task ~p~n", [self(), Job_ID, Name]), 
    {reply, task_removed, State#state{tasks=NewTasks}};

handle_call({list_tasks}, _From, #state{tasks=Tasks}=State) ->
    {reply, Tasks, State};

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
handle_info(stop, State) ->
    {stop, normal, State};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    format_log(error, "MONITOR_JOB(~p): Received EXIT(~p) from ~p...~n", [self(), _Reason, _Pid]),
    {stop, normal, State};

%% Spawn tasks to process the incomming responses
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) ->
    spawn(fun() -> handle_resp(Props#'P_basic'.content_type, Payload, State) end),
    {noreply, State};

handle_info({heartbeat}, #state{tasks=Tasks, job_id=Job_ID}=State) ->
    format_log(info, "MONITOR_JOB(~p): Job ~p woke up by timer~n", [self(), Job_ID]), 
    {noreply, run_tasks(Tasks, State)};

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
terminate(_Reason, #state{amqp_host=AHost, job_q=Job_Q}) ->
    amqp_util:queue_delete(AHost, Job_Q),
    format_log(info, "MONITOR_JOB(~p): Killed queue, going down(~p)...~n", [self(), _Reason]),
    ok;

terminate(_Reason, _State) ->
    format_log(info, "MONITOR_JOB(~p): Going down(~p)...~n", [self(), _Reason]),
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
start_amqp(AHost, Job_ID) ->
    Job_Key = monitor_util:to_binary("monitor.job." ++ Job_ID),

    amqp_util:monitor_exchange(AHost),
    amqp_util:targeted_exchange(AHost),

    Job_Q = amqp_util:new_monitor_queue(AHost),

    %% Bind the queue to the targeted exchange
    format_log(info, "MONITOR_JOB(~p): Bind ~p as a targeted queue~n", [self(), Job_Q]),
    amqp_util:bind_q_to_targeted(AHost, Job_Q),

    %% Bind the queue to an exchange
    format_log(info, "MONITOR_JOB(~p): Bind ~p for ~p~n", [self(), Job_Q, Job_Key]),
    amqp_util:bind_q_to_monitor(AHost, Job_Q, Job_Key),

    %% Register a consumer to listen to the queue
    format_log(info, "MONITOR_JOB(~p): Consume on ~p~n", [self(), Job_Q]),
    amqp_util:basic_consume(AHost, Job_Q),

    {ok, Job_Q}.

type_to_routing_key(Type) ->
    case Type of 
        "ping_net_req" -> ?KEY_AGENT_NET_REQ;
        "option_sip_req" -> ?KEY_AGENT_SIP_REQ;
        "basic_call_req" -> ?KEY_AGENT_CALL_REQ;
        _ -> undefined
    end.

run_tasks([], State) ->
    State;
run_tasks([{Name, Task}|T], #state{amqp_host=AHost, job_id=Job_ID, job_q=Job_Q, tasks=Tasks}=State)->
    format_log(info, "MONITOR_JOB(~p): Job ~p executing task ~p~n~p~n", [self(), Job_ID, Name, Task]),
    NewState = case create_req(Task, Job_Q, Name, Job_ID) of
        {ok, JSON} -> 
            send_req(AHost, JSON, type_to_routing_key(Task#task.type)),
            State#state{tasks=[{Name, Task#task{iteration=Task#task.iteration+1}} | proplists:delete(Name, Tasks)]};
        {error, Error} -> 
            format_log(error, "MONITOR_JOB(~p): Create task request error ~p~n ", [self(), Error]),
            State
    end,
    run_tasks(T, NewState).

create_req(Task, Job_Q, Name, Job_ID) ->
    Defaults = monitor_api:default_headers(Job_Q, <<"task">>, monitor_util:to_binary(Task#task.type)),
    Details = monitor_api:optional_default_headers(Job_ID, Name, Task#task.iteration),
    Headers = monitor_api:prepare_amqp_prop([Details, Defaults, Task#task.options]),
    apply(monitor_api, list_to_atom(Task#task.type), [Headers]).

send_req(AHost, JSON, RoutingKey) ->
    format_log(info, "MONITOR_JOB(~p): Sending request to monitor queue on ~p with key ~p~n", [self(), AHost, RoutingKey]),
    amqp_util:monitor_publish(AHost, JSON, <<"application/json">>, RoutingKey).

handle_resp(ContentType, Payload, _State) ->
    case ContentType of
    <<"application/json">> ->
        {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
        format_log(info, "MONITOR_JOB(~p): Recv CT: ~p~nPayload: ~p~n", [self(), ContentType, Prop]);
        %% process_req(get_msg_type(Prop), Prop, State);
    _ ->
        format_log(info, "MONITOR_JOB(~p): recieved unknown msg type: ~p~n", [self(), ContentType])
    end.
