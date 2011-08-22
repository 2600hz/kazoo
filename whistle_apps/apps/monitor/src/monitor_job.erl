%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Responsible for managing the monitoring application
%%% @end
%%% Created : 11 Nov 2010 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(monitor_job).

-behaviour(gen_server).

-include("monitor_amqp.hrl").
-include("monitor_couch.hrl").

-import(logger, [format_log/3]).
-import(props, [get_value/2, get_value/3]).
-import(timer, [send_interval/2, cancel/1]).
-import(wh_util, [to_list/1, to_binary/1]).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).

-record(state, {
         amqp_host  = false   :: string() | false
        ,job_id     = false   :: binary() | false
        ,tref       = false   :: reference() | false
        ,tasks      = []      :: list()
        ,iteration  = 0       :: pos_integer()
        ,interval   = 300000  :: pos_integer()
    }).

-record(task, {
         type       = false   :: string() | false
        ,options    = []      :: list()
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
start_link(Job_ID, AHost, Interval) ->
    gen_server:start_link(?MODULE, [Job_ID, AHost, Interval], []).

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
init([Job_ID, AHost, Interval]) ->
    format_log(info, "MONITOR_JOB(~p): Starting new job with id ~p and amqp host ~p on a interval of ~p", [self(), Job_ID, AHost, Interval]),
    {ok, TRef} = send_interval(Interval, iteration_cycle),
    {ok, #state{amqp_host = AHost, job_id = Job_ID, tref = TRef, interval = Interval}}.

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
handle_call({set_amqp_host, AHost}, _From, #state{job_id = Job_ID} = State) ->
    format_log(info, "MONITOR_JOB(~p): Job ~p updated Updating amqp host to ~p", [self(), Job_ID, AHost]),
    {reply, amqp_host_updated, State#state{amqp_host = AHost}};

handle_call({set_interval, Interval}, _From, #state{tref = CurTRef, job_id = Job_ID, interval = CurInterval} = State)  when Interval /= CurInterval->
    cancel(CurTRef),
    {ok, TRef} = send_interval(Interval, iteration_cycle), 
    format_log(info, "MONITOR_JOB(~p): Job ~p updated the interval to ~p", [self(), Job_ID, Interval]), 
    {reply, interval_set, State#state{tref = TRef, interval = Interval}};

handle_call({pause}, _From, #state{tref = TRef, job_id = Job_ID} = State) ->
    cancel(TRef),
    format_log(info, "MONITOR_JOB(~p): Job ~p has been paused", [self(), Job_ID]), 
    {reply, paused, State#state{tref = false}};

handle_call({resume}, _From, #state{tref = CurrentTRef, interval = Interval, job_id = Job_ID} = State) ->
    cancel(CurrentTRef),
    {ok, TRef} = send_interval(Interval, iteration_cycle),
    format_log(info, "MONITOR_JOB(~p): Job ~p has been resumed with an interval of ~p", [self(), Job_ID, Interval]), 
    {reply, resumed, State#state{tref = TRef}};

handle_call({update_task, Task_ID, Type, Options}, _From, #state{tasks = Tasks, job_id = Job_ID} = State) ->
    Task = #task{type = Type, options = Options},
    UpdatedTasks = proplists:delete(Task_ID, Tasks),
    format_log(info, "MONITOR_JOB(~p): Job ~p updated task ~p", [self(), Job_ID, Task]), 
    {reply, task_updated, State#state{tasks = [{Task_ID, Task}|UpdatedTasks]}};

handle_call({rm_task, Task_ID}, _From, #state{tasks = Tasks, job_id = Job_ID} = State) ->
    NewTasks = proplists:delete(Task_ID, Tasks),
    format_log(info, "MONITOR_JOB(~p): Job ~p removed task ~p", [self(), Job_ID, Task_ID]), 
    {reply, task_removed, State#state{tasks = NewTasks}};

handle_call({list_tasks}, _From, #state{tasks = Tasks} = State) ->
    {reply, Tasks, State};

handle_call({run_job}, _From, #state{tref = CurTRef, interval = Interval} = State) ->
    {ok, TRef} = case cancel(CurTRef) of
        {ok, cancel} ->
            send_interval(Interval, iteration_cycle);
        _ ->
            {ok, CurTRef}
    end,
    spawn_link(fun() -> run_job(State) end),
    {reply, cycle_started, State#state{tref = TRef}};

handle_call({sync, Job}, _From, #state{job_id = Job_ID, tref = CurTRef, interval = CurInterval} = State) ->
    {{ok, TRef}, Interval} = case wh_json:get_value(["interval"], Job) of
                                 undefined ->
                                     {{ok, CurTRef}, CurInterval};
                                 JobInterval when JobInterval /= CurInterval ->
                                     cancel(CurTRef),
                                     {send_interval(JobInterval, iteration_cycle), JobInterval};
                                 _ ->
                                     {{ok, CurTRef}, CurInterval}
                             end,
    Tasks = lists:foldl(fun({struct, Task}, TasksIn) -> 
                                Task_ID = to_list(get_value(<<"task_id">>, Task)),
                                Type    = to_list(get_value(<<"type">>, Task)),
                                {struct, Opt} = get_value(<<"options">>, Task, ?EMPTY_JSON_OBJECT),
                                [{Task_ID, #task{type = Type, options = Opt}} | TasksIn]
                        end, [], wh_json:get_value(["tasks"], Job, [])),
    format_log(info, "MONITOR_JOB(~p): Job ~p imported ~p tasks for execution every ~p", [self(), Job_ID, length(Tasks), Interval]),
    {reply, ok, State#state{tref = TRef, interval = Interval, tasks = Tasks}};

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
    format_log(error, "MONITOR_JOB(~p): Received EXIT(~p) from ~p...", [self(), _Reason, _Pid]),
    {stop, normal, State};

handle_info(iteration_cycle, #state{tasks = []} = State) ->
    {stop, normal, State};
    
handle_info(iteration_cycle, #state{job_id = Job_ID, iteration = Iteration} = State) ->
    format_log(info, "MONITOR_JOB(~p): Job ~p woke up", [self(), Job_ID]),
    spawn_link(fun() -> run_job(State) end),
    {noreply, State#state{iteration = Iteration + 1}};

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
terminate(_Reason, #state{tref = TRef, job_id = Job_ID}) ->
    cancel(TRef),
    format_log(info, "MONITOR_JOB(~p): Job ~p going down (~p)...", [self(), Job_ID, _Reason]),
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures there is a targeted exchange, then binds an anonymous
%% queue to it with a consumer.
%%
%% @spec(create_job_q/1 :: (AHost :: string()) -> tuple(ok, binary())).
%% @end
%%--------------------------------------------------------------------
create_job_q(AHost) ->
    amqp_util:targeted_exchange(AHost),
    Q = amqp_util:new_monitor_queue(AHost),
    %% Bind the queue to the targeted exchange
    amqp_util:bind_q_to_targeted(AHost, Q),
    %% Register a consumer to listen to the queue
    amqp_util:basic_consume(AHost, Q),
    {ok, Q}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Maps job types to the appropriate routing keys in the monitor
%% exchange
%%
%% @spec(type_to_routing_key/1 :: (Type :: string()) -> binary() | undefined).
%% @end
%%--------------------------------------------------------------------
type_to_routing_key(Type) ->
    case Type of 
        "ping_net_req" -> ?KEY_AGENT_NET_REQ;
        "option_sip_req" -> ?KEY_AGENT_SIP_REQ;
        "basic_call_req" -> ?KEY_AGENT_CALL_REQ;
        _ -> undefined
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sequencer for an iteration cycle
%%
%% @spec(run_job/1 :: (State :: #state{}) -> no_return()).
%% @end
%%--------------------------------------------------------------------
run_job(#state{amqp_host = AHost, tasks = Tasks, job_id = Job_ID, iteration = Iteration}) ->
    {ok, Job_Q} = create_job_q(AHost),
    Started     = start_tasks(Tasks, AHost, Job_Q, Job_ID, Iteration, []),
    Default     = monitor_api:default_headers(Job_Q, <<"log">>, <<"job_completion">>),
    Headers     = lists:append([Default, [{<<"Success">>, <<"true">>}]]),
    Resp        = wait_for_tasks(Started, Headers),
    %% Convert Resp to JSON
    %% Send JSON
    amqp_util:queue_delete(AHost, Job_Q),
    {ok, FileId} = file:open("/tmp/" ++ "monitor_task_" ++ binary_to_list(Job_ID) ++ ".txt", [append]),
    io:fwrite(FileId, "~s~n", [mochijson2:encode({struct, Resp})]),
    file:close(FileId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates and sends the appropriate AMQP requests out to agents for a
%% list of agents, returning a proplist of started tasks 
%%
%% @spec(start_tasks/6 :: (Tasks :: proplist(), AHost :: string(), 
%%      Job_Q :: binary(), Job_ID :: string(), Iteration :: pos_integer(), 
%%      Started :: proplist()) -> proplist()).
%% @end
%%--------------------------------------------------------------------
start_tasks([], _AHost, _Job_Q, _Job_ID, _Iteration, Started) ->
    Started;
start_tasks([{Task_ID, Task}|T], AHost, Job_Q, Job_ID, Iteration, Started) ->
    case create_req(Task, Job_Q, Task_ID, Job_ID, Iteration) of
        {ok, JSON} -> 
            send_req(AHost, JSON, type_to_routing_key(Task#task.type)),
            start_tasks(T, AHost, Job_Q, Job_ID, Iteration, [{to_binary(Task_ID), Task}|Started]);
        {error, Error} -> 
            format_log(error, "MONITOR_JOB(~p): Create task request error ~p", [self(), Error]),
            start_tasks(T, AHost, Job_Q, Job_ID, Iteration, Started)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Enters a recieve loop waiting for matching responses to the proplist of Tasks,
%% will time out if no message is received in one minute.  Returns a prolist
%% intended to be placed directly on AMQP.
%%
%% @spec(wait_for_tasks/2 :: (Tasks :: proplist(), Resp :: proplist()) -> no_return()).
%% @end
%%--------------------------------------------------------------------
wait_for_tasks([], Resp) ->
    Resp;
wait_for_tasks(Tasks, Resp) ->
    receive
        {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->
            {struct, Msg} = mochijson2:decode(binary_to_list(Payload)),
            StillPending  = proplists:delete(get_value(<<"Task-Name">>, Msg), Tasks),
            TaskReply     = [{struct, monitor_api:extract_nondefault(Msg)}],
            TasksReply    = lists:append([get_value(<<"Tasks-Reply">>, Resp, []), TaskReply]),
            UpdatedResp   = monitor_util:prop_update(<<"Tasks-Reply">>, TasksReply, Resp),
            case get_value(<<"Success">>, Msg) of
                <<"true">> ->
                    wait_for_tasks(StillPending, UpdatedResp);
                _ -> 
                    wait_for_tasks(StillPending, monitor_util:prop_update(<<"Success">>, <<"false">>, UpdatedResp))
            end
    after
        60000 ->
            wait_for_tasks([], monitor_util:prop_update(<<"Success">>, <<"false">>, Resp))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates the necessary JSON request for arbitrary Tasks
%%
%% @spec(create_req/5 :: (Tasks :: proplist(), Job_Q :: binary(), Task_ID :: string(), 
%%        Job_ID :: binary, Iteration :: pos_integer()) -> tuple(ok, iolist()) | tuple(error, string())).
%% @end
%%--------------------------------------------------------------------
create_req(Task, Job_Q, Task_ID, Job_ID, Iteration) ->
    Default = monitor_api:default_headers(Job_Q, <<"task">>, to_binary(Task#task.type)),
    Details = monitor_api:optional_default_headers(Job_ID, Task_ID, Iteration),
    Headers = monitor_api:prepare_amqp_prop([Details, Default, Task#task.options]),
    apply(monitor_api, list_to_atom(Task#task.type), [Headers]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Places the given JSON into an AMQP payload on the provided hosts
%% monitor exchange for queues matchiong the routing key.
%%
%%@spec(send_req/3 :: (JSON :: iolist(), RespQ :: binary(), RoutingKey :: binary()) -> no_return()).
%% @end
%%--------------------------------------------------------------------
send_req(AHost, JSON, RoutingKey) ->
    amqp_util:monitor_publish(AHost, JSON, <<"application/json">>, RoutingKey).
