%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.com>
%%% @copyright (C) 2010, Karl Anderson
%%% @doc
%%% Responsible for managing the monitoring application
%%% @end
%%% Created : 11 Nov 2010 by Karl Anderson <karl@2600hz.com>
%%%-------------------------------------------------------------------
-module(monitor_master).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([set_amqp_host/1]).
-export([start_job/1, start_job/2, sync_jobs/0, sync_job/1]).
-export([rm_job/1, list_jobs/0, run_job/1]).
-export([set_interval/2, pause/1, resume/1]).
-export([update_task/4, rm_task/2, list_tasks/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_INTERVAL, 300000).

-import(logger, [format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

-include("../include/monitor_amqp.hrl").
-include("../include/monitor_couch.hrl").

-record(state, {
         amqp_host = "" :: string()
        ,monitor_q = <<>> :: binary()
        ,database = ?MONITOR_DB :: string()
        ,db_view = ?MONITOR_VIEW :: string()
        ,jobs = [] :: list()
    }).

-record(job, {
         monitorRef = unknown
        ,processID = unknown
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

start_job(Job_ID) ->
    start_job(Job_ID, ?DEFAULT_INTERVAL).

start_job(Job_ID, Interval) ->
    gen_server:call(?SERVER, {start_job, Job_ID, Interval}, infinity).

sync_jobs() ->
    gen_server:call(?SERVER, {sync_jobs}, infinity).

sync_job(Options) ->
    gen_server:call(?SERVER, {sync_job, Options}, infinity).

rm_job(Job_ID) ->
    gen_server:call(?SERVER, {rm_job, Job_ID}, infinity).

list_jobs() ->
    gen_server:call(?SERVER, {list_jobs}, infinity).

run_job(Job_ID) ->
    gen_server:call(?SERVER, {run_job, Job_ID}, infinity).

set_interval(Job_ID, Interval) ->
    gen_server:call(?SERVER, {set_interval, Job_ID, Interval}, infinity).

pause(Job_ID) ->
    gen_server:call(?SERVER, {pause, Job_ID}, infinity).

resume(Job_ID) ->
    gen_server:call(?SERVER, {resume, Job_ID}, infinity).

update_task(Job_ID, Task_ID, Type, Options) ->
    gen_server:call(?SERVER, {update_task, Job_ID, Task_ID, Type, Options}, infinity).

rm_task(Job_ID, Task_ID) ->
    gen_server:call(?SERVER, {rm_task, Job_ID, Task_ID}, infinity).

list_tasks(Job_ID) ->
    gen_server:call(?SERVER, {list_tasks, Job_ID}, infinity).

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
    couch_mgr:add_change_handler(?MONITOR_DB, <<"">>),
    {ok, #state{amqp_host = AHost}}.

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
handle_call({set_amqp_host, AHost}, _From, #state{amqp_host=CurrentAHost, monitor_q=CurrentMonitorQ}=State) ->
    format_log(info, "MONITOR_MASTER(~p): Updating amqp host from ~p to ~p~n", [self(), CurrentAHost, AHost]),
    update_sup_children(AHost, supervisor:which_children(monitor_job_sup)),
    update_sup_children(AHost, supervisor:which_children(monitor_agent_sup)),
    {reply, ok, State#state{amqp_host=AHost}};

handle_call({start_job, Job_ID, Interval}, _From, #state{amqp_host = AHost, jobs = Jobs} = State) ->
    case get_value(Job_ID, Jobs) of
        #job{processID = Pid} ->
            {reply, {ok, Pid}, State};
        undefined ->
            case monitor_job_sup:start_job(Job_ID, AHost, Interval) of
                {ok, Pid} -> 
                    Job = #job{processID = Pid, monitorRef = monitor(process, Pid)},
                    {reply, {ok, Pid}, State#state{jobs = [{Job_ID, Job}|Jobs]}};
                {error, E} ->
                    {reply, {error, E}, State}
            end
    end;

handle_call({sync_jobs, Options}, _From, #state{jobs = Running_Jobs} = State) ->
    case get_jobs(State, Options) of
        {ok, Jobs} ->
            spawn(fun() -> sync_jobs(Jobs, Running_Jobs) end),
            {reply, {ok, sync_started}, State};
        {error, _Error} = E ->
            {reply, E, State}
    end;    

handle_call({rm_job, Job_ID}, _From, #state{jobs = Jobs} = State) ->
    stop_job(Job_ID, Jobs),
    NewJobs = proplists:delete(Job_ID, Jobs),
    format_log(info, "MONITOR_MASTER(~p): Removed job ~p~n", [self(), Job_ID]),
    {reply, {ok, job_removed}, State#state{jobs = NewJobs}};

handle_call({list_jobs}, _From, #state{jobs = Jobs} = State) ->
    {reply, {ok, Jobs}, State};

handle_call({set_interval, Job_ID, Interval}, _From, #state{jobs = Jobs} = State) ->
    {reply, msg_job(Job_ID, Jobs, {set_interval, Interval}), State};

handle_call({pause, Job_ID}, _From, #state{jobs = Jobs} = State) ->
    {reply, msg_job(Job_ID, Jobs, {pause}), State};

handle_call({resume, Job_ID}, _From, #state{jobs = Jobs} = State) ->
    {reply, msg_job(Job_ID, Jobs, {resume}), State};

handle_call({update_task, Job_ID, Task_ID, Type, Options}, _From, #state{jobs = Jobs} = State) ->
    {reply, msg_job(Job_ID, Jobs, {update_task, Task_ID, Type, Options}), State};

handle_call({rm_task, Job_ID, Task_ID}, _From, #state{jobs = Jobs} = State) ->
    {reply, msg_job(Job_ID, Jobs, {rm_task, Task_ID}), State};

handle_call({list_tasks, Job_ID}, _From, #state{jobs = Jobs} = State) ->
    {reply, msg_job(Job_ID, Jobs, {list_tasks}), State};

handle_call({run_tasks, Job_ID}, _From, #state{jobs = Jobs} = State) ->
    {reply, msg_job(Job_ID, Jobs, {run_tasks}), State};

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
handle_info({'DOWN', _MonitorRef, _Type, _Job_PID, _Info}, State) ->
    %%format_log(error, "MONITOR_MASTER(~p): Job PID ~p died unexpectedly!~n", [self(), Job_PID]),
    {noreply, State};

handle_info({document_deleted, Doc_ID}, #state{jobs = Jobs} = State) ->
    Job_ID = monitor_util:to_binary(Doc_ID),
    spawn(fun() -> case proplists:is_defined(Job_ID, Jobs) of true -> rm_job(Job_ID); _ -> ok end end),
    {noreply, State};
    
handle_info({document_changes, Doc_ID, _Changes}, #state{db_view = View, database = DB, jobs = Jobs} = State) ->
    Job_ID = monitor_util:to_binary(Doc_ID),
    case couch_mgr:get_results(DB, View, [{"key", Job_ID}]) of
        [] ->
            spawn(fun() -> case proplists:is_defined(Job_ID, Jobs) of true -> rm_job(Job_ID); _ -> ok end end);
        [{Job}] ->
            spawn(fun() -> sync_job(Job, Jobs) end);
        _ ->
            ok
    end,
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
terminate(_Reason, _State) ->
    format_log(error, "MONITOR_MASTER(~p): Going down(~p)...~n", [self(), _Reason]),
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

    Monitor_Q = amqp_util:new_monitor_queue(AHost),

    %% Bind the queue to the targeted exchange
    format_log(info, "MONITOR_MASTER(~p): Bind ~p as a targeted queue~n", [self(), Monitor_Q]),
    amqp_util:bind_q_to_targeted(AHost, Monitor_Q),

    %% Bind the queue to an exchange
    format_log(info, "MONITOR_MASTER(~p): Bind ~p for ~p~n", [self(), Monitor_Q, ?KEY_MONITOR_MASTER_REQ]),
    amqp_util:bind_q_to_monitor(AHost, Monitor_Q, ?KEY_MONITOR_MASTER_REQ),

    %% Register a consumer to listen to the queue
    format_log(info, "MONITOR_MASTER(~p): Consume on ~p~n", [self(), Monitor_Q]),
    amqp_util:basic_consume(AHost, Monitor_Q),

    {ok, Monitor_Q}.

get_jobs(#state{database = DB, db_view = View}, Options) ->
    case couch_mgr:get_results(DB, View, Options) of
        false ->
            format_log(error, "MONITOR_MASTER(~p): Sync jobs missing view ~p~n", [self(), View]),
            {error, missing_view};
        {error, not_found} ->
            format_log(info, "MONITOR_MASTER(~p): Sync jobs result not found~n", [self()]),
            {error, not_found};
        Jobs ->
            format_log(info, "MONITOR_MASTER(~p): Found ~p jobs in the database~n", [self(), length(Jobs)]),
            {ok, Jobs}
    end.

sync_jobs([], []) ->
    sync_complete;
sync_jobs([], [{Job_ID, _Job}|T]) ->
    rm_job(Job_ID),
    sync_jobs([], T);
sync_jobs([{Props}|T], Running) ->
    Job_ID = get_value(<<"id">>, Props),
    sync_job(Props, Running),
    sync_jobs(T, proplists:delete(Job_ID, Running)).

sync_job(Props, Running) ->
    Job_ID = get_value(<<"id">>, Props),
    {Job} = get_value(<<"value">>, Props),
    Interval = get_value(<<"interval">>, Job, ?DEFAULT_INTERVAL),
    %% Tasks = get_value(<<"tasks">>, Job, []),
    case proplists:is_defined(Job_ID, Running)  of
        true ->
            set_interval(Job_ID, Interval);
        _ ->
            start_job(Job_ID, Interval)
    end.

msg_job(Job_ID, Jobs, Msg) ->
    case get_value(Job_ID, Jobs) of
        #job{processID = Pid} ->
            {ok, gen_server:call(Pid, Msg, infinity)};
        undefined ->
            {error, job_not_found}
    end.

stop_job(Job_ID, Jobs) ->
    case get_value(Job_ID, Jobs) of
        #job{processID = Pid} ->
            {ok, Pid ! stop};
        undefined ->
            {error, job_not_found}
    end.

update_sup_children(_, []) ->
    ok;
update_sup_children(AHost, [{_,Pid,_,_}|T])->
    gen_server:call(Pid, {set_amqp_host, AHost}, infinity),    
    update_sup_children(AHost, T).
