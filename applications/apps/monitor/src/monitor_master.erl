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
-export([start_link/1, load/0]).
-export([set_amqp_host/1]).
-export([start_job/1, start_job/2, rm_job/1, list_jobs/0]).
-export([set_interval/2, pause/1, resume/1]).
-export([add_task/4, rm_task/2, list_tasks/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-import(logger, [format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

-include("../include/monitor_amqp.hrl").
-include("../include/monitor_couch.hrl").

-record(state, {
         amqp_host = "" :: string()
        ,monitor_q = <<>> :: binary()
        ,jobs = []
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
    start_job(Job_ID, []).

start_job(Job_ID, Tasks) ->
    gen_server:call(?SERVER, {start_job, Job_ID, Tasks}, infinity).

rm_job(Job_ID) ->
    gen_server:call(?SERVER, {rm_job, Job_ID}, infinity).

list_jobs() ->
    gen_server:call(?SERVER, {list_jobs}, infinity).

set_interval(Job_ID, Interval) ->
    gen_server:call(?SERVER, {set_interval, Job_ID, Interval}, infinity).

pause(Job_ID) ->
    gen_server:call(?SERVER, {pause, Job_ID}, infinity).

resume(Job_ID) ->
    gen_server:call(?SERVER, {resume, Job_ID}, infinity).

add_task(Job_ID, Name, Type, Options) ->
    gen_server:call(?SERVER, {add_task, Job_ID, Name, Type, Options}, infinity).

rm_task(Job_ID, Name) ->
    gen_server:call(?SERVER, {rm_task, Job_ID, Name}, infinity).

list_tasks(Job_ID) ->
    gen_server:call(?SERVER, {list_tasks, Job_ID}, infinity).

load() ->
    couch_mgr:set_host("whistle-erl001-fmt.2600hz.org"),
    case couch_mgr:get_results(?TS_DB, ?TS_VIEW_MONITOR, []) of
    false ->
        format_log(error, "MONITOR_MASTER(~p): Missing view ~p~n", [self(), ?TS_VIEW_MONITOR]),
        false;
    {error, not_found} ->
        format_log(info, "MONITOR_MASTER(~p): Something was not found~n", [self()]),
        false;
    [] ->
        format_log(info, "MONITOR_MASTER(~p): No monitoring required~n", [self()]),
        false;  
    [{ViewProp} | _Rest] ->
        Job_ID = get_value(<<"key">>, ViewProp, []),
        Tasks = get_value(<<"value">>, ViewProp, []),
        start_job(Job_ID, []),
        load_jobs(Tasks, Job_ID),
        true;
    _Else ->
        format_log(error, "TS_AUTH(~p): Got something unexpected~n~p~n", [self(), _Else]),
        false
    end.

load_jobs([], _Job_ID) ->
    ok;
load_jobs([{H}|T], Job_ID) ->
    Task_ID = whistle_util:to_list(get_value(<<"task_id">>, H, <<"unknown">>)),
    Type = whistle_util:to_list(get_value(<<"type">>, H, <<"unknown">>)),
    {Options} = get_value(<<"options">>, H, <<"unknown">>),
    gen_server:call(?SERVER, {add_task, Job_ID, Task_ID, Type, Options}, infinity),
    load_jobs(T, Job_ID).


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

handle_call({start_job, Job_ID, Tasks}, _From, #state{amqp_host = AHost, jobs = Jobs} = State) ->
    case get_value(Job_ID, Jobs) of
        #job{processID = Pid} ->
            {reply, {ok, Pid}, State};
        undefined ->
    case monitor_job_sup:start_job(Job_ID, Tasks, AHost) of
        {ok, Pid} -> 
            %%Job = #job{processID = Pid, monitorRef = monitor(process, Pid)},
            Job = #job{processID = Pid},
            {reply, {ok, Pid}, State#state{jobs = [{Job_ID, Job}|Jobs]}};
        {error, E} ->
            {reply, {error, E}, State}
    end
    end;

handle_call({rm_job, Job_ID}, _From, #state{jobs = Jobs} = State) ->
    stop_job(Job_ID, Jobs),
    NewJobs = proplists:delete(Job_ID, Jobs),
    format_log(info, "MONITOR_MASTER(~p): Removed job ~p~n", [self(), Job_ID]),
    {reply, {ok, job_removed}, State#state{jobs = NewJobs}};

handle_call({list_jobs}, _From, #state{jobs = Jobs} = State) ->
    {reply, Jobs, State};

handle_call({set_interval, Job_ID, Interval}, _From, #state{jobs = Jobs} = State) ->
    {reply, msg_job(Job_ID, Jobs, {set_interval, Interval}), State};

handle_call({pause, Job_ID}, _From, #state{jobs = Jobs} = State) ->
    {reply, msg_job(Job_ID, Jobs, {pause}), State};

handle_call({resume, Job_ID}, _From, #state{jobs = Jobs} = State) ->
    {reply, msg_job(Job_ID, Jobs, {resume}), State};

handle_call({add_task, Job_ID, Name, Type, Options}, _From, #state{jobs = Jobs} = State) ->
    {reply, msg_job(Job_ID, Jobs, {add_task, Name, Type, Options}), State};

handle_call({rm_task, Job_ID, Name}, _From, #state{jobs = Jobs} = State) ->
    {reply, msg_job(Job_ID, Jobs, {rm_task, Name}), State};

handle_call({list_tasks, Job_ID}, _From, #state{jobs = Jobs} = State) ->
    {reply, msg_job(Job_ID, Jobs, {list_tasks}), State};

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
