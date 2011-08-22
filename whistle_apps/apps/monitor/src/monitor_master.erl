%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Responsible for managing the monitoring application
%%% @end
%%% Created : 11 Nov 2010 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(monitor_master).

-behaviour(gen_server).

-include("monitor_amqp.hrl").
-include("monitor_couch.hrl").

-import(logger, [format_log/3]).
-import(proplists, [get_value/2, get_value/3]).
-import(wh_util, [to_binary/1]).

%% API
-export([start_link/1]).
-export([set_amqp_host/1]).
-export([start_job/1, start_job/2, sync_jobs/0, sync_job/1]).
-export([rm_job/1, list_jobs/0, run_job/1]).
-export([set_interval/2, pause/1, resume/1]).
-export([update_task/4, rm_task/2, list_tasks/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_INTERVAL, 300000).

-record(state, {
         amqp_host   = false          :: string() | false
        ,monitor_q   = false          :: binary() | false
        ,database    = ?MONITOR_DB    :: string()
        ,db_view     = ?MONITOR_VIEW  :: string()
        ,jobs        = []             :: list()
    }).

-record(job, {
         monitorRef  = false          :: reference() | false
        ,processID   = false          :: reference() | false
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
    gen_server:call(?SERVER, {start_job, to_binary(Job_ID), Interval}, infinity).

sync_jobs() ->
    gen_server:call(?SERVER, {sync_jobs}, infinity).

sync_job(Job_ID) ->
    gen_server:call(?SERVER, {sync_job, to_binary(Job_ID)}, infinity).

rm_job(Job_ID) ->
    gen_server:call(?SERVER, {rm_job, to_binary(Job_ID)}, infinity).

list_jobs() ->
    gen_server:call(?SERVER, {list_jobs}, infinity).

run_job(Job_ID) ->
    gen_server:call(?SERVER, {run_job, to_binary(Job_ID)}, infinity).

set_interval(Job_ID, Interval) ->
    gen_server:call(?SERVER, {set_interval, to_binary(Job_ID), Interval}, infinity).

pause(Job_ID) ->
    gen_server:call(?SERVER, {pause, to_binary(Job_ID)}, infinity).

resume(Job_ID) ->
    gen_server:call(?SERVER, {resume, to_binary(Job_ID)}, infinity).

update_task(Job_ID, Task_ID, Type, Options) ->
    gen_server:call(?SERVER, {update_task, to_binary(Job_ID), Task_ID, Type, Options}, infinity).

rm_task(Job_ID, Task_ID) ->
    gen_server:call(?SERVER, {rm_task, to_binary(Job_ID), Task_ID}, infinity).

list_tasks(Job_ID) ->
    gen_server:call(?SERVER, {list_tasks, to_binary(Job_ID)}, infinity).

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
    couch_mgr:load_doc_from_file(?MONITOR_DB, monitor, "monitor.json"),
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
handle_call({set_amqp_host, AHost}, _From, #state{amqp_host = CurAHost} = State) ->
    format_log(info, "MONITOR_MASTER(~p): Updating amqp host from ~p to ~p", [self(), CurAHost, AHost]),
    %% Update the AMQP host of all the JOB
    lists:foreach(fun({_,Pid,_,_}) -> 
                          gen_server:call(Pid, {set_amqp_host, AHost}, infinity) 
                  end, supervisor:which_children(monitor_job_sup)),
    %% Update the AMQP host of all the Agents
    lists:foreach(fun({_,Pid,_,_}) -> 
                          gen_server:call(Pid, {set_amqp_host, AHost}, infinity) 
                  end, supervisor:which_children(monitor_agent_sup)),
    {reply, ok, State#state{amqp_host = AHost}};

handle_call({start_job, Job_ID, Interval}, _From, State) ->
    case ensure_running(Job_ID, State, Interval) of
        {_Pid, NewState} ->
            {reply, ok, NewState};
        _ ->
            {reply, error, State}
    end;

handle_call({sync_jobs}, _From, #state{jobs = CurJobs} = State) ->            
    case get_jobs(State) of
        {error, _Error} = E ->
            {reply, E, State};
        [] ->
            lists:foreach(fun({Job_ID, _Job}) -> spawn(fun() -> rm_job(Job_ID) end) end, CurJobs),
            {reply, ok, State};
        Jobs ->
            {NewState, Zombies} = lists:foldl(fun({Job}, {StateIn, ZombiesIn}) ->
                Job_ID = get_value(<<"id">>, Job),
                ZombiesOut = lists:delete(to_binary(Job_ID), ZombiesIn),
                case ensure_running(Job_ID, StateIn) of
                    {Pid, StateOut} ->
                        gen_server:call(Pid, {sync, get_value(<<"value">>, Job, [])}, infinity),
                        {StateOut, ZombiesOut};
                    _ ->
                        {StateIn, ZombiesOut}
                end end, {State, proplists:get_keys(State#state.jobs)}, Jobs),
            lists:foreach(fun(Job_ID) -> spawn(fun() -> rm_job(Job_ID) end) end, Zombies),
            {reply, ok, NewState}
    end;

handle_call({sync_job, Job_ID}, _From, State) ->
    case get_jobs(State, Job_ID) of
        {error, _Error} = E ->
            {reply, E, State};
        [] ->
            spawn(fun() -> rm_job(Job_ID) end),
            {reply, ok, State};
        [{struct, Job}  ] ->
            case ensure_running(Job_ID, State) of
                {Pid, NewState} ->
                    gen_server:call(Pid, {sync, get_value(<<"value">>, Job, [])}, infinity),
                    format_log(info, "~p", get_value(<<"value">>, Job, [])),
                    {reply, ok, NewState};
                _ ->
                    {reply, ok, State}
            end
    end;

handle_call({rm_job, Job_ID}, _From, #state{jobs = Jobs} = State) ->
    case get_value(Job_ID, Jobs) of
        #job{processID = Pid} -> 
            {reply, {ok, Pid ! stop}, State};
        undefined -> 
            {reply, {ok, not_running}, State}
    end;

handle_call({list_jobs}, _From, #state{jobs = Jobs} = State) ->
    {reply, Jobs, State};

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

handle_call({run_job, Job_ID}, _From, #state{jobs = Jobs} = State) ->
    {reply, msg_job(Job_ID, Jobs, {run_job}), State};

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
handle_info({'DOWN', _MonitorRef, _Type, Job_PID, _Info}, #state{jobs = CurJobs} = State) ->
    Jobs = lists:filter(fun({_, #job{processID = Pid}}) -> Pid /= Job_PID end, CurJobs),
    format_log(info, "MONITOR_MASTER(~p): Job PID ~p went down (~p)", [self(), Job_PID, _Info]),
    {noreply, State#state{jobs = Jobs}};

handle_info({document_deleted, Doc_ID}, State) ->
    spawn(fun() -> rm_job(Doc_ID) end),
    {noreply, State};
    
handle_info({document_changes, Doc_ID, _Changes}, State) ->
    spawn(fun() -> sync_job(Doc_ID) end),
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
    format_log(error, "MONITOR_MASTER(~p): Going down (~p)...", [self(), _Reason]),
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
%% Looks up job definitions from the database with an optional
%% key, if provided will return only definitions for that key
%%
%% @spec(get_jobs/2 :: (State :: #state{}, Key :: string) -> 
%%        touple(error, atom()) | proplist()).
%% @end
%%--------------------------------------------------------------------
get_jobs(State) ->
    get_jobs(State, "").

get_jobs(#state{database = DB, db_view = View}, Key) ->
    Options = case to_binary(Key) of <<>> -> []; K -> [{"key", K}] end, 
    case couch_mgr:get_results(DB, View, Options) of
        {error, _}=E ->
            format_log(info, "MONITOR_MASTER(~p): Result not found (~p)", [self(), E]),
            E;
        {ok, Jobs} ->
            format_log(info, "MONITOR_MASTER(~p): Found ~p jobs in the database ~p using options ~p", [self(), length(Jobs), DB, Options]), 
            Jobs
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Messages a job server
%%
%% @spec(msg_job/2 :: (State :: binary(), Jobs :: proplist(), 
%%  Msg :: any()) -> touple(ok, Msg) | tuple(error, job_not_found).
%% @end
%%--------------------------------------------------------------------
msg_job(Job_ID, Jobs, Msg) ->
    case get_value(Job_ID, Jobs) of
        #job{processID = Pid} ->
            {ok, gen_server:call(Pid, Msg, infinity)};
        undefined ->
            {error, job_not_found}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Finds the PID of a running job or starts a new job serve if it 
%% does not exist
%%
%% @spec(ensure_running/3 :: (Job_ID :: binary, State :: binary(), 
%%      Interval :: pos_integer()) -> 
%%      tuple(pid(), #state{}) | tuple(error, #state{}).
%% @end
%%--------------------------------------------------------------------
ensure_running(Job_ID, State) ->
    ensure_running(Job_ID, State, ?DEFAULT_INTERVAL).

ensure_running(Job_ID, #state{jobs = Jobs, amqp_host = AHost} = State, Interval) ->
    case get_value(Job_ID, Jobs) of
        #job{processID = Pid} ->
            {Pid, State};
        undefined ->
            case monitor_job_sup:start_job(Job_ID, AHost, Interval) of
                {ok, Pid} ->
                    Job = #job{processID = Pid, monitorRef = erlang:monitor(process, Pid)},
                    {Pid, State#state{jobs = [{Job_ID, Job}|Jobs]}};
                _Else ->
                    {error, State}
            end 
    end.
