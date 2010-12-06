%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.com>
%%% @copyright (C) 2010, Karl Anderson
%%% @doc
%%% Responsible for retrieving monitoring jobs from couchdb
%%% @end
%%% Created : 05 Dev 2010 by Karl Anderson <karl@2600hz.com>
%%%-------------------------------------------------------------------
-module(monitor_import).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([set_db_host/1]).

-export([sync/0, sync/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-import(logger, [format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

-include("../include/monitor_amqp.hrl").
-include("../include/monitor_couch.hrl").

-record(state, {
         db_host = "" :: string()
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
start_link() ->
    start_link(?DB_HOST).

start_link(DBHost) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [DBHost], []).

set_db_host(DBHost) ->
    gen_server:call(?SERVER, {set_db_host, DBHost}, infinity).

sync() ->
    sync([]).

sync(Options) ->
    gen_server:call(?SERVER, {sync_jobs, Options}, infinity).


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
init([DBHost]) ->
    couch_mgr:set_host(DBHost),
    {ok, #state{db_host = DBHost}}.

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
handle_call({set_db_host, DBHost}, _From, State) ->
    couch_mgr:set_host(DBHost),
    {reply, ok, State#state{db_host = DBHost}};
   
handle_call({sync_jobs, Options}, _From, State) ->
    case get_jobs(Options) of
        {error, E} ->
            {reply, {error, E}, State};
        {ok, Jobs} ->
            load_jobs(Jobs),
            {ok, Running} = monitor_master:list_jobs(),
            unload_jobs(Jobs, Running),
            {reply, ok, State}
    end;
 
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
    format_log(error, "MONITOR_COUCH_SYNC(~p): Going down(~p)...~n", [self(), _Reason]),
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
get_jobs(Options) ->
    case couch_mgr:get_results(?TS_DB, ?TS_VIEW_MONITOR, Options) of
        false ->
            format_log(error, "MONITOR_COUCH_SYNC(~p): Missing view ~p~n", [self(), ?TS_VIEW_MONITOR]),
            {error, missing_view};
        {error, not_found} ->
            format_log(info, "MONITOR_COUCH_SYNC(~p): Something was not found~n", [self()]),
            {error, not_found};
        [] ->
            format_log(info, "MONITOR_COUCH_SYNC(~p): No monitoring required~n", [self()]),
            {error, no_jobs_found};
        Jobs ->
            {ok, Jobs}
    end.

unload_jobs([], []) ->
    ok;
unload_jobs([], [{Job_ID, _Job}|T]) ->
    monitor_master:rm_job(Job_ID),
    unload_jobs([], T);
unload_jobs([{Job} | T], Running) ->
    Job_ID = get_value(<<"key">>, Job, []),
    unload_jobs(T, proplists:delete(Job_ID, Running)).

load_jobs([]) ->
    ok;
load_jobs([{Job} | T]) ->
    Job_ID = get_value(<<"key">>, Job, []),
    Tasks = get_value(<<"value">>, Job, []),
    monitor_master:start_job(Job_ID, []),
    load_tasks(Tasks, Job_ID),
    {ok, Running} = monitor_master:list_tasks(Job_ID),
    unload_tasks(Tasks, Job_ID, Running),
    load_jobs(T).

unload_tasks([], _Job_ID, []) ->
    ok;
unload_tasks([], Job_ID, [{Task_ID, _Task}|T]) ->
    monitor_master:rm_task(Job_ID, Task_ID),
    unload_tasks([], Job_ID, T);
unload_tasks([{H} | T], Job_ID, Running) ->
    Task_ID = whistle_util:to_list(get_value(<<"task_id">>, H, <<"unknown">>)),
    unload_tasks(T, Job_ID, proplists:delete(Task_ID, Running)).

load_tasks([], _Job_ID) ->
    ok;
load_tasks([{H}|T], Job_ID) ->
    Task_ID = whistle_util:to_list(get_value(<<"task_id">>, H, <<"unknown">>)),
    Type = whistle_util:to_list(get_value(<<"type">>, H, <<"unknown">>)),
    {Options} = get_value(<<"options">>, H, <<"unknown">>),
    monitor_master:add_task(Job_ID, Task_ID, Type, Options),
    load_tasks(T, Job_ID).
