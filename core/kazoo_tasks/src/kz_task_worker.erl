%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%% Run tasks scheduled by parent.
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_task_worker).
-behaviour(gen_server).

%% API
-export([start_link/4
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("tasks.hrl").

-define(SERVER, ?MODULE).

-record(state, { task_pid :: pid()
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_link(kz_tasks:task_id(), module(), atom(), list()) -> startlink_ret().
start_link(TaskId, M, F, A) ->
    _ = kz_util:put_callid(TaskId),
    gen_server:start_link(?SERVER, [TaskId, M, F, A], []).


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
init([_TaskId, _M, _F, _A]=Args) ->
    State =
        #state{ task_pid = kz_util:spawn_link(fun run_task/4, Args)
              },
    {'ok', State}.

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
    lager:debug("unhandled call ~p from ~p", [_Request, _From]),
    {'reply', {'error', 'not_implemented'}, State}.

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
    lager:debug("unhandled cast ~p", [_Msg]),
    {'noreply', State}.

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
    lager:debug("unhandled message ~p", [_Info]),
    {'noreply', State}.

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
terminate(_Reason, #state{task_pid = Pid}) ->
    lager:debug("~s (handling ~p) terminating: ~p", [?MODULE, Pid, _Reason]),
    _ = exit(Pid, 'kill'),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec run_task(kz_tasks:task_id(), module(), atom(), list()) -> any().
run_task(TaskId, M, F, A) ->
    try
        R = apply(M, F, A),
        _ = kz_tasks:worker_finished(TaskId),
        lager:debug("R = ~p. Should store this in MoDB", [R])
    catch
        _E:_R ->
            Stacktrace = erlang:get_stacktrace(),
            Msg = io_lib:format("(~p) ~p: ~w\n~p"
                               ,[self(), _E, _R, Stacktrace]),
            _ = kz_tasks:worker_failed(TaskId, iolist_to_binary(Msg)),
            lager:error("task ~s ~s", [TaskId, Msg]),
            error_logger:error_report(Msg)
    end.

%%% End of Module.
