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
-export([start_link/1
        ,status/1
        ,stop/1
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("kz_tasks.hrl").

-define(SERVER, ?MODULE).

-type task_data() :: kz_json:object().
-type task_status() :: 'waiting' | 'running' | 'done' | 'error'.

-record(state, { data :: task_data()
               , task_pid :: pid()
               , status = 'waiting' :: task_status()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_link(task_data()) -> startlink_ret().
start_link(TaskData) ->
    gen_server:start_link(?SERVER, [TaskData], []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec status(pid()) -> {task_status(), pid()}.
status(Pid) ->
    gen_server:call(Pid, 'status').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec stop(pid()) -> 'ok'.
stop(Pid) ->
    gen_server:cast(Pid, 'stop').


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the server
%%--------------------------------------------------------------------
-spec init([task_data()]) -> {'ok', state()}.
init([TaskData]) ->
    Self = self(),
    Pid = spawn_link(
            fun () ->
                    gen_server:cast(Self, 'running'),
                    try
                        lager:debug(">>> TaskData ~s", [kz_json:encode(TaskData)]),
                        M = kz_util:to_atom(kz_json:get_value([<<"task">>, <<"M">>], TaskData), 'true'),
                        F = kz_util:to_atom(kz_json:get_value([<<"task">>, <<"F">>], TaskData), 'true'),
                        A = kz_json:get_list_value([<<"task">>, <<"Args">>], TaskData),
                        R = apply(M, F, A),
                        gen_server:cast(Self, 'done'),
                        lager:debug("R = ~p. Should store this in MoDB", [R])
                    catch
                        _E:_R ->
                            Stacktrace = erlang:get_stacktrace(),
                            Msg = io_lib:format("Task ~p in process ~p with value: ~p"
                                               ,[_E, self(), {_R, Stacktrace}]),
                            gen_server:cast(Self, 'error'),
                            lager:error(Msg),
                            error_logger:error_report(Msg)
                    end
            end),
    {'ok', #state{ task_pid = Pid
                 , data = TaskData
                 }
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling call messages
%%--------------------------------------------------------------------
handle_call('status', _From, State) ->
    Status = State#state.status,
    TaskPid = State#state.task_pid,
    {'reply', {Status, TaskPid}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling cast messages
%%--------------------------------------------------------------------
handle_cast('error', State) ->
    {'noreply', State#state{status = 'error'}};
handle_cast('done', State) ->
    {'noreply', State#state{status = 'done'}};
handle_cast('waiting', State) ->
    {'noreply', State#state{status = 'waiting'}};
handle_cast('running', State) ->
    {'noreply', State#state{status = 'running'}};
handle_cast('stop', State) ->
    {'stop', 'normal', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    lager:debug("unhandled message ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%--------------------------------------------------------------------
terminate(_Reason, #state{task_pid = Pid}) ->
    lager:debug("~s (handling ~p) terminating: ~p", [?MODULE, Pid, _Reason]),
    _ = exit(Pid, 'kill'),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
