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
-export([start_link/6]).
-export([stop/1
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

-record(state, { task_id :: kz_tasks:task_id()
               , module :: module()
               , function :: atom()
               , fassoc :: kz_csv:fassoc()
               , extra_args :: kz_proplist()
               , total_rows = 0 :: non_neg_integer()
               , total_errors = 0 :: non_neg_integer()
               }).

-define(NOREPLY(State), {'noreply', State}).
-define(CSV, 'csv').


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_link(kz_tasks:task_id()
                ,module()
                ,atom()
                ,kz_proplist()
                ,ne_binaries()
                ,ne_binary()
                ) -> startlink_ret().
start_link(TaskId, Module, Function, ExtraArgs, OrderedFields, AName) ->
    gen_server:start_link(?SERVER, [TaskId, Module, Function, ExtraArgs, OrderedFields, AName], []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec stop(pid()) -> any().
stop(Pid)
  when is_pid(Pid) ->
    gen_server:cast(Pid, 'stop').


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
init([TaskId, Module, Function, ExtraArgs, OrderedFields, AName]) ->
    _ = kz_util:put_callid(TaskId),
    case kz_datamgr:fetch_attachment(?KZ_TASKS_DB, TaskId, AName) of
        {'error', Reason} ->
            lager:error("failed loading attachment ~s from ~s/~s: ~p"
                       ,[AName, ?KZ_TASKS_DB, TaskId, Reason]),
            {'stop', Reason};
        {'ok', CSV} ->
            {Header, CSVRest} = kz_csv:take_row(CSV),
            FAssoc = kz_csv:associator(Header, OrderedFields),
            State = #state{ task_id = TaskId
                          , module = Module
                          , function = Function
                          , fassoc = FAssoc
                          , extra_args = ExtraArgs
                          },
            lager:debug("task ~s worker starting", [TaskId]),
            'undefined' = put(?CSV, CSVRest),
            gen_server:cast(?SERVER, 'go'),
            {'ok', State}
    end.

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
handle_cast('go', State=#state{task_id = TaskId
                              ,module = Module
                              ,function = Function
                              ,fassoc = FAssoc
                              ,extra_args = ExtraArgs
                              ,total_rows = TotalRows
                              ,total_errors = TotalErrors
                              }) ->
    case kz_csv:take_row(get(?CSV)) of
        'eof' ->
            kz_tasks:worker_finished(TaskId, TotalRows, TotalErrors),
            gen_server:cast(self(), 'stop'),
            _ = erase(?CSV),
            ?NOREPLY(State);
        {Row, CSVRest} ->
            ReOrderedArgs = FAssoc(Row),
            Errors =
                try
                    apply(Module, Function, [ExtraArgs]++ReOrderedArgs),
                    0
                catch
                    _E:_R ->
                        kz_util:log_stacktrace(),
                        1
                end,
            NewState = State#state{total_errors = TotalErrors + Errors
                                  ,total_rows = TotalRows + 1
                                  },
            _ = put(?CSV, CSVRest),
            gen_server:cast(self(), 'go'),
            ?NOREPLY(NewState)
    end;

handle_cast('stop', State) ->
    {'stop', 'normal', State};

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
terminate(_Reason, _State) ->
    lager:debug("terminating: ", [_Reason]).

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

%%% End of Module.
