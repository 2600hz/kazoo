%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc Trigger jobs for execution
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_tasks_trigger).
-behaviour(gen_server).

-export([start_link/0]).
-export([status/0]).
-export([browse_dbs_for_triggers/1]).

%%% gen_server callbacks
-export([init/1
        ,handle_cast/2
        ,handle_call/3
        ,handle_info/2
        ,code_change/3
        ,terminate/2
        ]).

-include("tasks.hrl").

-define(SERVER, {'via', 'kz_globals', ?MODULE}).

-record(state, {minute_ref = minute_timer() :: reference()
               ,hour_ref = hour_timer() :: reference()
               ,day_ref = day_timer() :: reference()
               ,browse_dbs_ref = browse_dbs_timer() :: reference() %%TODO: gen_listen for DB news!
               }).

-type state() :: #state{}.

-define(CLEANUP_TIMER
       ,kapps_config:get_pos_integer(?CONFIG_CAT, <<"browse_dbs_interval_s">>, ?SECONDS_IN_DAY)).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec status() -> kz_term:proplist().
status() ->
    gen_server:call(?SERVER, 'status').

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    case gen_server:start_link(?SERVER, ?MODULE, [], []) of
        {'error', {'already_started', Pid}} ->
            'true' = link(Pid),
            {'ok', Pid};
        Other -> Other
    end.


%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    _ = process_flag('trap_exit', 'true'),
    kz_util:put_callid(?MODULE),
    lager:debug("started ~s", [?MODULE]),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('status', _From, #state{minute_ref = Minute
                                   ,hour_ref = Hour
                                   ,day_ref = Day
                                   ,browse_dbs_ref = Browse
                                   }=State) ->
    Timers = [{'minute', erlang:read_timer(Minute)}
             ,{'hour', erlang:read_timer(Hour)}
             ,{'day', erlang:read_timer(Day)}
             ,{'cleanup', erlang:read_timer(Browse)}
             ],
    {'reply', Timers, State};

handle_call(_Request, _From, State) ->
    lager:debug("unhandled call ~p from ~p", [_Request, _From]),
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'cleanup_finished', Ref}, #state{browse_dbs_ref = Ref}=State) ->
    lager:debug("cleanup finished for ~p, starting timer", [Ref]),
    {'noreply', State#state{browse_dbs_ref = browse_dbs_timer()}, 'hibernate'};

handle_cast(_Msg, State) ->
    lager:debug("unhandled cast ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'EXIT', _Pid, normal}, State) ->
    lager:debug("job ~p terminated normally", [_Pid]),
    {noreply, State};
handle_info({'EXIT', _Pid, _Reason}, State) ->
    lager:error("job ~p crashed: ~p", [_Pid, _Reason]),
    {noreply, State};

handle_info({timeout, Ref, _Msg}, #state{minute_ref = Ref}=State) ->
    spawn_jobs(Ref, ?TRIGGER_MINUTELY),
    {'noreply', State#state{minute_ref = minute_timer()}};

handle_info({timeout, Ref, _Msg}, #state{hour_ref = Ref}=State) ->
    spawn_jobs(Ref, ?TRIGGER_HOURLY),
    {'noreply', State#state{hour_ref = hour_timer()}};

handle_info({timeout, Ref, _Msg}, #state{day_ref = Ref}=State) ->
    spawn_jobs(Ref, ?TRIGGER_DAILY),
    {'noreply', State#state{day_ref = day_timer()}};

handle_info({timeout, Ref, _Msg}, #state{browse_dbs_ref = Ref}=State) ->
    _Pid = kz_util:spawn(fun browse_dbs_for_triggers/1, [Ref]),
    lager:debug("cleaning up in ~p(~p)", [_Pid, Ref]),
    {'noreply', State};

handle_info(_Info, State) ->
    lager:debug("unhandled message ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("~s terminating: ~p", [?MODULE, _Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec minute_timer() -> reference().
minute_timer() ->
    erlang:start_timer(?MILLISECONDS_IN_MINUTE, self(), ok).

-spec hour_timer() -> reference().
hour_timer() ->
    erlang:start_timer(?MILLISECONDS_IN_HOUR, self(), ok).

-spec day_timer() -> reference().
day_timer() ->
    erlang:start_timer(?MILLISECONDS_IN_DAY, self(), ok).

-spec browse_dbs_timer() -> reference().
browse_dbs_timer() ->
    Expiry = ?CLEANUP_TIMER,
    lager:debug("starting cleanup timer for ~b s", [Expiry]),
    erlang:start_timer(Expiry * ?MILLISECONDS_IN_SECOND, self(), ok).


-spec spawn_jobs(reference(), kz_term:ne_binary()) -> ok.
spawn_jobs(Ref, Binding) ->
    CallId = make_callid(Ref, Binding),
    _Pid = erlang:spawn_link(fun () ->
                                     _ = kz_util:put_callid(CallId),
                                     tasks_bindings:map(Binding, [])
                             end),
    lager:debug("binding ~s triggered ~p via ~p", [Binding, _Pid, Ref]).

-spec make_callid(reference(), kz_term:ne_binary()) -> kz_term:ne_binary().
make_callid(Ref, Binding) ->
    Key = lists:last(binary:split(Binding, <<$.>>, [global])),
    Id = ref_to_id(Ref),
    <<"task_", Key/binary, "_", Id/binary>>.

ref_to_id(Ref) ->
    Bin = list_to_binary(io_lib:format("~p", [Ref])),
    Start = <<"#Ref<">>,
    StartSize = byte_size(Start),
    Size = byte_size(Bin) - StartSize - 1,
    <<Start:StartSize/binary, Id:Size/binary, ">">> = Bin,
    Id.

%% =======================================================================================
%% Start - Automatic Compaction Section
%% =======================================================================================

%%------------------------------------------------------------------------------
%% @doc Entry point for starting the automatic compaction job.
%%
%% This functions gets triggered by the `browse_dbs_ref' based on `browse_dbs_timer'
%% function. By default it triggers the action 1 day after the timer starts.
%% @end
%%------------------------------------------------------------------------------
-spec browse_dbs_for_triggers(atom() | reference()) -> 'ok'.
browse_dbs_for_triggers(Ref) ->
    CallId = <<"cleanup_pass_", (kz_binary:rand_hex(4))/binary>>,
    kz_util:put_callid(CallId),
    lager:debug("starting cleanup pass of databases"),
    Sorted = kt_compactor:get_all_dbs_and_sort_by_disk(),
    TotalSorted = length(Sorted),
    'ok' = kt_compaction_reporter:start_tracking_job(self(), node(), CallId, Sorted),
    F = fun({Db, _Sizes}, Ctr) ->
                lager:debug("compacting ~p out of ~p dbs (~p remaining)",
                            [Ctr, TotalSorted, (TotalSorted - Ctr)]),
                cleanup_pass(Db),
                Ctr + 1
        end,
    _Counter = lists:foldl(F, 1, Sorted),
    'ok' = kt_compaction_reporter:stop_tracking_job(CallId),
    kz_util:put_callid('undefined'), % Reset callid
    lager:debug("pass completed for ~p", [Ref]),
    gen_server:cast(?SERVER, {'cleanup_finished', Ref}).

-spec cleanup_pass(kz_term:ne_binary()) -> boolean().
cleanup_pass(Db) ->
    _ = tasks_bindings:map(db_to_trigger(Db), Db),
    erlang:garbage_collect(self()).

-spec db_to_trigger(kz_term:ne_binary()) -> kz_term:ne_binary().
db_to_trigger(Db) ->
    Classifiers = [{fun kapps_util:is_account_db/1, ?TRIGGER_ACCOUNT}
                  ,{fun kapps_util:is_account_mod/1, ?TRIGGER_ACCOUNT_MOD}
                  ,{fun is_system_db/1, ?TRIGGER_SYSTEM}
                  ],
    db_to_trigger(Db, Classifiers).

db_to_trigger(_Db, []) -> ?TRIGGER_OTHER;
db_to_trigger(Db, [{Classifier, Trigger} | Classifiers]) ->
    case Classifier(Db) of
        'true' -> Trigger;
        'false' -> db_to_trigger(Db, Classifiers)
    end.

-spec is_system_db(kz_term:ne_binary()) -> boolean().
is_system_db(Db) ->
    lists:member(Db, ?KZ_SYSTEM_DBS).
%% =======================================================================================
%% End - Automatic Compaction Section
%% =======================================================================================

%%% End of Module.
