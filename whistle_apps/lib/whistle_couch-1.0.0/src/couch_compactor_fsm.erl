%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(couch_compactor_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/0
         ,compact/0
         ,compact_node/1
         ,compact_db/1
         ,compact_db/2
         ,status/0
         ,is_compactor_running/0
         ,cancel_current_job/0
         ,cancel_all_jobs/0
         ,start_auto_compaction/0
         ,stop_auto_compaction/0
        ]).

%% Internal
-export([compact_shard/3]).

%% gen_fsm callbacks
-export([init/1
         ,handle_event/3
         ,handle_sync_event/4
         ,handle_info/3
         ,terminate/3
         ,code_change/4

         %% state functions
         ,ready/2, ready/3                     % FSM is ready to compact something
         ,compact/2, compact/3         % FSM is compacting all nodes
         ,wait/2, wait/3                       % FSM is waiting to compact the next thing
        ]).

-include_lib("whistle_couch/include/wh_couch.hrl").

-define(SLEEP_BETWEEN_COMPACTION, 60000).
-define(SLEEP_BETWEEN_POLL, 1000).
-define(MAX_COMPACTING_SHARDS, 10).
-define(MAX_COMPACTING_VIEWS, 5).
-define(SLEEP_BETWEEN_VIEWS, 2000).
-define(MAX_WAIT_FOR_COMPACTION_PID, 360000). % five minutes

-define(SERVER, ?MODULE).

-type req_job() :: 'req_compact' |
                   {'req_compact_node', ne_binary()} |
                   {'req_compact_db', ne_binary()} |
                   {'req_compact_db', ne_binary(), ne_binary()}.

-type not_compacting() :: {'error', 'compactor_down'}.

-record(state, {
          nodes :: ne_binaries()
          ,dbs :: ne_binaries()
          ,cookie :: atom()
          ,wait_ref :: reference()

          ,current_node :: ne_binary()
          ,current_db :: ne_binary()
          ,conn :: #server{}
          ,admin_conn :: #server{}

          %% [ {Job, Pid, Ref},...]
          ,queued_jobs = queue:new() :: queue()
          ,current_job_pid :: pid()
          ,current_job_ref :: reference()
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec compact/0 :: () -> {'queued', reference()} | not_compacting().
compact() ->
    case is_compactor_running() of
        true -> gen_fsm:sync_send_event(?SERVER, req_compact);
        false -> {error, compactor_down}
    end.

-spec compact_node/1 :: (ne_binary()) -> {'queued', reference()} | not_compacting().
compact_node(Node) ->
    case is_compactor_running() of
        true -> gen_fsm:sync_send_event(?SERVER, {req_compact_node, Node});
        false -> {'error', 'compactor_down'}
    end.

-spec compact_db/1 :: (ne_binary()) -> {'queued', reference()} | not_compacting().
compact_db(Db) ->
    case is_compactor_running() of
        true -> gen_fsm:sync_send_event(?SERVER, {req_compact_db, Db});
        false -> {'error', 'compactor_down'}
    end.

-spec compact_db/2 :: (ne_binary(), ne_binary()) -> {'queued', reference()} | not_compacting().
compact_db(Node, Db) ->
    case is_compactor_running() of
        true -> gen_fsm:sync_send_event(?SERVER, {req_compact_db, Node, Db});
        false -> {'error', 'compactor_down'}
    end.

-spec status/0 :: () -> {'ok', 'ready' | 'not_running' | wh_proplist()}.
status() ->
    case is_compactor_running() of
        true -> gen_fsm:sync_send_event(?SERVER, status);
        false -> {ok, not_running}
    end.

-spec cancel_current_job/0 :: () -> {'ok', 'job_cancelled'} |
                                    {'error', 'no_job_running'} |
                                    not_compacting().
cancel_current_job() ->
    case is_compactor_running() of
        true -> gen_fsm:sync_send_event(?SERVER, cancel_current_job);
        false -> {'error', 'compactor_down'}
    end.

-spec cancel_all_jobs/0 :: () -> {'ok', 'jobs_cancelled'} | not_compacting().
cancel_all_jobs() ->
    case is_compactor_running() of
        true -> gen_fsm:sync_send_event(?SERVER, cancel_all_jobs);
        false -> {'error', 'compactor_down'}
    end.

-spec start_auto_compaction/0 :: () -> {'ok', 'already_started'} |
                                       {'queued', reference()} |
                                       not_compacting().
start_auto_compaction() ->
    case is_compactor_running() of
        true ->
            case couch_config:fetch(<<"compact_automatically">>, false) of
                true -> {ok, already_started};
                false ->
                    _ = couch_config:store(<<"compact_automatically">>, true),
                    compact()
            end;
        false -> {'error', 'compactor_down'}
    end.

-spec stop_auto_compaction/0 :: () -> {'ok', 'updated' | 'already_stopped'} | not_compacting().
stop_auto_compaction() ->
    case is_compactor_running() of
        true ->
            case couch_config:fetch(<<"compact_automatically">>, false) of
                false -> {ok, already_stopped};
                true ->
                    _ = couch_config:store(<<"compact_automatically">>, false),
                    {ok, updated}
            end;
        false -> {'error', 'compactor_down'}
    end.

-spec is_compactor_running/0 :: () -> boolean().
is_compactor_running() ->
    is_pid(whistle_couch_sup:compactor_pid()).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    _ = random:seed(erlang:now()),
    put(callid, ?MODULE),

    maybe_start_auto_compaction_job(),

    {ok, ready, #state{cookie=wh_util:to_atom(couch_config:fetch(<<"bigcouch_cookie">>, <<"monster">>), true)
                       ,conn=undefined
                       ,admin_conn=undefined
                      }}.

%%--------------------------------------------------------------------
ready(compact, State) ->
    gen_fsm:send_event(self(), compact),
    {next_state, compact, State#state{nodes=get_nodes()
                                      ,conn=undefined
                                      ,admin_conn=undefined
                                      ,current_node=undefined
                                      ,current_db=undefined
                                     }};
ready({compact_node, N}, State) ->
    gen_fsm:send_event(self(), compact),
    {next_state, compact, State#state{nodes=[N]
                                      ,conn=undefined
                                      ,admin_conn=undefined
                                      ,current_node=N
                                      ,current_db=undefined
                                     }};
ready({compact_db, D}, State) ->
    [N|Ns] = get_nodes(),
    gen_fsm:send_event(self(), {compact_db, N, D}),
    {next_state, compact, State#state{nodes=Ns
                                      ,dbs=[D]
                                      ,conn=undefined
                                      ,admin_conn=undefined
                                      ,current_node=N
                                      ,current_db=D
                                     }};
ready({compact_db, N, D}, State) ->
    gen_fsm:send_event(self(), {compact_db, N, D}),
    {next_state, compact, State#state{nodes=[]
                                      ,dbs=[]
                                      ,conn=undefined
                                      ,admin_conn=undefined
                                      ,current_node=N
                                      ,current_db=D
                                     }};
ready(next_job, #state{queued_jobs=Jobs}=State) ->
    case queue:out(Jobs) of
        {empty, _} ->
            maybe_start_auto_compaction_job(),
            {next_state, ready, State#state{current_job_pid=undefined
                                            ,current_job_ref=undefined
                                           }};
        {{value, {Job, P, Ref}}, Jobs1} ->
            maybe_send_update(P, Ref, job_starting),
            gen_fsm:send_event(self(), Job),
            lager:debug("starting job for ~p:~p", [P, Ref]),
            {next_state, ready, State#state{queued_jobs=Jobs1
                                            ,current_job_pid=P
                                            ,current_job_ref=Ref
                                           }}
    end;
ready(_Msg, State) ->
    lager:debug("unhandled msg: ~p", [_Msg]),
    {next_state, ready, State}.

ready(status, _, #state{}=State) ->
    {reply, {ok, ready}, ready, State};

ready(cancel_current_job, _, State) ->
    {reply, {error, no_job_running}, ready, State};

ready(cancel_all_jobs, _, #state{queued_jobs=Jobs}=State) ->
    _ = [ maybe_send_update(P, Ref, job_cancelled) || {_, P, Ref} <- queue:to_list(Jobs)],
    {reply, {ok, jobs_cancelled}, ready, State#state{queued_jobs=queue:new()}};

ready(Msg, {NewP, _}, #state{queued_jobs=Jobs}=State) ->
    case queue:out(Jobs) of
        {empty, _} ->
            {Ref, Jobs1} = queue_job(Msg, NewP, Jobs),
            gen_fsm:send_event(self(), next_job),
            {reply, {queued, Ref}, ready, State#state{queued_jobs=Jobs1}};

        {{value, {Job, P, Ref}}, Jobs1} ->
            maybe_send_update(P, Ref, job_starting),
            gen_fsm:send_event(self(), Job),
            lager:debug("starting job for ~p:~p", [P, Ref]),

            {Ref, Jobs2} = queue_job(Msg, NewP, Jobs1),

            {reply, {queued, Ref}, ready, State#state{queued_jobs=Jobs2
                                                      ,current_job_pid=P
                                                      ,current_job_ref=Ref
                                                     }}
    end.

-spec queue_job/3 :: (req_job(), pid(), queue()) -> {reference(), queue()}.
queue_job(req_compact, P, Jobs) ->
    Ref = erlang:make_ref(),
    {Ref, queue:in({compact, P, Ref}, Jobs)};
queue_job({req_compact_node, Node}, P, Jobs) ->
    Ref = erlang:make_ref(),
    {Ref, queue:in({{compact_node, Node}, P, Ref}, Jobs)};
queue_job({req_compact_db, Db}, P, Jobs) ->
    Ref = erlang:make_ref(),
    {Ref, queue:in({{compact_db, Db}, P, Ref}, Jobs)};
queue_job({req_compact_db, Node, Db}, P, Jobs) ->
    Ref = erlang:make_ref(),
    {Ref, queue:in({{compact_db, Node, Db}, P, Ref}, Jobs)}.

%%--------------------------------------------------------------------
compact({compact, N}, #state{conn=undefined
                             ,admin_conn=undefined
                             ,cookie=Cookie
                             ,nodes=[]
                             ,current_job_pid=P
                             ,current_job_ref=Ref
                            }=State) ->
    try get_node_connections(N, Cookie) of
        {Conn, AdminConn} ->
            gen_fsm:send_event(self(), {compact, N}),
            {next_state, compact, State#state{conn=Conn
                                              ,admin_conn=AdminConn
                                              ,current_node=N
                                             }}
    catch
        _:{error,{conn_failed,{error,etimedout}}} ->
            lager:debug("failed to connect to node ~s: timed out", [N]),
            maybe_send_update(P, Ref, job_finished),
            gen_fsm:send_event(self(), next_job),
            {next_state, ready, State#state{conn=undefined
                                            ,admin_conn=undefined
                                            ,current_node=undefined
                                            ,current_job_pid=undefined
                                            ,current_job_ref=undefined
                                           }}
    end;
compact({compact, N}=Msg, #state{conn=undefined
                                 ,admin_conn=undefined
                                 ,cookie=Cookie
                                 ,nodes=[Node|Ns]
                                }=State) ->
    try get_node_connections(N, Cookie) of
        {Conn, AdminConn} ->
            gen_fsm:send_event(self(), Msg),
            {next_state, compact, State#state{conn=Conn
                                              ,admin_conn=AdminConn
                                              ,current_node=N
                                             }}
    catch
        _:{error,{conn_failed,{error,etimedout}}} ->
            lager:debug("failed to connect to node ~s: timed out", [N]),
            gen_fsm:send_event(self(), {compact, Node}),
            {next_state, compact, State#state{nodes=Ns
                                              ,current_node=undefined
                                             }}
    end;

compact({compact_db, N, D}=Msg, #state{conn=undefined
                                       ,admin_conn=undefined
                                       ,cookie=Cookie
                                       ,nodes=[]
                                       ,current_job_pid=P
                                       ,current_job_ref=Ref
                                      }=State) ->
    try get_node_connections(N, Cookie) of
        {Conn, AdminConn} ->
            gen_fsm:send_event(self(), Msg),
            {next_state, compact, State#state{conn=Conn
                                              ,admin_conn=AdminConn
                                              ,current_node=N
                                              ,current_db=D
                                             }}
    catch
        _:{error,{conn_failed,{error,etimedout}}} ->
            lager:debug("failed to connect to node ~s: timed out", [N]),
            maybe_send_update(P, Ref, job_finished),
            gen_fsm:send_event(self(), next_job),
            {next_state, ready, State#state{conn=undefined
                                            ,admin_conn=undefined
                                            ,current_node=undefined
                                            ,current_db=undefined
                                            ,current_job_pid=undefined
                                            ,current_job_ref=undefined
                                           }}
    end;

compact({compact_db, N, D}=Msg, #state{conn=undefined
                                       ,admin_conn=undefined
                                       ,cookie=Cookie
                                       ,nodes=[Node|Ns]
                                      }=State) ->
    try get_node_connections(N, Cookie) of
        {Conn, AdminConn} ->
            gen_fsm:send_event(self(), Msg),
            {next_state, compact, State#state{conn=Conn
                                              ,admin_conn=AdminConn
                                              ,current_node=N
                                              ,current_db=D
                                             }}
    catch
        _:{error,{conn_failed,{error,etimedout}}} ->
            lager:debug("failed to connect to node ~s: timed out", [N]),
            gen_fsm:send_event(self(), {compact_db, Node, D}),
            {next_state, compact, State#state{nodes=Ns
                                              ,current_node=Node
                                              ,current_db=D
                                             }}
    end;

compact(compact, #state{nodes=[]
                        ,current_job_pid=P
                        ,current_job_ref=Ref
                       }=State) ->
    lager:debug("no nodes to compact"),
    maybe_send_update(P, Ref, job_finished),
    gen_fsm:send_event(self(), next_job),
    {next_state, ready, State#state{conn=undefined
                                    ,admin_conn=undefined
                                    ,current_node=undefined
                                    ,current_db=undefined
                                    ,current_job_pid=undefined
                                    ,current_job_ref=undefined
                                   }};
compact(compact, #state{nodes=[N|Ns]}=State) ->
    lager:debug("compact node ~s", [N]),
    gen_fsm:send_event(self(), {compact, N}),
    {next_state, compact, State#state{nodes=Ns}};

compact({compact, N}, #state{admin_conn=AdminConn}=State) ->
    lager:debug("compacting node ~s", [N]),

    {ok, DBs} = node_dbs(AdminConn),
    [D|Ds] = shuffle(DBs),
    gen_fsm:send_event(self(), {compact, N, D}),
    {next_state, compact, State#state{dbs=Ds, current_db=D}};

compact({compact, N, D}, #state{conn=Conn
                                ,admin_conn=AdminConn
                                ,dbs=[]
                               }=State) ->
    case couch_util:db_exists(Conn, D) of
        false ->
            lager:debug("db ~s not found on ~s", [D, N]),
            gen_fsm:send_event(self(), compact),
            {next_state, compact, State#state{current_db=undefined}};
        true ->
            lager:debug("compacting ~s on ~s", [D, N]),
            Ss = db_shards(AdminConn, N, D),
            DDs = db_design_docs(Conn, D),
            gen_fsm:send_event(self(), {compact, N, D, Ss, DDs}),
            {next_state, compact, State#state{current_db=D}}
    end;

compact({compact, N, D}, #state{conn=Conn
                                ,admin_conn=AdminConn
                                ,dbs=[Db|Dbs]
                               }=State) ->
    case couch_util:db_exists(Conn, D) of
        false ->
            lager:debug("db ~s not found on ~s", [D, N]),
            gen_fsm:send_event(self(), {compact, N, Db}),
            {next_state, compact, State#state{dbs=Dbs
                                              ,current_db=Db
                                             }};
        true ->
            lager:debug("compacting ~s on ~s", [D, N]),
            Ss = db_shards(AdminConn, N, D),
            DDs = db_design_docs(Conn, D),
            gen_fsm:send_event(self(), {compact, N, D, Ss, DDs}),
            {next_state, compact, State#state{current_db=D}}
    end;

compact({compact_db, N, D}, #state{conn=Conn
                                   ,admin_conn=AdminConn
                                   ,nodes=[]
                                   ,current_job_pid=P
                                   ,current_job_ref=Ref
                                  }=State) ->
    case couch_util:db_exists(Conn, D) of
        false ->
            lager:debug("db ~s not found on ~s", [D, N]),
            maybe_send_update(P, Ref, job_finished),
            gen_fsm:send_event(self(), next_job),
            {next_state, ready, State#state{conn=undefined
                                            ,admin_conn=undefined
                                            ,current_node=undefined
                                            ,current_db=undefined
                                            ,current_job_pid=undefined
                                            ,current_job_ref=undefined
                                           }};
        true ->
            lager:debug("compacting ~s on ~s", [D, N]),
            Ss = db_shards(AdminConn, N, D),
            DDs = db_design_docs(Conn, D),
            gen_fsm:send_event(self(), {compact_db, N, D, Ss, DDs}),
            {next_state, compact, State#state{current_node=N
                                             ,current_db=N
                                             }}
    end;
compact({compact_db, N, D}, #state{conn=Conn
                                   ,admin_conn=AdminConn
                                   ,nodes=[Node|Ns]
                                  }=State) ->
    case couch_util:db_exists(Conn, D) of
        false ->
            lager:debug("db ~s not found on ~s", [D, N]),
            gen_fsm:send_event(self(), {compact_db, Node, D}),
            {next_state, compact, State#state{nodes=Ns
                                              ,current_node=Node
                                              ,current_db=D
                                             }};
        true ->
            lager:debug("compacting ~s on ~s", [D, N]),
            Ss = db_shards(AdminConn, N, D),
            DDs = db_design_docs(Conn, D),
            gen_fsm:send_event(self(), {compact_db, N, D, Ss, DDs}),
            {next_state, compact, State#state{current_node=N
                                              ,current_db=D
                                             }}
    end;

compact({compact, N, D, [], _}, #state{dbs=[]}=State) ->
    lager:debug("no shards to compact for ~s on ~s", [D, N]),
    gen_fsm:send_event(self(), compact),
    {next_state, compact, State};
compact({compact, N, D, [], _}, #state{dbs=[Db|Dbs]}=State) ->
    lager:debug("no shards to compact for ~s on ~s", [D, N]),
    gen_fsm:send_event(self(), {compact, N, Db}),
    {next_state, compact, State#state{dbs=Dbs}};

compact({compact, N, D, Ss, DDs}, #state{admin_conn=AdminConn
                                         ,dbs=[]
                                        }=State) ->
    try lists:split(?MAX_COMPACTING_SHARDS, Ss) of
        {Compact, Shards} ->
            compact_shards(AdminConn, Compact, DDs),
            Ref = gen_fsm:start_timer(?SLEEP_BETWEEN_COMPACTION, {compact, N, D, Shards, DDs}),
            {next_state, wait, State#state{wait_ref=Ref}}
    catch
        'error':'badarg' ->
            compact_shards(AdminConn, Ss, DDs),
            Ref = gen_fsm:start_timer(?SLEEP_BETWEEN_COMPACTION, compact),
            {next_state, wait, State#state{wait_ref=Ref}}
    end;
compact({compact, N, D, Ss, DDs}, #state{admin_conn=AdminConn
                                         ,dbs=[Db|Dbs]
                                        }=State) ->
    try lists:split(?MAX_COMPACTING_SHARDS, Ss) of
        {Compact, Shards} ->
            compact_shards(AdminConn, Compact, DDs),
            Ref = gen_fsm:start_timer(?SLEEP_BETWEEN_COMPACTION, {compact, N, D, Shards, DDs}),
            {next_state, wait, State#state{wait_ref=Ref}}
    catch
        'error':'badarg' ->
            compact_shards(AdminConn, Ss, DDs),
            Ref = gen_fsm:start_timer(?SLEEP_BETWEEN_COMPACTION, {compact, N, Db}),
            {next_state, wait, State#state{dbs=Dbs
                                           ,wait_ref=Ref
                                          }}
    end;

compact({compact_db, N, D, [], _}, #state{nodes=[]
                                          ,current_job_pid=P
                                          ,current_job_ref=Ref
                                         }=State) ->
    lager:debug("no shards to compact for ~s on ~s", [D, N]),
    maybe_send_update(P, Ref, job_finished),
    gen_fsm:send_event(self(), next_job),
    {next_state, ready, State#state{conn=undefined
                                    ,admin_conn=undefined
                                    ,current_node=undefined
                                    ,current_db=undefined
                                    ,current_job_pid=undefined
                                    ,current_job_ref=undefined
                                   }};
compact({compact_db, N, D, [], _}, #state{nodes=[Node|Ns]}=State) ->
    lager:debug("no shards to compact for ~s on ~s", [D, N]),
    gen_fsm:send_event(self(), {compact_db, Node, D}),
    {next_state, compact, State#state{nodes=Ns}};
compact({compact_db, N, D, Ss, DDs}, #state{admin_conn=AdminConn}=State) ->
    try lists:split(?MAX_COMPACTING_SHARDS, Ss) of
        {Compact, Shards} ->
            compact_shards(AdminConn, Compact, DDs),
            Ref = gen_fsm:start_timer(?SLEEP_BETWEEN_COMPACTION, {compact_db, N, D, Shards, DDs}),
            {next_state, wait, State#state{wait_ref=Ref}}
    catch
        'error':'badarg' ->
            compact_shards(AdminConn, Ss, DDs),
            Ref = gen_fsm:start_timer(?SLEEP_BETWEEN_COMPACTION, {compact_db, N, D, [], DDs}),
            {next_state, wait, State#state{wait_ref=Ref}}
    end;
compact(_Msg, State) ->
    lager:debug("unhandled msg: ~p", [_Msg]),
    {next_state, compact, State}.

compact(status, _, #state{current_node=N
                          ,current_db=D
                          ,queued_jobs=Jobs
                         }= State) ->
    {reply, {ok, [{node, N}
                  ,{db, D}
                  ,{queued_jobs, queued_jobs_status(Jobs)}
                 ]}, compact, State};

compact(cancel_current_job, _, #state{current_job_pid=P
                                      ,current_job_ref=Ref
                                     }=State) ->
    lager:debug("cancelling job"),
    maybe_send_update(P, Ref, job_cancelled),
    gen_fsm:send_event(self(), next_job),
    {reply, {ok, job_cancelled}, ready, State#state{conn=undefined
                                                    ,admin_conn=undefined
                                                    ,current_node=undefined
                                                    ,current_db=undefined
                                                    ,nodes=[]
                                                    ,dbs=[]
                                                    ,wait_ref=undefined
                                                    ,current_job_pid=undefined
                                                    ,current_job_ref=undefined
                                                   }};
compact(cancel_all_jobs, _, #state{queued_jobs=Jobs
                                   ,current_job_pid=CP
                                   ,current_job_ref=CRef
                                  }=State) ->
    lager:debug("cancelling all jobs"),

    maybe_send_update(CP, CRef, job_cancelled),

    _ = [ maybe_send_update(P, Ref, job_cancelled) || {_, P, Ref} <- queue:to_list(Jobs)],
    {reply, {ok, jobs_cancelled}, ready, State#state{conn=undefined
                                                     ,admin_conn=undefined
                                                     ,current_node=undefined
                                                     ,current_db=undefined
                                                     ,nodes=[]
                                                     ,dbs=[]
                                                     ,wait_ref=undefined
                                                     ,current_job_pid=undefined
                                                     ,current_job_ref=undefined
                                                     ,queued_jobs=queue:new()
                                                    }};
compact(Msg, {NewP, _}, #state{queued_jobs=Jobs}=State) ->
    lager:debug("recv msg, assuming new job: ~p", [Msg]),
    {Ref, Jobs1} = queue_job(Msg, NewP, Jobs),
    {reply, {queued, Ref}, compact, State#state{queued_jobs=Jobs1}}.

%%--------------------------------------------------------------------
wait({timeout, Ref, Msg}, #state{wait_ref=Ref}=State) ->
    gen_fsm:send_event(self(), Msg),
    {next_state, compact, State};
wait(_Msg, State) ->
    lager:debug("unhandled msg: ~p", [_Msg]),
    {next_state, wait, State}.

wait(status, _, #state{current_node=N
                       ,current_db=D
                       ,wait_ref=Ref
                       ,queued_jobs=Jobs
                      }= State) ->
    {reply, {ok, [{node, N}
                  ,{db, D}
                  ,{wait_left, erlang:read_timer(Ref)}
                  ,{queued_jobs, queued_jobs_status(Jobs)}
                 ]}
     ,wait, State};

wait(cancel_current_job, _, #state{current_job_pid=P
                                   ,current_job_ref=Ref
                                   ,wait_ref=WRef
                                  }=State) ->
    lager:debug("cancelling job"),
    maybe_send_update(P, Ref, job_cancelled),
    _ = erlang:cancel_timer(WRef),
    gen_fsm:send_event(self(), next_job),
    {reply, {ok, job_cancelled}, ready, State#state{conn=undefined
                                                    ,admin_conn=undefined
                                                    ,current_node=undefined
                                                    ,current_db=undefined
                                                    ,nodes=[]
                                                    ,dbs=[]
                                                    ,wait_ref=undefined
                                                    ,current_job_pid=undefined
                                                    ,current_job_ref=undefined
                                                   }};
wait(cancel_all_jobs, _, #state{queued_jobs=Jobs
                                ,current_job_pid=CP
                                ,current_job_ref=CRef
                                ,wait_ref=WRef
                               }=State) ->
    lager:debug("cancelling all jobs"),

    _ = erlang:cancel_timer(WRef),
    maybe_send_update(CP, CRef, job_cancelled),

    _ = [ maybe_send_update(P, Ref, job_cancelled) || {_, P, Ref} <- queue:to_list(Jobs)],
    {reply, {ok, jobs_cancelled}, ready, State#state{conn=undefined
                                                     ,admin_conn=undefined
                                                     ,current_node=undefined
                                                     ,current_db=undefined
                                                     ,nodes=[]
                                                     ,dbs=[]
                                                     ,wait_ref=undefined
                                                     ,current_job_pid=undefined
                                                     ,current_job_ref=undefined
                                                     ,queued_jobs=queue:new()
                                                    }};

wait(Msg, {NewP, _}, #state{queued_jobs=Jobs}=State) ->
    lager:debug("recv msg, assuming new job: ~p", [Msg]),
    {Ref, Jobs1} = queue_job(Msg, NewP, Jobs),
    {reply, {queued, Ref}, wait, State#state{queued_jobs=Jobs1}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    lager:debug("unhandled evt for ~s: ~p", [StateName, _Event]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    lager:debug("unhandled evt for ~s: ~p", [StateName, _Event]),
    {reply, {error, invalid_sync_event}, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    lager:debug("unhandled msg for ~s: ~p", [StateName, _Info]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    lager:debug("compactor FSM going down in ~s: ~p", [_StateName, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_nodes() ->
    {ok, Nodes} = couch_mgr:admin_all_docs(<<"nodes">>),
    shuffle([wh_json:get_value(<<"id">>, Node) || Node <- Nodes]).

-spec shuffle/1 :: ([ne_binary(),...]) -> [ne_binary(),...].
shuffle(L) ->
    [O || {_, O} <- lists:keysort(1, [{random:uniform(), N} || N <- L])].

encode_db(D) ->
    binary:replace(D, <<"/">>, <<"%2f">>, [global]).

encode_design_doc(Design) ->
    binary:replace(Design, <<"_design/">>, <<>>, [global]).

-spec node_dbs/1 :: (#server{}) -> {'ok', wh_json:json_strings()}.
node_dbs(AdminConn) ->
    {ok, Dbs} = couch_util:all_docs(AdminConn, <<"dbs">>, []),
    {ok, shuffle([wh_json:get_value(<<"id">>, Db) || Db <- Dbs])}.

db_shards(AdminConn, N, D) ->
    case couch_util:open_cache_doc(AdminConn, <<"dbs">>, D, []) of
        {ok, Doc} ->
            Suffix = wh_json:get_value(<<"shard_suffix">>, Doc),
            Ranges = wh_json:get_value([<<"by_node">>, N], Doc, []),
            [<<"shards%2f", Range/binary, "%2f", (encode_db(D))/binary, (wh_util:to_binary(Suffix))/binary>>
                 || Range <- Ranges
            ];
        {error, _E} ->
            lager:debug("failed to fetch shards for ~s on ~s", [D, N]),
            []
    end.

db_design_docs(Conn, D) ->
    case couch_util:all_design_docs(Conn, encode_db(D), []) of
        {ok, Designs} -> [encode_design_doc(wh_json:get_value(<<"id">>, Design)) || Design <- Designs];
        {error, _} -> []
    end.

compact_shards(AdminConn, Ss, DDs) ->
    Ps = [spawn_monitor(?MODULE, compact_shard, [AdminConn, Shard, DDs]) || Shard <- Ss],

    MaxWait = wh_util:to_integer(
                couch_config:fetch(<<"max_wait_for_compaction_pid">>, ?MAX_WAIT_FOR_COMPACTION_PID)
               ),

    wait_for_pids(MaxWait, Ps).

wait_for_pids(_, []) -> ok;
wait_for_pids(MaxWait, [{P,Ref}|Ps]) ->
    receive {'DOWN', Ref, process, P, _} -> wait_for_pids(MaxWait, Ps)
    after MaxWait -> wait_for_pids(MaxWait, Ps)
    end.

compact_shard(AdminConn, S, DDs) ->
    put(callid, S),

    wait_for_compaction(AdminConn, S),
    couch_util:db_compact(AdminConn, S),
    wait_for_compaction(AdminConn, S),

    couch_util:db_view_cleanup(AdminConn, S),
    wait_for_compaction(AdminConn, S),

    compact_design_docs(AdminConn, S, DDs),
    wait_for_compaction(AdminConn, S),

    lager:debug("finished compacting").

compact_design_docs(AdminConn, S, DDs) ->
    try lists:split(?MAX_COMPACTING_VIEWS, DDs) of
        {Compact, Remaining} ->
            _ = [couch_util:design_compact(AdminConn, S, DD) || DD <- Compact],
            wait_for_compaction(AdminConn, S),
            compact_design_docs(AdminConn, S, Remaining)
    catch
        'error':'badarg' ->
            _ = [couch_util:design_compact(AdminConn, S, DD) || DD <- DDs],
            wait_for_compaction(AdminConn, S)
    end.

wait_for_compaction(AdminConn, S) ->
    wait_for_compaction(AdminConn, S, couch_util:db_info(AdminConn, S)).

wait_for_compaction(_AdminConn, _S, {error, db_not_found}) -> ok;
wait_for_compaction(AdminConn, S, {error, _E}) ->
    ok = timer:sleep(couch_config:fetch(<<"sleep_between_poll">>, ?SLEEP_BETWEEN_POLL)),
    wait_for_compaction(AdminConn, S);
wait_for_compaction(AdminConn, S, {ok, ShardData}) ->
    case wh_json:is_true(<<"compact_running">>, ShardData, false) of
        false -> ok;
        true ->
            ok = timer:sleep(couch_config:fetch(<<"sleep_between_poll">>, ?SLEEP_BETWEEN_POLL)),
            wait_for_compaction(AdminConn, S)
    end.

get_node_connections(N, Cookie) ->
    [_, Host] = binary:split(N, <<"@">>),

    {User,Pass} = couch_mgr:get_creds(),
    {Port, AdminPort} = get_ports(wh_util:to_atom(N, true), Cookie),

    get_node_connections(Host, Port, User, Pass, AdminPort).

get_node_connections(Host, Port, User, Pass, AdminPort) ->
    {couch_util:get_new_connection(Host, Port, User, Pass),
     couch_util:get_new_connection(Host, AdminPort, User, Pass)
    }.

get_ports(Node, Cookie) ->
    erlang:set_cookie(Node, Cookie),
    case net_adm:ping(Node) =:= pong andalso get_ports(Node) of
        false -> {couch_mgr:get_port(), couch_mgr:get_admin_port()};
        Ports -> Ports
    end.

get_ports(Node) ->
    try {get_port(Node, ["chttpd", "port"], fun couch_mgr:get_port/0)
         ,get_port(Node, ["httpd", "port"], fun couch_mgr:get_admin_port/0)
        } of
        Ports -> Ports
    catch
        _E:_R ->
            lager:debug("failed to get ports: ~s: ~p", [_E, _R]),
            {couch_mgr:get_port(), couch_mgr:get_admin_port()}
    end.

get_port(Node, Key, DefaultFun) ->
    case rpc:call(Node, couch_config, get, Key) of
        {badrpc, _} -> DefaultFun();
        P -> wh_util:to_integer(P)
    end.

maybe_send_update(P, Ref, Update) when is_pid(P) ->
    case erlang:is_process_alive(P) of
        true -> P ! {Update, Ref}, ok;
        false -> ok
    end;
maybe_send_update(_,_,_) -> ok.

maybe_start_auto_compaction_job() ->
    case couch_config:fetch(<<"compact_automatically">>, false) of
        true -> gen_fsm:send_event(self(), compact);
        false -> ok
    end.

-spec queued_jobs_status/1 :: (queue()) -> 'none' | [wh_proplist(),...].
queued_jobs_status(Jobs) ->
    case queue:to_list(Jobs) of
        [] -> none;
        Js -> [[{job, J}, {requested_by, P}] || {J, P, _} <- Js]
    end.
