%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz
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
         ,compact_node/1, compact_node/2
         ,compact_db/1, compact_db/2, compact_db/3

         ,status/0
         ,is_compactor_running/0
         ,cancel_current_job/0
         ,cancel_current_shard/0
         ,cancel_all_jobs/0
         ,start_auto_compaction/0
         ,stop_auto_compaction/0
         ,compact_automatically/0, compact_automatically/1

         %% Inspection
         ,nodes_left/0
         ,dbs_left/0
         ,current_node/0
         ,current_db/0
         ,current/0
        ]).

%% Internal
-export([compact_shard/4
         ,rebuild_design_docs/3
        ]).

%% gen_fsm callbacks
-export([init/1
         ,handle_event/3
         ,handle_sync_event/4
         ,handle_info/3
         ,terminate/3
         ,code_change/4

         %% state functions
         ,ready/2, ready/3     % FSM is 'ready' to compact something
         ,compact/2, compact/3 % FSM is compacting all nodes
         ,wait/2, wait/3       % FSM is waiting to compact the next thing
        ]).

-include("wh_couch.hrl").

-define(SLEEP_BETWEEN_COMPACTION
        ,whapps_config:get_integer(?CONFIG_CAT, <<"sleep_between_compaction">>, 60 * ?MILLISECONDS_IN_SECOND)
       ).
-define(SLEEP_BETWEEN_POLL
        ,whapps_config:get_integer(?CONFIG_CAT, <<"sleep_between_poll">>, 3 * ?MILLISECONDS_IN_SECOND)
       ).
-define(SLEEP_BETWEEN_VIEWS
        ,whapps_config:get_integer(?CONFIG_CAT, <<"sleep_between_views">>, 2 * ?MILLISECONDS_IN_SECOND)
       ).
-define(MAX_COMPACTING_SHARDS
        ,whapps_config:get_integer(?CONFIG_CAT, <<"max_compacting_shards">>, 2)
       ).
-define(MAX_COMPACTING_VIEWS
        ,whapps_config:get_integer(?CONFIG_CAT, <<"max_compacting_views">>, 2)
       ).
-define(MAX_WAIT_FOR_COMPACTION_PIDS
        ,case whapps_config:get(?CONFIG_CAT, <<"max_wait_for_compaction_pids">>, 360 * ?MILLISECONDS_IN_SECOND) of
             <<"infinity">> -> 'infinity';
             N -> wh_util:to_integer(N)
         end
       ).

-define(AUTOCOMPACTION_CHECK_TIMEOUT, whapps_config:get_integer(?CONFIG_CAT, <<"autocompaction_check">>, 60 * ?MILLISECONDS_IN_SECOND)).

-define(MIN_RATIO, whapps_config:get_float(?CONFIG_CAT, <<"min_ratio">>, 1.2)).
-define(MIN_DATA, whapps_config:get_integer(?CONFIG_CAT, <<"min_data_size">>, 131072)). % 128Kb

-define(SERVER, ?MODULE).

-define(HEUR_NONE, 'none').
-define(HEUR_RATIO, 'ratio').

-type req_job() :: 'req_compact' |
                   {'req_compact_node', ne_binary(), wh_proplist()} |
                   {'req_compact_db', ne_binary()} |
                   {'req_compact_db', ne_binary(), ne_binary()}.

-type not_compacting() :: {'error', 'compactor_down'}.
-type compactor_heuristic() :: ?HEUR_NONE | ?HEUR_RATIO.

-type node_with_options() :: {ne_binary(), wh_proplist()}.
-type nodes_with_options() :: [node_with_options()].
-record(state, {
          nodes :: ne_binaries() | nodes_with_options()
          ,dbs :: ne_binaries()
          ,wait_ref :: reference()
          ,shards_pid_ref :: {pid(), reference()}  %% proc/monitor for pid of shard compactor
          ,next_compaction_msg :: tuple() | atom() %% what to send once shards_pid_ref is done

          ,current_node :: ne_binary() | node_with_options()
          ,current_db :: ne_binary()
          ,conn :: server()
          ,admin_conn :: server()

          %% [ {Job, Pid, Ref},...]
          ,queued_jobs = queue:new() :: queue:queue()
          ,current_job_pid :: pid()
          ,current_job_ref :: reference()
          ,current_job_heuristic = ?HEUR_NONE :: compactor_heuristic()
          ,current_job_start :: wh_now() | 'undefined'
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm 'process' which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {'ok', Pid} | ignore | {'error', Error}
%% @end
%%--------------------------------------------------------------------
start_link() -> gen_fsm:start_link({'local', ?SERVER}, ?MODULE, [], []).

-spec compact() -> {'queued', reference()} | not_compacting().
compact() ->
    case is_compactor_running() of
        'true' -> gen_fsm:sync_send_event(?SERVER, 'req_compact');
        'false' -> {'error', 'compactor_down'}
    end.

-spec compact_node(ne_binary()) ->
                          {'queued', reference()} |
                          not_compacting().
-spec compact_node(ne_binary(), wh_proplist()) ->
                          {'queued', reference()} |
                          not_compacting().
compact_node(Node) ->
    compact_node(Node, []).
compact_node(Node, Opts) ->
    case is_compactor_running() of
        'true' -> gen_fsm:sync_send_event(?SERVER, {'req_compact_node', Node, Opts});
        'false' -> {'error', 'compactor_down'}
    end.

-spec compact_db(ne_binary()) ->
                        {'queued', reference()} |
                        not_compacting().
-spec compact_db(ne_binary(), ne_binary()) ->
                        {'queued', reference()} |
                        not_compacting().
-spec compact_db(ne_binary(), ne_binary(), wh_proplist()) ->
                        {'queued', reference()} |
                        not_compacting().
compact_db(Db) ->
    case is_compactor_running() of
        'true' -> gen_fsm:sync_send_event(?SERVER, {'req_compact_db', Db});
        'false' -> {'error', 'compactor_down'}
    end.

compact_db(Node, Db) ->
    case is_compactor_running() of
        'true' -> gen_fsm:sync_send_event(?SERVER, {'req_compact_db', Node, Db, []});
        'false' -> {'error', 'compactor_down'}
    end.

compact_db(Node, Db, Opts) ->
    case is_compactor_running() of
        'true' -> gen_fsm:sync_send_event(?SERVER, {'req_compact_db', Node, Db, Opts});
        'false' -> {'error', 'compactor_down'}
    end.

-spec status() -> {'ok', 'ready' | 'not_running' | wh_proplist()}.
status() ->
    case is_compactor_running() of
        'true' -> gen_fsm:sync_send_event(?SERVER, 'status');
        'false' -> {'ok', 'not_running'}
    end.

-spec cancel_current_job() -> {'ok', 'job_cancelled'} |
                              {'error', 'no_job_running'} |
                              not_compacting().
cancel_current_job() ->
    case is_compactor_running() of
        'true' -> gen_fsm:sync_send_event(?SERVER, 'cancel_current_job');
        'false' -> {'error', 'compactor_down'}
    end.

-spec cancel_current_shard() -> {'ok', 'shard_cancelled'} |
                                {'error', 'no_shard_running'} |
                                not_compacting().
cancel_current_shard() ->
    case is_compactor_running() of
        'true' -> gen_fsm:sync_send_event(?SERVER, 'cancel_current_shard');
        'false' -> {'error', 'compactor_down'}
    end.

-spec cancel_all_jobs() -> {'ok', 'jobs_cancelled'} | not_compacting().
cancel_all_jobs() ->
    case is_compactor_running() of
        'true' -> gen_fsm:sync_send_event(?SERVER, 'cancel_all_jobs');
        'false' -> {'error', 'compactor_down'}
    end.

-spec start_auto_compaction() -> {'ok', 'already_started'} |
                                 {'queued', reference()} |
                                 not_compacting().
start_auto_compaction() ->
    case is_compactor_running() of
        'true' ->
            case compact_automatically() of
                'true' -> {'ok', 'already_started'};
                'false' ->
                    _ = compact_automatically('true'),
                    compact()
            end;
        'false' -> {'error', 'compactor_down'}
    end.

-spec stop_auto_compaction() -> {'ok', 'updated' | 'already_stopped'} |
                                not_compacting().
stop_auto_compaction() ->
    case is_compactor_running() of
        'true' ->
            case compact_automatically() of
                'false' -> {'ok', 'already_stopped'};
                'true' ->
                    _ = compact_automatically('false'),
                    {'ok', 'updated'}
            end;
        'false' -> {'error', 'compactor_down'}
    end.

-spec is_compactor_running() -> boolean().
is_compactor_running() ->
    is_pid(whistle_couch_sup:compactor_pid()).

nodes_left() -> gen_fsm:sync_send_all_state_event(?SERVER, 'nodes_left').
dbs_left() -> gen_fsm:sync_send_all_state_event(?SERVER, 'dbs_left').
current_node() ->
    {N,_} = gen_fsm:sync_send_all_state_event(?SERVER, 'current'),
    N.
current_db() ->
    {_,D} = gen_fsm:sync_send_all_state_event(?SERVER, 'current'),
    D.
current() -> gen_fsm:sync_send_all_state_event(?SERVER, 'current').

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% 'process' to initialize.
%%
%% @spec init(Args) -> {'ok', StateName, State} |
%%                     {'ok', StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    _ = random:seed(wh_util:now()),
    wh_util:put_callid(?MODULE),
    self() ! '$maybe_start_auto_compaction_job',
    {'ok', 'ready', #state{conn='undefined'
                           ,admin_conn='undefined'
                          }}.

%%--------------------------------------------------------------------
ready('compact', State) ->
    lager:debug("start compaction on all nodes"),
    gen_fsm:send_event(self(), 'compact'),
    {'next_state', 'compact', State#state{nodes=get_nodes()
                                          ,conn='undefined'
                                          ,admin_conn='undefined'
                                          ,current_node='undefined'
                                          ,current_db='undefined'
                                          ,current_job_heuristic=?HEUR_RATIO
                                          ,current_job_start=os:timestamp()
                                         }};
ready({'compact_node', N, Opts}, State) ->
    lager:debug("start compaction on node w/ options"),
    gen_fsm:send_event(self(), 'compact'),
    {'next_state', 'compact', State#state{nodes=[{N, Opts}]
                                          ,conn='undefined'
                                          ,admin_conn='undefined'
                                          ,current_node={N, Opts}
                                          ,current_db='undefined'
                                          ,current_job_heuristic=?HEUR_RATIO
                                          ,current_job_start=os:timestamp()
                                         }};
ready({'compact_db', D}, State) ->
    [N|Ns] = get_nodes(D),
    lager:debug("start compaction on node's db"),
    gen_fsm:send_event(self(), {'compact_db', N, D}),
    {'next_state', 'compact', State#state{nodes=Ns
                                          ,dbs=[D]
                                          ,conn='undefined'
                                          ,admin_conn='undefined'
                                          ,current_node=N
                                          ,current_db=D
                                          ,current_job_heuristic=?HEUR_NONE
                                          ,current_job_start=os:timestamp()
                                         }};
ready({'compact_db', N, D, Opts}, State) ->
    lager:debug("start compaction on node's db"),
    gen_fsm:send_event(self(), {'compact_db', {N, Opts}, D}),
    {'next_state', 'compact', State#state{nodes=[]
                                          ,dbs=[]
                                          ,conn='undefined'
                                          ,admin_conn='undefined'
                                          ,current_node=N
                                          ,current_db=D
                                          ,current_job_heuristic=?HEUR_NONE
                                          ,current_job_start=os:timestamp()
                                         }};
ready('next_job', #state{queued_jobs=Jobs}=State) ->
    case queue:out(Jobs) of
        {'empty', _} ->
            maybe_start_auto_compaction_job(),
            lager:debug("returning to 'ready'"),
            {'next_state', 'ready', State#state{current_job_pid='undefined'
                                                ,current_job_ref='undefined'
                                                ,current_job_start='undefined'
                                               }};
        {{'value', {Job, P, Ref}}, Jobs1} ->
            maybe_send_update(P, Ref, 'job_starting'),
            gen_fsm:send_event(self(), Job),
            lager:debug("starting queued job for ~p:~p: ~p", [P, Ref, Job]),
            lager:debug("returning to 'ready'"),
            {'next_state', 'ready', State#state{queued_jobs=Jobs1
                                                ,current_job_pid=P
                                                ,current_job_ref=Ref
                                                ,current_job_start=os:timestamp()
                                               }}
    end;
ready(_Msg, State) ->
    lager:debug("unhandled msg in ready: ~p", [_Msg]),
    {'next_state', 'ready', State}.

ready('status', _, #state{}=State) ->
    {'reply', {'ok', 'ready'}, 'ready', State};

ready('cancel_current_job', _, State) ->
    {'reply', {'error', 'no_job_running'}, 'ready', State};
ready('cancel_current_shard', _, State) ->
    {'reply', {'error', 'no_shard_running'}, 'ready', State};

ready('cancel_all_jobs', _, #state{queued_jobs=Jobs}=State) ->
    _ = [ maybe_send_update(P, Ref, 'job_cancelled') || {_, P, Ref} <- queue:to_list(Jobs)],
    {'reply', {'ok', 'jobs_cancelled'}, 'ready', State#state{nodes=[]
                                                             ,dbs=[]
                                                             ,wait_ref='undefined'
                                                             ,shards_pid_ref='undefined'
                                                             ,next_compaction_msg='undefined'
                                                             ,current_node='undefined'
                                                             ,current_db='undefined'
                                                             ,conn='undefined'
                                                             ,admin_conn='undefined'
                                                             ,queued_jobs=queue:new()
                                                             ,current_job_pid='undefined'
                                                             ,current_job_ref='undefined'
                                                             ,current_job_heuristic = ?HEUR_NONE
                                                             ,current_job_start='undefined'
                                                            }};

ready(Msg, {NewP, _}, #state{queued_jobs=Jobs}=State) ->
    case queue:out(Jobs) of
        {'empty', _} ->
            {Ref, Jobs1} = queue_job(Msg, NewP, Jobs),
            lager:debug("next job please"),
            gen_fsm:send_event(self(), 'next_job'),
            {'reply', {'queued', Ref}, 'ready', State#state{queued_jobs=Jobs1}};

        {{'value', {Job, P, Ref}}, Jobs1} ->
            maybe_send_update(P, Ref, 'job_starting'),
            gen_fsm:send_event(self(), Job),
            lager:debug("starting queued job for ~p:~p: ~p", [P, Ref, Job]),

            {Ref2, Jobs2} = queue_job(Msg, NewP, Jobs1),

            {'reply', {'queued', Ref2}, 'ready', State#state{queued_jobs=Jobs2
                                                             ,current_job_pid=P
                                                             ,current_job_ref=Ref
                                                             ,current_job_start=os:timestamp()
                                                            }}
    end.

-spec queue_job(req_job(), pid(), queue:queue()) -> {reference(), queue:queue()}.
queue_job('req_compact', Pid, Jobs) ->
    Ref = erlang:make_ref(),
    {Ref, queue:in({'compact', Pid, Ref}, Jobs)};
queue_job({'req_compact_node', Node, Opts}, Pid, Jobs) ->
    Ref = erlang:make_ref(),
    {Ref, queue:in({{'compact_node', Node, Opts}, Pid, Ref}, Jobs)};
queue_job({'req_compact_db', Db}, Pid, Jobs) ->
    Ref = erlang:make_ref(),
    {Ref, queue:in({{'compact_db', Db}, Pid, Ref}, Jobs)};
queue_job({'req_compact_db', Node, Db, Opts}, Pid, Jobs) ->
    Ref = erlang:make_ref(),
    {Ref, queue:in({{'compact_db', Node, Db, Opts}, Pid, Ref}, Jobs)}.

%%--------------------------------------------------------------------
compact({'compact', N}, #state{conn='undefined'
                               ,admin_conn='undefined'
                               ,nodes=[]
                               ,current_job_pid=Pid
                               ,current_job_ref=Ref
                              }=State) ->
    Cookie = wh_couch_connections:get_node_cookie(),
    try get_node_connections(N, Cookie) of
        {'error', _E} ->
            lager:debug("failed to connect to node ~p: ~p", [N, _E]),
            maybe_send_update(Pid, Ref, 'job_finished'),
            gen_fsm:send_event(self(), 'next_job'),
            lager:debug("returning to 'ready'"),
            {'next_state'
             ,'ready'
             ,State#state{conn='undefined'
                          ,admin_conn='undefined'
                          ,current_node='undefined'
                          ,current_db='undefined'
                          ,current_job_pid='undefined'
                          ,current_job_ref='undefined'
                          ,current_job_start='undefined'
                         }
             ,'hibernate'
            };
        {Conn, AdminConn} ->
            lager:debug("got conns, let's compact"),
            gen_fsm:send_event(self(), {'compact', N}),
            {'next_state', 'compact', State#state{conn=Conn
                                                  ,admin_conn=AdminConn
                                                  ,current_node=N
                                                 }}
    catch
        _:{'error', {'conn_failed', {'error', 'etimedout'}}} ->
            lager:debug("failed to connect to node ~s: timed out", [N]),
            maybe_send_update(Pid, Ref, 'job_finished'),
            gen_fsm:send_event(self(), 'next_job'),
            lager:debug("returning to 'ready'"),
            {'next_state'
             ,'ready'
             ,State#state{conn='undefined'
                          ,admin_conn='undefined'
                          ,current_node='undefined'
                          ,current_job_pid='undefined'
                          ,current_job_ref='undefined'
                          ,current_job_start='undefined'
                         }
             ,'hibernate'
            }
    end;
compact({'compact', N}=Msg, #state{conn='undefined'
                                   ,admin_conn='undefined'
                                   ,nodes=[Node|Ns]
                                  }=State) ->
    Cookie = wh_couch_connections:get_node_cookie(),
    try get_node_connections(N, Cookie) of
        {'error', _E} ->
            lager:debug("failed to connect to node ~s: ~p", [N, _E]),
            gen_fsm:send_event(self(), {'compact', Node}),
            {'next_state', 'compact', State#state{nodes=Ns
                                                  ,current_node='undefined'
                                                 }};
        {Conn, AdminConn} ->
            lager:debug("got conns, let's compact"),
            gen_fsm:send_event(self(), Msg),
            {'next_state', 'compact', State#state{conn=Conn
                                                  ,admin_conn=AdminConn
                                                  ,current_node=N
                                                 }}
    catch
        _:{'error', {'conn_failed', {'error', 'etimedout'}}} ->
            lager:debug("failed to connect to node ~s: timed out", [N]),
            gen_fsm:send_event(self(), {'compact', Node}),
            {'next_state', 'compact', State#state{nodes=Ns
                                                  ,current_node='undefined'
                                                 }}
    end;

compact({'compact_db', N, D}=Msg, #state{conn='undefined'
                                         ,admin_conn='undefined'
                                         ,nodes=[]
                                         ,current_job_pid=Pid
                                         ,current_job_ref=Ref
                                        }=State) ->
    Cookie = wh_couch_connections:get_node_cookie(),
    try get_node_connections(N, Cookie) of
        {'error', _} ->
            lager:debug("failed to connect to node ~s: timed out", [N]),
            maybe_send_update(Pid, Ref, 'job_finished'),
            gen_fsm:send_event(self(), 'next_job'),
            lager:debug("returning to 'ready'"),
            {'next_state'
             ,'ready'
             ,State#state{conn='undefined'
                          ,admin_conn='undefined'
                          ,current_node='undefined'
                          ,current_db='undefined'
                          ,current_job_pid='undefined'
                          ,current_job_ref='undefined'
                          ,current_job_start='undefined'
                         }
             ,'hibernate'
            };
        {Conn, AdminConn} ->
            lager:debug("got conns, let's compact"),
            gen_fsm:send_event(self(), Msg),
            {'next_state', 'compact', State#state{conn=Conn
                                                  ,admin_conn=AdminConn
                                                  ,current_node=N
                                                  ,current_db=D
                                                 }}
    catch
        _:{'error', {'conn_failed', {'error', 'etimedout'}}} ->
            lager:debug("failed to connect to node ~s: timed out", [N]),
            maybe_send_update(Pid, Ref, 'job_finished'),
            gen_fsm:send_event(self(), 'next_job'),
            lager:debug("returning to 'ready'"),
            {'next_state'
             ,'ready'
             ,State#state{conn='undefined'
                          ,admin_conn='undefined'
                          ,current_node='undefined'
                          ,current_db='undefined'
                          ,current_job_pid='undefined'
                          ,current_job_ref='undefined'
                          ,current_job_start='undefined'
                         }
             ,'hibernate'
            }
    end;

compact({'compact_db', N, D}=Msg, #state{conn='undefined'
                                         ,admin_conn='undefined'
                                         ,nodes=[Node|Ns]
                                        }=State) ->
    Cookie = wh_couch_connections:get_node_cookie(),
    try get_node_connections(N, Cookie) of
        {'error', _E} ->
            lager:debug("failed to connect to node ~s: ~p", [N, _E]),
            gen_fsm:send_event(self(), {'compact_db', Node, D}),
            {'next_state', 'compact', State#state{nodes=Ns
                                                  ,current_node=Node
                                                  ,current_db=D
                                                 }};
        {Conn, AdminConn} ->
            lager:debug("got conns, let's compact"),
            gen_fsm:send_event(self(), Msg),
            {'next_state', 'compact', State#state{conn=Conn
                                                  ,admin_conn=AdminConn
                                                  ,current_node=N
                                                  ,current_db=D
                                                 }}
    catch
        _:{'error', {'conn_failed', {'error', 'etimedout'}}} ->
            lager:debug("failed to connect to node ~s: timed out", [N]),
            gen_fsm:send_event(self(), {'compact_db', Node, D}),
            {'next_state', 'compact', State#state{nodes=Ns
                                                  ,current_node=Node
                                                  ,current_db=D
                                                 }}
    end;

compact('compact', #state{nodes=[]
                          ,dbs=[]
                          ,current_job_pid=Pid
                          ,current_job_ref=Ref
                         }=State) ->
    lager:debug("no nodes to compact: ~p", [State]),
    maybe_send_update(Pid, Ref, 'job_finished'),
    gen_fsm:send_event(self(), 'next_job'),
    lager:debug("returning to 'ready'"),
    {'next_state'
     ,'ready'
     ,State#state{conn='undefined'
                  ,admin_conn='undefined'
                  ,current_node='undefined'
                  ,current_db='undefined'
                  ,current_job_pid='undefined'
                  ,current_job_ref='undefined'
                  ,current_job_start='undefined'
                 }
     ,'hibernate'
    };

compact('compact', #state{nodes=[{N, _}=Node|Ns]}=State) ->
    lager:debug("compact node ~s", [N]),
    gen_fsm:send_event(self(), {'compact', Node}),
    {'next_state', 'compact', State#state{conn='undefined'
                                          ,admin_conn='undefined'
                                          ,nodes=Ns
                                         }};
compact('compact', #state{nodes=[N|Ns]}=State) ->
    lager:debug("compact node ~s", [N]),
    gen_fsm:send_event(self(), {'compact', N}),
    {'next_state', 'compact', State#state{conn='undefined'
                                          ,admin_conn='undefined'
                                          ,nodes=Ns
                                         }};
compact({'compact', {N, _}}, #state{admin_conn=AdminConn}=State) ->
    lager:debug("compacting node ~s w/ options", [N]),

    {'ok', [D|Ds]} = node_dbs(AdminConn),
    gen_fsm:send_event(self(), {'compact', N, D}),
    {'next_state', 'compact', State#state{dbs=Ds
                                          ,current_db=D
                                          ,current_node=N
                                         }};
compact({'compact', N}, #state{admin_conn=AdminConn}=State) ->
    lager:debug("compacting node ~s", [N]),

    {'ok', [D|Ds]} = node_dbs(AdminConn),
    gen_fsm:send_event(self(), {'compact', N, D}),
    {'next_state', 'compact', State#state{dbs=Ds
                                          ,current_db=D
                                          ,current_node=N
                                         }};
compact({'compact', {N, _}, D}, State) ->
    lager:debug("compacting node ~s db ~s", [N, D]),
    gen_fsm:send_event(self(), {'compact', N, D}),
    {'next_state', 'compact', State};
compact({'compact', N, <<"dbs">> = D}, State) ->
    lager:debug("compacting the 'dbs' db"),
    gen_fsm:send_event(self(), {'compact', N, D, [], []}),
    {'next_state', 'compact', State};
compact({'compact', N, D}, #state{conn=Conn
                                  ,admin_conn=AdminConn
                                  ,dbs=[]
                                  ,current_job_heuristic=Heur
                                 }=State) ->
    lager:debug("checking if we should compact ~s on ~s", [D, N]),

    Encoded = encode_db(D),
    case couch_util:db_exists(Conn, Encoded) andalso
        should_compact(Conn, Encoded, Heur)
    of
        'false' ->
            lager:debug("db ~s not found on ~s OR heuristic not met", [D, N]),
            gen_fsm:send_event_after(?SLEEP_BETWEEN_POLL, 'compact'),
            {'next_state', 'compact', State#state{current_db='undefined'}};
        'true' ->
            lager:debug("db exists and should compact"),
            Ss = db_shards(AdminConn, N, D),
            DDs = db_design_docs(Conn, D),
            gen_fsm:send_event(self(), {'compact', N, D, Ss, DDs}),
            lager:debug("compacting ~s on ~s", [D, N]),
            {'next_state', 'compact', State#state{current_db=D
                                                  ,current_node=N
                                                 }}
    end;
compact({'compact', N, D}, #state{conn=Conn
                                  ,admin_conn=AdminConn
                                  ,dbs=[Db|Dbs]
                                  ,current_job_heuristic=Heur
                                 }=State) ->
    lager:debug("checking if we should compact ~s on ~s", [D, N]),

    Encoded = encode_db(D),
    case couch_util:db_exists(Conn, Encoded)
        andalso should_compact(Conn, Encoded, Heur)
    of
        'false' ->
            lager:debug("db ~s not found on ~s OR heuristic not met", [D, N]),
            gen_fsm:send_event_after(?SLEEP_BETWEEN_POLL, {'compact', N, Db}),
            {'next_state', 'compact', State#state{dbs=Dbs
                                                  ,current_db=Db
                                                  ,current_node=N
                                                 }};
        'true' ->
            lager:debug("db exists and should compact"),
            Ss = db_shards(AdminConn, N, D),
            DDs = db_design_docs(Conn, D),
            gen_fsm:send_event(self(), {'compact', N, D, Ss, DDs}),
            lager:debug("compacting ~s on ~s", [D, N]),
            {'next_state', 'compact', State#state{current_db=D
                                                  ,current_node=N
                                                 }}
    end;
compact({'compact_db', {N, _}, D}, State) ->
    lager:debug("compacting node ~s on ~s", [N, D]),
    gen_fsm:send_event(self(), {'compact_db', N, D}),
    {'next_state', 'compact', State};
compact({'compact_db', N, D}, #state{conn=Conn
                                     ,admin_conn=AdminConn
                                     ,nodes=[]
                                     ,current_job_pid=Pid
                                     ,current_job_ref=Ref
                                     ,current_job_heuristic=Heur
                                    }=State) ->
    lager:debug("checking if we should compact ~s on ~s", [D, N]),
    Encoded = encode_db(D),
    case couch_util:db_exists(Conn, Encoded) andalso
        should_compact(Conn, Encoded, Heur)
    of
        'false' ->
            lager:debug("db ~s not found on ~s OR heuristic not met", [D, N]),
            maybe_send_update(Pid, Ref, 'job_finished'),
            _R = gen_fsm:send_event_after(?SLEEP_BETWEEN_POLL, 'next_job'),
            lager:debug("returning to 'ready': ~p", [_R]),
            {'next_state'
             ,'ready'
             ,State#state{conn='undefined'
                          ,admin_conn='undefined'
                          ,current_node='undefined'
                          ,current_db='undefined'
                          ,current_job_pid='undefined'
                          ,current_job_ref='undefined'
                          ,current_job_start='undefined'
                         }
             ,'hibernate'
            };
        'true' ->
            lager:debug("db exists and should compact"),
            Ss = db_shards(AdminConn, N, D),
            DDs = db_design_docs(Conn, D),
            gen_fsm:send_event(self(), {'compact_db', N, D, Ss, DDs}),
            lager:debug("compacting ~s on ~s", [D, N]),
            {'next_state', 'compact', State#state{current_node=N
                                                  ,current_db=D
                                                 }}
    end;
compact({'compact_db', N, D}, #state{conn=Conn
                                     ,admin_conn=AdminConn
                                     ,nodes=[Node|Ns]
                                     ,current_job_heuristic=Heur
                                    }=State) ->
    lager:debug("checking if we should compact ~s on ~s", [D, N]),

    Encoded = encode_db(D),
    case couch_util:db_exists(Conn, Encoded) andalso
        should_compact(Conn, Encoded, Heur)
    of
        'false' ->
            lager:debug("db ~s not found on ~s OR heuristic not met", [D, N]),
            gen_fsm:send_event_after(?SLEEP_BETWEEN_POLL, {'compact_db', Node, D}),
            {'next_state', 'compact', State#state{nodes=Ns
                                                  ,current_node=Node
                                                  ,current_db=D
                                                 }};
        'true' ->
            lager:debug("db exists and should compact"),
            Ss = db_shards(AdminConn, N, D),
            DDs = db_design_docs(Conn, D),
            gen_fsm:send_event(self(), {'compact_db', N, D, Ss, DDs}),
            lager:debug("compacting db '~s' on node '~s'", [D, N]),
            {'next_state', 'compact', State#state{current_node=N
                                                  ,current_db=D
                                                 }}
    end;

compact({'compact', N, <<"dbs">> = D, _Shards, _DDs}, #state{conn=Conn
                                                             ,admin_conn=AdminConn
                                                             ,dbs=Dbs
                                                            }=State) ->
    ShardsPidRef = compact_shards(Conn, AdminConn, N, [D], []),

    {NextMsg, NextDbs} =
        case Dbs of
            [] -> {'compact', Dbs};
            [Db|T] ->  {{'compact', N, Db}, T}
        end,

    {'next_state', 'compact', State#state{shards_pid_ref=ShardsPidRef
                                          ,next_compaction_msg=NextMsg
                                          ,dbs=NextDbs
                                         }};
compact({'compact', N, D, [], _}, #state{dbs=[]}=State) ->
    lager:debug("no shards to compact for ~s on ~s", [D, N]),
    gen_fsm:send_event(self(), 'compact'),
    {'next_state', 'compact', State};
compact({'compact', N, D, [], _}, #state{dbs=[Db|Dbs]}=State) ->
    lager:debug("no shards to compact for ~s on ~s", [D, N]),
    gen_fsm:send_event(self(), {'compact', N, Db}),
    {'next_state', 'compact', State#state{dbs=Dbs}};

compact({'compact', N, D, Ss, DDs}, #state{admin_conn=AdminConn
                                           ,conn=Conn
                                           ,dbs=[]
                                          }=State) ->
    try lists:split(?MAX_COMPACTING_SHARDS, Ss) of
        {Compact, Shards} ->
            lager:debug("compacting ~b shards for ~s on ~s", [?MAX_COMPACTING_SHARDS, D, N]),
            ShardsPidRef = compact_shards(Conn, AdminConn, N, Compact, DDs),
            {'next_state', 'compact', State#state{shards_pid_ref=ShardsPidRef
                                                  ,next_compaction_msg={'compact', N, D, Shards, DDs}
                                                 }}
    catch
        'error':'badarg' ->
            lager:debug("compacting last of the shards for ~s on ~s", [D, N]),
            ShardsPidRef = compact_shards(Conn, AdminConn, N, Ss, DDs),
            {'next_state', 'compact', State#state{shards_pid_ref=ShardsPidRef
                                                  ,next_compaction_msg='compact'
                                                 }}
    end;
compact({'compact', N, D, Ss, DDs}, #state{admin_conn=AdminConn
                                           ,conn=Conn
                                           ,dbs=[Db|Dbs]
                                          }=State) ->
    try lists:split(?MAX_COMPACTING_SHARDS, Ss) of
        {Compact, Shards} ->
            lager:debug("compacting ~b shards for ~s on ~s", [?MAX_COMPACTING_SHARDS, D, N]),
            ShardsPidRef = compact_shards(Conn, AdminConn, N, Compact, DDs),
            {'next_state', 'compact', State#state{shards_pid_ref=ShardsPidRef
                                                  ,next_compaction_msg={'compact', N, D, Shards, DDs}
                                                 }}
    catch
        'error':'badarg' ->
            lager:debug("compacting last of the shards for ~s on ~s", [D, N]),
            ShardsPidRef = compact_shards(Conn, AdminConn, N, Ss, DDs),
            {'next_state', 'compact', State#state{dbs=Dbs
                                                  ,shards_pid_ref=ShardsPidRef
                                                  ,next_compaction_msg={'compact', N, Db}
                                                 }}
    end;

compact({'compact', N, _D, [], _}, #state{dbs=[Db|Dbs]}=State) ->
    lager:debug("compacting node ~s with new db ~s", [N, Db]),
    gen_fsm:send_event(self(), {'compact', N, Db}),
    {'next_state', 'compact', State#state{dbs=Dbs
                                          ,shards_pid_ref='undefined'
                                          ,next_compaction_msg='undefined'
                                         }};
compact({'compact_db', N, D, [], _}, #state{nodes=[]
                                            ,current_job_pid=Pid
                                            ,current_job_ref=Ref
                                           }=State) ->
    lager:debug("no shards to compact for ~s on ~s", [D, N]),
    maybe_send_update(Pid, Ref, 'job_finished'),
    gen_fsm:send_event(self(), 'next_job'),
    lager:debug("returning to 'ready'"),
    {'next_state'
     ,'ready'
     ,State#state{conn='undefined'
                  ,admin_conn='undefined'
                  ,current_node='undefined'
                  ,current_db='undefined'
                  ,current_job_pid='undefined'
                  ,current_job_ref='undefined'
                  ,current_job_start='undefined'
                  ,next_compaction_msg='undefined'
                 }
     ,'hibernate'
    };

compact({'rebuild_views', N, D, DDs}, #state{conn=Conn}=State) ->
    _P = wh_util:spawn(
           fun() ->
                   wh_util:put_callid(N),
                   ?MODULE:rebuild_design_docs(Conn, encode_db(D), DDs)
           end),
    lager:debug("rebuilding views in ~p", [_P]),
    gen_fsm:send_event(self(), {'compact_db', N, D, [], DDs}),
    {'next_state', 'compact', State};

compact({'compact_db', N, D, [], _}, #state{nodes=[Node|Ns]}=State) ->
    lager:debug("no shards left to compact for db '~s' on node '~s'", [D, N]),
    gen_fsm:send_event(self(), {'compact_db', Node, D}),
    {'next_state', 'compact', State#state{conn='undefined'
                                         ,admin_conn='undefined'
                                         ,nodes=Ns
                                         ,dbs=[D]
                                         }};
compact({'compact_db', N, D, Ss, DDs}, #state{admin_conn=AdminConn
                                              ,conn=Conn
                                             }=State) ->
    lager:debug("compacting shards for db '~s' on node '~s'", [D, N]),
    try lists:split(?MAX_COMPACTING_SHARDS, Ss) of
        {Compact, Shards} ->
            ShardsPidRef = compact_shards(Conn, AdminConn, N, Compact, DDs),
            {'next_state', 'compact', State#state{shards_pid_ref=ShardsPidRef
                                                  ,next_compaction_msg={'compact_db', N, D, Shards, DDs}
                                                 }}
    catch
        'error':'badarg' ->
            ShardsPidRef = compact_shards(Conn, AdminConn, N, Ss, DDs),
            {'next_state', 'compact', State#state{shards_pid_ref=ShardsPidRef
                                                  ,next_compaction_msg={'rebuild_views', N, D, DDs}
                                                 }}
    end;
compact(_Msg, State) ->
    lager:debug("unhandled compact/2 msg: ~p", [_Msg]),
    {'next_state', 'compact', State}.

compact('status', _, #state{current_node=N
                            ,current_db=D
                            ,queued_jobs=Jobs
                            ,dbs=Dbs
                            ,nodes=Ns
                            ,current_job_start=Start
                           }= State) ->
    {'reply', {'ok', [{'node', N}
                      ,{'db', D}
                      ,{'queued_jobs', queued_jobs_status(Jobs)}
                      ,{'nodes_left', length(Ns)}
                      ,{'dbs_left', length(Dbs)}
                      ,{'start_time', calendar:now_to_universal_time(Start)}
                      ,{'elapsed_s', wh_util:elapsed_s(Start)}
                     ]}, 'compact', State};

compact('cancel_current_shard', _, #state{shards_pid_ref='undefined'}=State) ->
    {'reply', {'ok', 'shard_cancelled'}, 'compact', State};
compact('cancel_current_shard', _, #state{shards_pid_ref={Pid, Ref}}=State) ->
    lager:debug("cancelling pidref ~p(~p)", [Pid, Ref]),
    Pid ! 'cancel_shard',
    {'reply', {'ok', 'shard_cancelled'}, 'compact', State};
compact('cancel_current_job', _, #state{current_job_pid=Pid
                                        ,current_job_ref=Ref
                                       }=State) ->
    lager:debug("cancelling job ~p(~p)", [Pid, Ref]),
    maybe_send_update(Pid, Ref, 'job_cancelled'),
    gen_fsm:send_event(self(), 'next_job'),
    lager:debug("returning to 'ready'"),
    {'reply'
     ,{'ok', 'job_cancelled'}
     ,'ready'
     ,State#state{conn='undefined'
                  ,admin_conn='undefined'
                  ,current_node='undefined'
                  ,current_db='undefined'
                  ,nodes=[]
                  ,dbs=[]
                  ,wait_ref='undefined'
                  ,current_job_pid='undefined'
                  ,current_job_ref='undefined'
                  ,current_job_start='undefined'
                 }
     ,'hibernate'
    };
compact('cancel_all_jobs', _, #state{queued_jobs=Jobs
                                     ,current_job_pid=CPid
                                     ,current_job_ref=CRef
                                  }=State) ->
    lager:debug("cancelling all jobs"),

    maybe_send_update(CPid, CRef, 'job_cancelled'),

    _ = [ maybe_send_update(P, Ref, 'job_cancelled') || {_, P, Ref} <- queue:to_list(Jobs)],
    lager:debug("returning to 'ready'"),
    {'reply'
     ,{'ok', 'jobs_cancelled'}
     ,'ready'
     ,State#state{conn='undefined'
                  ,admin_conn='undefined'
                  ,current_node='undefined'
                  ,current_db='undefined'
                  ,nodes=[]
                  ,dbs=[]
                  ,wait_ref='undefined'
                  ,current_job_pid='undefined'
                  ,current_job_ref='undefined'
                  ,current_job_start='undefined'
                  ,queued_jobs=queue:new()
                 }
     ,'hibernate'
    };
compact(Msg, {NewP, _}, #state{queued_jobs=Jobs}=State) ->
    lager:debug("recv msg, assuming new job: ~p", [Msg]),
    {Ref, Jobs1} = queue_job(Msg, NewP, Jobs),
    {'reply', {'queued', Ref}, 'compact', State#state{queued_jobs=Jobs1}}.

%%--------------------------------------------------------------------
wait({'timeout', Ref, Msg}, #state{wait_ref=Ref}=State) ->
    gen_fsm:send_event(self(), Msg),
    lager:debug("done waiting for ~p, compacting with ~p", [Ref, Msg]),
    {'next_state', 'compact', State#state{wait_ref='undefined'}};
wait(_Msg, State) ->
    lager:debug("unhandled wait/2 msg: ~p", [_Msg]),
    {'next_state', 'wait', State, 'hibernate'}.

wait('status', _, #state{current_node=N
                         ,current_db=D
                         ,wait_ref=Ref
                         ,queued_jobs=Jobs
                         ,nodes=Ns
                         ,dbs=Dbs
                         ,current_job_start=Start
                        }= State) ->
    {'reply'
     ,{'ok', [{'node', N}
              ,{'db', D}
              ,{'wait_left', erlang:read_timer(Ref)}
              ,{'queued_jobs', queued_jobs_status(Jobs)}
              ,{'nodes_left', length(Ns)}
              ,{'dbs_left', length(Dbs)}
              ,{'start_time', calendar:now_to_universal_time(Start)}
              ,{'elapsed_s', wh_util:elapsed_s(Start)}
             ]}
     ,'wait'
     ,State
    };
wait('cancel_current_shard', _, State) ->
    {'reply', {'error', 'no_shard_running'}, 'wait', State, 'hibernate'};
wait('cancel_current_job', _, #state{current_job_pid=Pid
                                     ,current_job_ref=Ref
                                     ,wait_ref=WRef
                                    }=State) ->
    lager:debug("cancelling job"),
    maybe_send_update(Pid, Ref, 'job_cancelled'),
    _ = erlang:cancel_timer(WRef),
    gen_fsm:send_event(self(), 'next_job'),
    lager:debug("returning to 'ready'"),
    {'reply'
     ,{'ok', 'job_cancelled'}
     ,'ready'
     ,State#state{conn='undefined'
                  ,admin_conn='undefined'
                  ,current_node='undefined'
                  ,current_db='undefined'
                  ,nodes=[]
                  ,dbs=[]
                  ,wait_ref='undefined'
                  ,current_job_pid='undefined'
                  ,current_job_ref='undefined'
                  ,current_job_start='undefined'
                 }
     ,'hibernate'
    };
wait('cancel_all_jobs', _, #state{queued_jobs=Jobs
                                  ,current_job_pid=CPid
                                  ,current_job_ref=CRef
                                  ,wait_ref=WRef
                                 }=State) ->
    lager:debug("cancelling all jobs"),

    _ = erlang:cancel_timer(WRef),
    maybe_send_update(CPid, CRef, 'job_cancelled'),

    _ = [ maybe_send_update(P, Ref, 'job_cancelled') || {_, P, Ref} <- queue:to_list(Jobs)],
    lager:debug("returning to 'ready'"),
    {'reply'
     ,{'ok', 'jobs_cancelled'}
     ,'ready'
     ,State#state{conn='undefined'
                  ,admin_conn='undefined'
                  ,current_node='undefined'
                  ,current_db='undefined'
                  ,nodes=[]
                  ,dbs=[]
                  ,wait_ref='undefined'
                  ,current_job_pid='undefined'
                  ,current_job_ref='undefined'
                  ,current_job_start='undefined'
                  ,queued_jobs=queue:new()
                 }
     ,'hibernate'
    };

wait(Msg, {NewP, _}, #state{queued_jobs=Jobs}=State) ->
    lager:debug("recv msg, assuming new job: ~p", [Msg]),
    {Ref, Jobs1} = queue_job(Msg, NewP, Jobs),
    {'reply'
     ,{'queued', Ref}
     ,'wait'
     ,State#state{queued_jobs=Jobs1}
     ,'hibernate'
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {'next_state', NextStateName, NextState} |
%%                   {'next_state', NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    lager:debug("unhandled evt for ~s: ~p", [StateName, _Event]),
    {'next_state', StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {'next_state', NextStateName, NextState} |
%%                   {'next_state', NextStateName, NextState, Timeout} |
%%                   {'reply', Reply, NextStateName, NextState} |
%%                   {'reply', Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event('nodes_left', _, StateName, #state{nodes=Ns}=State) ->
    {'reply', Ns, StateName, State};
handle_sync_event('dbs_left', _, StateName, #state{dbs=DBs}=State) ->
    {'reply', DBs, StateName, State};
handle_sync_event('current', _, StateName, #state{current_node=CN
                                                  ,current_db=CDB
                                                 }=State) ->
    {'reply', {CN, CDB}, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    lager:debug("unhandled evt for ~s: ~p", [StateName, _Event]),
    {'reply', {'error', 'invalid_sync_event'}, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {'next_state', NextStateName, NextState} |
%%                   {'next_state', NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info('$maybe_start_auto_compaction_job', 'ready'=CurrentState, State) ->
    maybe_start_auto_compaction_job(),
    {'next_state', CurrentState, State, 'hibernate'};
handle_info('$maybe_start_auto_compaction_job', CurrentState, State) ->
    {'next_state', CurrentState, State};
handle_info({'DOWN', Ref, 'process', P, _Reason}, _StateName, #state{shards_pid_ref={P, Ref}
                                                                     ,next_compaction_msg=Msg
                                                                     ,wait_ref=_OldWaitRef
                                                                   }=State) ->
    WaitRef = gen_fsm:start_timer(?SLEEP_BETWEEN_COMPACTION, Msg),
    lager:debug("pidref down ~p(~p) down during ~s", [P, Ref, _StateName]),
    lager:debug("old wait ref: ~p new wait ref: ~p", [_OldWaitRef, WaitRef]),
    lager:debug("next compaction msg: ~p", [Msg]),

    {'next_state'
     ,'wait'
     ,State#state{wait_ref=WaitRef
                  ,next_compaction_msg='undefined'
                  ,shards_pid_ref='undefined'
                 }
     ,'hibernate'
    };
handle_info(_Info, StateName, #state{}=State) ->
    lager:debug("unhandled msg for ~s: ~p", [StateName, _Info]),
    {'next_state', StateName, State}.

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
%% Convert 'process' state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {'ok', StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_nodes() -> ne_binaries().
get_nodes() ->
    {'ok', Nodes} = couch_mgr:admin_all_docs(<<"nodes">>),
    shuffle([wh_doc:id(Node) || Node <- Nodes]).

-spec get_nodes(ne_binary()) -> ne_binaries().
get_nodes(Database) ->
    case couch_mgr:admin_open_doc(<<"dbs">>, Database) of
        {'ok', DbDoc} ->
            shuffle(wh_json:get_keys(wh_json:get_value(<<"by_node">>, DbDoc)));
        {'error', 'not_found'} ->
            lager:debug("database '~s' not found", [Database]),
            get_admin_nodes(Database);
        {'error', _E} ->
            lager:debug("failed to get nodes for db '~s': ~p", [Database, _E]),
            []
    end.

-spec get_admin_nodes(ne_binary()) -> ne_binaries().
get_admin_nodes(Database) ->
    case couch_mgr:admin_db_exists(Database) of
        'true' ->
            lager:debug("database '~s' is an admin DB", [Database]),
            {'ok', NodesJObj} = couch_mgr:admin_all_docs(<<"nodes">>),
            shuffle([wh_doc:id(NodeJObj) || NodeJObj <- NodesJObj]);
        'false' -> []
    end.

-spec shuffle(ne_binaries()) -> ne_binaries().
shuffle(L) -> [O || {_, O} <- lists:keysort(1, [{random:uniform(), N} || N <- L])].

-spec encode_db(ne_binary()) -> ne_binary().
encode_db(Database) ->
    cow_qs:urlencode(Database).

-spec encode_design_doc(ne_binary()) -> ne_binary().
encode_design_doc(Design) ->
    binary:replace(Design, <<"_design/">>, <<>>, ['global']).

-spec node_dbs(server()) -> {'ok', ne_binaries()}.
node_dbs(AdminConn) ->
    {'ok', Dbs} = couch_util:all_docs(AdminConn, <<"dbs">>, []),
    {'ok', shuffle([<<"dbs">> | [wh_doc:id(Db) || Db <- Dbs]])}.

-spec db_shards(server(), ne_binary(), ne_binary()) -> ne_binaries().
db_shards(AdminConn, N, D) ->
    case couch_util:open_cache_doc(AdminConn, <<"dbs">>, D, []) of
        {'ok', Doc} ->
            Suffix = wh_json:get_value(<<"shard_suffix">>, Doc),
            Ranges = wh_json:get_value([<<"by_node">>, N], Doc, []),
            [cow_qs:urlencode(<<"shards/", Range/binary, "/", D/binary, (wh_util:to_binary(Suffix))/binary>>)
             || Range <- Ranges
            ];
        {'error', 'not_found'} ->
            lager:debug("didn't find db '~s' on node ~s in 'dbs'", [D, N]),
            db_admin(AdminConn, D);
        {'error', _E} ->
            lager:debug("failed to fetch shards for '~s' on ~s", [D, N]),
            []
    end.

-spec db_admin(server(), ne_binary()) -> ne_binaries().
db_admin(AdminConn, D) ->
    case couch_util:db_exists(AdminConn, D) of
        'true' ->
            lager:debug("db '~s' is an admin db", [D]),
            [D];
        'false' -> []
    end.

-spec db_design_docs(server(), ne_binary()) -> ne_binaries().
db_design_docs(Conn, D) ->
    case couch_util:all_design_docs(Conn, encode_db(D), []) of
        {'ok', Designs} -> [encode_design_doc(wh_doc:id(Design)) || Design <- Designs];
        {'error', _} -> []
    end.

-spec rebuild_design_docs(server(), ne_binary(), ne_binaries()) -> 'ok'.
-spec rebuild_design_doc(server(), ne_binary(), ne_binary()) -> 'ok'.
-spec rebuild_design_doc(server(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
rebuild_design_docs(Conn, D, DDs) ->
    _ = [rebuild_design_doc(Conn, D, DD) || DD <- DDs],
    'ok'.

rebuild_design_doc(Conn, D, DD) ->
    lager:debug("rebuilding design doc '~s' on '~s'", [DD, D]),

    %% first, get the design doc itself
    case couch_util:open_doc(Conn, D, <<"_design/", DD/binary>>, []) of
        {'ok', DesignDoc} ->
            rebuild_design_doc(Conn, D, DD, DesignDoc);
        {'error', _E} ->
            lager:debug("failed to load design doc for '~s' in db '~s': ~p", [DD, D, _E])
    end.

rebuild_design_doc(Conn, D, DD, DesignDoc) ->
    case wh_json:get_keys(<<"views">>, DesignDoc) of
        [] -> lager:debug("design doc '~s' in '~s' had no views", [DD, D]);
        Views ->
            rebuild_views(Conn, D, DD, Views)
    end.

-spec rebuild_views(server(), ne_binary(), ne_binary(), ne_binaries()) -> 'ok'.
-spec rebuild_view(server(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
rebuild_views(Conn, D, DD, Views) ->
    _ = [rebuild_view(Conn, D, DD, V) || V <- Views],
    'ok'.

rebuild_view(Conn, D, DD, View) ->
    case couch_util:get_results(Conn, D, <<DD/binary, "/", View/binary>>, [{'stale', 'update_after'}
                                                                           ,{'limit', 1}
                                                                          ])
    of
        {'error', _E} ->
            lager:debug("error while rebuilding view '~s/~s' has rebuilt on '~s'\: ~p", [DD, View, D, _E]),
            'ok' = timer:sleep(?SLEEP_BETWEEN_VIEWS);
        {'ok', _} ->
            lager:debug("view '~s/~s' has rebuilt on '~s'", [DD, View, D]),
            'ok' = timer:sleep(?SLEEP_BETWEEN_VIEWS)
    end.

-spec compact_shards(server(), server(), list(), list(), list()) -> pid_ref().
compact_shards(Conn, AdminConn, Node, Ss, DDs) ->
    PR = spawn_monitor(fun() ->
                               wh_util:put_callid(Node),
                               Ps = [spawn_monitor(?MODULE, 'compact_shard', [Conn, AdminConn, Shard, DDs])
                                     || Shard <- Ss
                                    ],
                               lager:debug("shard compaction pids: ~p", [Ps]),
                               wait_for_pids(?MAX_WAIT_FOR_COMPACTION_PIDS, Ps)
                       end),
    lager:debug("compacting ~s shards in ~p", [Node, PR]),
    PR.

-spec wait_for_pids(wh_timeout(), pid_refs()) -> 'ok'.
wait_for_pids(_, []) -> lager:debug("done waiting for compaction pids");
wait_for_pids(MaxWait, [{P,Ref}|Ps]) ->
    lager:debug("waiting ~p for compaction pid ~p(~p)", [MaxWait, P, Ref]),
    receive
        {'DOWN', Ref, 'process', P, _} ->
            lager:debug("recv down from ~p(~p)", [P, Ref]),
            wait_for_pids(MaxWait, Ps);
        'cancel_shard' ->
            lager:debug("cancelling waiting for ~p(~p)", [P, Ref]),
            wait_for_pids(MaxWait, Ps)
    after MaxWait ->
            lager:debug("timed out waiting for ~p(~p), moving on", [P, Ref]),
            wait_for_pids(MaxWait, Ps)
    end.

-spec compact_shard(server(), server(), ne_binary(), ne_binaries()) -> 'ok'.
compact_shard(Conn, AdminConn, S, DDs) ->
    wh_util:put_callid('compact_shard'),

    wait_for_compaction(AdminConn, S),

    case get_db_disk_and_data(AdminConn, S) of
        'undefined' ->
            lager:debug("beginning compacting shard"),
            start_compacting_shard(Conn, AdminConn, S, DDs);
        'not_found' -> 'ok';
        {BeforeDisk, BeforeData} ->
            lager:debug("beginning compacting shard: ~p disk/~p data", [BeforeDisk, BeforeData]),
            start_compacting_shard(Conn, AdminConn, S, DDs)
    end.

-spec start_compacting_shard(server(), server(), ne_binary(), ne_binaries()) -> 'ok'.
start_compacting_shard(Conn, AdminConn, S, DDs) ->
    case couch_util:db_compact(AdminConn, S) of
        'true' -> continue_compacting_shard(Conn, AdminConn, S, DDs);
        'false' -> lager:debug("compaction of shard failed, skipping")
    end.

-spec continue_compacting_shard(server(), server(), ne_binary(), ne_binaries()) -> 'ok'.
continue_compacting_shard(Conn, AdminConn, S, DDs) ->
    wait_for_compaction(AdminConn, S),

    %% cleans up old view indexes
    lager:debug("db view cleanup starting"),
    couch_util:db_view_cleanup(Conn, S),

    wait_for_compaction(AdminConn, S),

    %% compacts views
    lager:debug("design doc compaction starting"),
    compact_design_docs(AdminConn, S, DDs),

    case get_db_disk_and_data(AdminConn, S) of
        'undefined' -> lager:debug("finished compacting shard");
        'not_found' -> lager:debug("finished compacting shard");
        {AfterDisk, AfterData} ->
            lager:debug("finished compacting shard: ~p disk/~p data", [AfterDisk, AfterData])
    end.

-spec compact_design_docs(server(), ne_binary(), ne_binaries()) -> 'ok'.
compact_design_docs(AdminConn, S, DDs) ->
    try lists:split(?MAX_COMPACTING_VIEWS, DDs) of
        {Compact, Remaining} ->
            lager:debug("compacting chunk of views: ~p", [Compact]),
            _ = [couch_util:design_compact(AdminConn, S, DD) || DD <- Compact],
            wait_for_design_compaction(AdminConn, S, Compact),
            compact_design_docs(AdminConn, S, Remaining)
    catch
        'error':'badarg' when DDs =:= [] -> 'ok';
        'error':'badarg' ->
            lager:debug("compacting last chunk of views: ~p", [DDs]),
            _ = [couch_util:design_compact(AdminConn, S, DD) || DD <- DDs],
            wait_for_design_compaction(AdminConn, S, DDs)
    end.

-type db_info_resp() :: {'ok', wh_json:object()} |
                        couchbeam_error().
-type design_info_resp() :: {'ok', wh_json:object()} |
                            couchbeam_error().

-spec wait_for_design_compaction(server(), ne_binary(), ne_binaries()) ->
                                        'ok'.
-spec wait_for_design_compaction(server(), ne_binary(), ne_binaries(), ne_binary(), design_info_resp()) ->
                                        'ok'.
wait_for_design_compaction(_, _, []) -> 'ok';
wait_for_design_compaction(AdminConn, Shard, [DD|DDs]) ->
    wait_for_design_compaction(AdminConn, Shard, DDs, DD, couch_util:design_info(AdminConn, Shard, DD)).

wait_for_design_compaction(AdminConn, Shard, DDs, DD, {'error', {'conn_failed', {'error', 'timeout'}}}) ->
    lager:debug("connecting to BigCouch timed out, waiting then retrying"),
    'ok' = timer:sleep(?SLEEP_BETWEEN_POLL),
    wait_for_design_compaction(AdminConn, Shard, DDs, DD, couch_util:design_info(AdminConn, Shard, DD));
wait_for_design_compaction(AdminConn, Shard, DDs, _DD, {'error', 'not_found'}) ->
    wait_for_design_compaction(AdminConn, Shard, DDs);
wait_for_design_compaction(AdminConn, Shard, DDs, _DD, {'error', _E}) ->
    lager:debug("failed design status for '~s/~s': ~p", [Shard, _DD, _E]),
    'ok' = timer:sleep(?SLEEP_BETWEEN_POLL),
    wait_for_design_compaction(AdminConn, Shard, DDs);
wait_for_design_compaction(AdminConn, Shard, DDs, DD, {'ok', DesignInfo}) ->
    case wh_json:is_true(<<"compact_running">>, DesignInfo, 'false') of
        'false' ->
            wait_for_design_compaction(AdminConn, Shard, DDs);
        'true' ->
            'ok' = timer:sleep(?SLEEP_BETWEEN_POLL),
            wait_for_design_compaction(AdminConn, Shard, DDs, DD, couch_util:design_info(AdminConn, Shard, DD))
    end.

-spec wait_for_compaction(server(), ne_binary()) -> 'ok'.
-spec wait_for_compaction(server(), ne_binary(), db_info_resp()) -> 'ok'.
wait_for_compaction(AdminConn, S) ->
    wait_for_compaction(AdminConn, S, couch_util:db_info(AdminConn, S)).

wait_for_compaction(_AdminConn, _S, {'error', 'db_not_found'}) ->
    lager:debug("shard '~s' wasn't found", [_S]);
wait_for_compaction(AdminConn, S, {'error', 'timeout'}) ->
    lager:warning("timed out querying db status; that seems irregular!"),
    'ok' = timer:sleep(?SLEEP_BETWEEN_POLL * 2),
    wait_for_compaction(AdminConn, S);
wait_for_compaction(AdminConn, S, {'error', _E}) ->
    lager:debug("failed to query db status: ~p", [couch_util:format_error(_E)]),
    'ok' = timer:sleep(?SLEEP_BETWEEN_POLL),
    wait_for_compaction(AdminConn, S);
wait_for_compaction(AdminConn, S, {'ok', ShardData}) ->
    case wh_json:is_true(<<"compact_running">>, ShardData, 'false') of
        'false' -> lager:debug("compaction has ended");
        'true' ->
            'ok' = timer:sleep(?SLEEP_BETWEEN_POLL),
            wait_for_compaction(AdminConn, S)
    end.

-spec get_node_connections({ne_binary(), list()} | ne_binary(), atom()) ->
                                  {server(), server()} |
                                  {'error', 'no_connection'}.
-spec get_node_connections(text(), pos_integer(), string(), string(), pos_integer()) ->
                                  {server(), server()} |
                                  {'error', 'no_connection'}.
-spec get_node_connections(text(), pos_integer(), string(), string(), pos_integer(), non_neg_integer()) ->
                                  {server(), server()} |
                                  {'error', 'no_connection'}.

get_node_connections({N, Opts}, ConfigCookie) ->
    lager:debug("getting connections from opts: ~p", [Opts]),
    [_, Host] = binary:split(N, <<"@">>),

    {ConfigUser, ConfigPass} = wh_couch_connections:get_creds(),
    {User, Pass} = {props:get_value('username', Opts, ConfigUser)
                    ,props:get_value('password', Opts, ConfigPass)
                   },

    Cookie = props:get_value('cookie', Opts, ConfigCookie),
    {ConfigPort, ConfigAdminPort} = get_ports(wh_util:to_atom(N, 'true'), Cookie),
    {Port, AdminPort} = {props:get_value('port', Opts, ConfigPort)
                         ,props:get_value('admin_port', Opts, ConfigAdminPort)
                        },
    get_node_connections(Host, Port, User, Pass, AdminPort);
get_node_connections(N, Cookie) ->
    lager:debug("getting connections from known connection"),
    [_, Host] = binary:split(N, <<"@">>),

    {User,Pass} = wh_couch_connections:get_creds(),
    {Port, AdminPort} = get_ports(wh_util:to_atom(N, 'true'), Cookie),

    get_node_connections(Host, Port, User, Pass, AdminPort).

get_node_connections(Host, Port, User, Pass, AdminPort) ->
    get_node_connections(Host, Port, User, Pass, AdminPort, 0).

get_node_connections(_Host, _Port, _User, _Pass, _AdminPort, Retries) when Retries > 2 ->
    lager:warning("failed to get connections for ~s on ~p and ~p", [_Host, _Port, _AdminPort]),
    {'error', 'no_connection'};
get_node_connections(Host, Port, User, Pass, AdminPort, Retries) ->
    lager:info("getting connection information for ~s, ~p and ~p", [Host, Port, AdminPort]),
    try {couch_util:get_new_connection(Host, Port, User, Pass),
         couch_util:get_new_connection(Host, AdminPort, User, Pass)
        }
    of
        {{'error', 'timeout'}, _} ->
            lager:debug("timed out getting connection for ~s, try again", [Host]),
            get_node_connections(Host, Port, User, Pass, AdminPort, Retries+1);
        {_, {'error', 'timeout'}} ->
            lager:debug("timed out getting connection for ~s, try again", [Host]),
            get_node_connections(Host, Port, User, Pass, AdminPort, Retries+1);
        {{'error', _E}, _} ->
            lager:debug("error getting conn: ~p", [_E]),
            get_node_connections(Host, Port, User, Pass, AdminPort, Retries+1);
        {_, {'error', _E}} ->
            lager:debug("error getting admin conn: ~p", [_E]),
            get_node_connections(Host, Port, User, Pass, AdminPort, Retries+1);
        {_Conn, _AdminConn}=Conns -> Conns
    catch
        'error':{'case_clause',{'error',{'conn_failed',{'error','econnrefused'}}}} ->
            lager:warning("connection refused when connecting to ~s (on either ~p or ~p)"
                          ,[Host, Port, AdminPort]
                         ),
            BCCookie = whapps_config:get(?CONFIG_CAT, <<"bigcouch_cookie">>),
            lager:warning("check that those ports are correct and the bigcouch_cookie(~s) is correct"
                          ,[BCCookie]
                         ),
            lager:warning("'sup whapps_config set_default whistle_couch bigcouch_cookie <cookie>' if needed"),
            get_node_connections(Host, Port, User, Pass, AdminPort, Retries+1);
        _E:_R ->
            lager:warning("failed to connect to ~s: ~s: ~p", [Host, _E, _R]),
            get_node_connections(Host, Port, User, Pass, AdminPort, Retries+1)
    end.

-spec get_ports(atom(), atom()) -> {pos_integer(), pos_integer()}.
get_ports(Node, Cookie) ->
    erlang:set_cookie(Node, Cookie),
    case net_adm:ping(Node) =:= 'pong' andalso get_ports(Node) of
        'false' ->
            lager:warning("failed to ping '~s' using cookie '~s'", [Node, Cookie]),
            BCCookie = whapps_config:get(?CONFIG_CAT, <<"bigcouch_cookie">>),
            lager:warning("check that the configured bigcouch_cookie(~s) is correct"
                          ,[BCCookie]
                         ),
            lager:warning("'sup whapps_config set_default whistle_couch bigcouch_cookie <cookie>' if needed"),
            {wh_couch_connections:get_port(), wh_couch_connections:get_admin_port()};
        Ports -> Ports
    end.

-spec get_ports(atom()) -> {pos_integer(), pos_integer()}.
get_ports(Node) ->
    try {get_port(Node, ["chttpd", "port"], fun wh_couch_connections:get_port/0)
         ,get_port(Node, ["httpd", "port"], fun wh_couch_connections:get_admin_port/0)
        }
    of
        Ports ->
            lager:debug("fetched ports ~p from node ~s", [Ports, Node]),
            Ports
    catch
        _E:_R ->
            lager:debug("failed to get ports: ~s: ~p", [_E, _R]),
            lager:debug("using kazoo couch ports instead"),
            {wh_couch_connections:get_port(), wh_couch_connections:get_admin_port()}
    end.

-spec get_port(atom(), [string()], fun(() -> pos_integer())) -> pos_integer().
get_port(Node, Key, DefaultFun) ->
    case rpc:call(Node, 'couch_config', 'get', Key) of
        {'badrpc', _} -> DefaultFun();
        P -> wh_util:to_integer(P)
    end.

-type client_update() :: 'job_starting' |
                         'job_finished' |
                         'job_cancelled'.
-spec maybe_send_update(pid(), reference(), client_update()) -> 'ok'.
maybe_send_update(P, Ref, Update) when is_pid(P) ->
    case erlang:is_process_alive(P) of
        'true' -> P ! {Update, Ref}, 'ok';
        'false' -> 'ok'
    end;
maybe_send_update(_,_,_) -> 'ok'.

-spec maybe_start_auto_compaction_job() -> 'ok'.
-spec maybe_start_auto_compaction_job(boolean()) -> 'ok'.
maybe_start_auto_compaction_job() ->
    maybe_start_auto_compaction_job(compact_automatically()).
maybe_start_auto_compaction_job('false') ->
    start_auto_compaction_check_timer(),
    'ok';
maybe_start_auto_compaction_job('true') ->
    try wh_couch_connections:test_admin_conn() of
        {'ok', _} ->
            lager:debug("sending compact after timeout"),
            gen_fsm:send_event_after(?AUTOCOMPACTION_CHECK_TIMEOUT, 'compact');
        {'error', _E} ->
            start_auto_compaction_check_timer(),
            lager:debug("failed to test admin conn: ~p", [_E])
    catch
        _E:_R ->

            lager:debug("~s when testing admin conn: ~p", [_E, _R])
    end.

-spec start_auto_compaction_check_timer() -> reference().
start_auto_compaction_check_timer() ->
    erlang:send_after(?AUTOCOMPACTION_CHECK_TIMEOUT, self(), '$maybe_start_auto_compaction_job').

-spec queued_jobs_status(queue:queue()) -> 'none' | [wh_proplist(),...].
queued_jobs_status(Jobs) ->
    case queue:to_list(Jobs) of
        [] -> 'none';
        Js -> [[{'job', J}, {'requested_by', P}] || {J, P, _} <- Js]
    end.

-spec compact_automatically() -> boolean().
compact_automatically() ->
    Default = case wh_cache:fetch_local(?WH_COUCH_CACHE, <<"compact_automatically">>) of
                  {'ok', V} -> wh_util:is_true(V);
                  {'error', 'not_found'} -> 'false'
              end,

    try wh_util:is_true(whapps_config:get(?CONFIG_CAT, <<"compact_automatically">>, Default)) of
        WhappsConfig -> WhappsConfig
    catch _:_ -> Default
    end.

-spec compact_automatically(boolean()) -> 'ok'.
compact_automatically(Boolean) ->
    _ = (catch whapps_config:set(?CONFIG_CAT, <<"compact_automatically">>, Boolean)),
    CacheProps = [{'expires', 'infinity'}
                  ,{'origin', {'db', ?WH_CONFIG_DB, ?CONFIG_CAT}}
                 ],
    wh_cache:store_local(?WH_COUCH_CACHE, <<"compact_automatically">>, Boolean, CacheProps).

-spec should_compact(server(), ne_binary(), ?HEUR_NONE | ?HEUR_RATIO) -> boolean().
should_compact(_Conn, _Encoded, ?HEUR_NONE) ->
    lager:debug("no heur, true"),
    'true';
should_compact(Conn, Encoded, ?HEUR_RATIO) ->
    case get_db_disk_and_data(Conn, Encoded) of
        {Disk, Data} -> should_compact_ratio(Disk, Data);
        'undefined' -> 'false';
        'not_found' -> 'false'
    end.

-spec get_db_disk_and_data(server(), ne_binary()) ->
                                  {pos_integer(), pos_integer()} |
                                  'undefined' | 'not_found'.
get_db_disk_and_data(Conn, Encoded) ->
    get_db_disk_and_data(Conn, Encoded, 1).
get_db_disk_and_data(_Conn, _Encoded, N) when N >= 3 ->
    lager:warning("getting db info for ~s failed ~b times", [_Encoded, N]),
    'undefined';
get_db_disk_and_data(Conn, Encoded, N) ->
    lager:debug("getting db info attempt ~p", [N]),
    case couch_util:db_info(Conn, Encoded) of
        {'ok', Info} ->
            {wh_json:get_integer_value(<<"disk_size">>, Info)
             ,wh_json:get_integer_value([<<"other">>, <<"data_size">>], Info)
            };
        {'error', {'conn_failed',{'error','timeout'}}} ->
            lager:debug("timed out asking for info, waiting and trying again"),
            'ok' = timer:sleep(?MILLISECONDS_IN_SECOND),
            get_db_disk_and_data(Conn, Encoded, N+1);
        {'error', 'not_found'} ->
            lager:debug("db '~s' not found, skipping", [Encoded]),
            'not_found';
        {'error', 'db_not_found'} ->
            lager:debug("shard '~s' not found, skipping", [Encoded]),
            'not_found';
        {'error', _E} ->
            lager:debug("failed to lookup info: ~p", [_E]),
            'undefined'
    end.

-spec should_compact_ratio(integer(), integer()) -> boolean().
should_compact_ratio(Disk, Data) ->
    min_data_met(Data, ?MIN_DATA) andalso min_ratio_met(Disk, Data, ?MIN_RATIO).

-spec min_data_met(integer(), integer()) -> boolean().
min_data_met(Data, Min) when Data > Min ->
    lager:debug("data size ~b is larger than minimum ~b", [Data, Min]),
    'true';
min_data_met(_Data, _Min) ->
    lager:debug("data size ~b is under min_data_size threshold ~b", [_Data, _Min]),
    'false'.

-spec min_ratio_met(integer(), integer(), float()) -> boolean().
min_ratio_met(Disk, Data, MinRatio) ->
    case Disk / Data of
        R when R > MinRatio ->
            lager:debug("ratio ~p is greater than min ratio: ~p", [R, MinRatio]),
            'true';
        _R ->
            lager:debug("ratio ~p (~p/~p) is under min threshold ~p", [_R, Disk, Data, MinRatio]),
            'false'
    end.
