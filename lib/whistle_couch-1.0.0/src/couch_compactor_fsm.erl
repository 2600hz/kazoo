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

-record(state, {
          nodes :: [ne_binary(),...] | []
          ,dbs :: [ne_binary(),...] | []
          ,cookie :: atom()
          ,wait_ref :: reference()

          ,current_node :: ne_binary()
          ,current_db :: ne_binary()
          ,conn :: #server{}
          ,admin_conn :: #server{}
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

compact() ->
    gen_fsm:sync_send_event(?SERVER, compact).

compact_node(Node) ->
    gen_fsm:sync_send_event(?SERVER, {compact_node, Node}).

compact_db(Db) ->
    gen_fsm:sync_send_event(?SERVER, {compact_db, Db}).

compact_db(Node, Db) ->
    gen_fsm:sync_send_event(?SERVER, {compact_db, Node, Db}).

status() ->
    gen_fsm:sync_send_event(?SERVER, status).

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

    case couch_config:fetch(<<"compact_automatically">>, false) of
        true -> gen_fsm:send_event(self(), compact);
        false -> ok
    end,

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
ready(_Msg, State) ->
    lager:debug("unhandled msg: ~p", [_Msg]),
    {next_state, ready, State}.

ready(status, _, State) ->
    {reply, <<"ready">>, ready, State};
ready(compact=Msg, _, State) ->
    gen_fsm:send_event(self(), Msg),
    {reply, starting, ready, State};
ready({compact_node, _Node}=Msg, _, State) ->
    gen_fsm:send_event(self(), Msg),
    {reply, starting, ready, State};
ready({compact_db, _Db}=Msg, _, State) ->
    gen_fsm:send_event(self(), Msg),
    {reply, starting, ready, State};
ready({compact_db, _Node, _Db}=Msg, _, State) ->
    gen_fsm:send_event(self(), Msg),
    {reply, starting, ready, State}.

%%--------------------------------------------------------------------
compact({compact, N}, #state{conn=undefined
                             ,admin_conn=undefined
                             ,cookie=Cookie
                             ,nodes=[]
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
            {next_state, ready, State#state{conn=undefined
                                            ,admin_conn=undefined
                                            ,current_node=undefined
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
            {next_state, ready, State#state{conn=undefined
                                            ,admin_conn=undefined
                                            ,current_node=undefined
                                            ,current_db=undefined
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

compact(compact, #state{nodes=[]}=State) ->
    lager:debug("no nodes to compact"),
    {next_state, ready, State#state{conn=undefined
                                    ,admin_conn=undefined
                                    ,current_node=undefined
                                    ,current_db=undefined
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
                                  }=State) ->
    case couch_util:db_exists(Conn, D) of
        false ->
            lager:debug("db ~s not found on ~s", [D, N]),
            {next_state, ready, State#state{conn=undefined
                                            ,admin_conn=undefined
                                            ,current_node=undefined
                                            ,current_db=undefined
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

compact({compact_db, N, D, [], _}, #state{nodes=[]}=State) ->
    lager:debug("no shards to compact for ~s on ~s", [D, N]),
    {next_state, ready, State#state{conn=undefined
                                    ,admin_conn=undefined
                                    ,current_node=undefined
                                    ,current_db=undefined
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
                         }= State) ->
    {reply, {{node, N},{db, D}}, compact, State}.

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
                      }= State) ->
    {reply, {{node, N},{db, D}, {wait_left, erlang:read_timer(Ref)}}, wait, State};
wait(_Msg, _, State) ->
    lager:debug("unhandled msg: ~p", [_Msg]),
    {reply, {error, unhandled_message}, wait, State}.

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
    wait_for_compaction(AdminConn, S),
    couch_util:db_compact(AdminConn, S),
    wait_for_compaction(AdminConn, S),

    couch_util:db_view_cleanup(AdminConn, S),
    wait_for_compaction(AdminConn, S),

    compact_design_docs(AdminConn, S, DDs),
    wait_for_compaction(AdminConn, S),

    lager:debug("compaction for ~s finished", [S]).

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
