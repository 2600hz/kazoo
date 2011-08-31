%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Monitor DBs and their disk usage, compacting when necessary
%%% @end
%%% Created : 18 May 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(couch_compactor).

-behaviour(gen_server).

-include("wh_couch.hrl").

%% API
-export([start_link/0, force_compaction/0, force_compaction/2, compact_db/1]).

-export([add_threshold/2, get_thresholds/0, rm_threshold/1, get_db_report/0]).

-export([close_files/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_THRESHOLDS, orddict:from_list([{0,1}])). %% catch-all to force compaction
-define(LARGEST_MDS, round(math:pow(2,30))).
-define(SLEEP_BETWEEN_COMPACTIONS, 1000 * 60 * 10). %% ten minutes between each compaction event

-type thresholds() :: orddict:orddict().
%% State :: orddict()
%% Key :: Minimum Disk Size :: integer()
%% Value :: Ratio of DiskSize to DataSize :: integer()
%% When deciding whether to compact a database (or view), find its disk size
%% then iterate through the State for the smallest key that is > disk size.
%% So, DS = 500,000 and keys = [1, 1,000, 100,000, 1,000,000, 10,000,000], use key 1,000,000.
%% With this key, use the value (the ratio) to determine if compaction is
%% needed. So, one strategy could be, as disk size increases, ratio decreases,
%% resulting in more aggressive compaction the bigger a DB grows.

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec force_compaction/2 :: (MDS, CT) -> ok when
      MDS :: non_neg_integer(),
      CT :: non_neg_integer().
force_compaction(MDS, CT) ->
    gen_server:cast(?SERVER, {force_compaction, MDS, CT}).

-spec force_compaction/0 :: () -> ok.
force_compaction() ->
    gen_server:cast(?SERVER, {force_compaction, 0, 1}).

-spec compact_db/1 :: (DBName) -> ok when
      DBName :: binary().
compact_db(DBName) ->
    gen_server:cast(?SERVER, {compact_db, DBName}).

%% Inserts or replaces the Ratio MDS
-spec add_threshold/2 :: (MDS, Ratio) -> ok when
      MDS :: non_neg_integer(),
      Ratio :: non_neg_integer().
add_threshold(MDS, Ratio) when is_integer(MDS) andalso is_integer(Ratio) ->
    gen_server:cast(?SERVER, {add_threshold, MDS, Ratio}).

-spec get_thresholds/0 :: () -> list(tuple(non_neg_integer(), non_neg_integer())) | [].
get_thresholds() ->
    gen_server:call(?SERVER, get_thresholds).

-spec rm_threshold/1 :: (MDS) -> ok when
      MDS :: non_neg_integer().
rm_threshold(MDS) when is_integer(MDS) ->
    gen_server:cast(?SERVER, {rm_threshold, MDS}).

-spec get_db_report/0 :: () -> [proplist(),...] | [].
get_db_report() ->
    gen_server:call(?SERVER, get_db_report, 10*60*1000). %% takes a while to get the report

%% When nodes have file descriptors opened but the file is deleted, disk reporting tools
%% like df don't accurately report available disk space.
-spec close_files/0 :: () -> ok.
close_files() ->
    gen_server:cast(?SERVER, close_files).

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
init([]) ->
    ?LOG_SYS("Started compactor"),
    process_flag(trap_exit, true),
    {ok, ?DEFAULT_THRESHOLDS}.

handle_call(get_thresholds, _From, Thresholds) ->
    {reply, orddict:to_list(Thresholds), Thresholds};
handle_call(get_db_report, From, Thresholds) ->
    spawn_link(fun() -> get_db_report(From, Thresholds) end),
    {noreply, Thresholds}.

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
handle_cast({add_threshold, MDS, Ratio}, Thresholds) ->
    {noreply, orddict:store(MDS, Ratio, Thresholds)};
handle_cast({rm_threshold, MDS}, Thresholds) ->
    {noreply, orddict:erase(MDS, Thresholds)};
handle_cast({force_compaction, MDS, CT}, Thresholds) ->
    spawn_link(fun() -> compact_nodes(orddict:from_list([{MDS, CT}])) end),
    {noreply, Thresholds};
handle_cast({compact_db, DBName}, Thresholds) ->
    spawn_link((fun compact_a_db/1)(DBName)),
    {noreply, Thresholds};
handle_cast(close_files, Thresholds) ->
    spawn_link(fun close_files_on_all_nodes/0),
    {noreply, Thresholds}.

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
handle_info({'EXIT', _Pid, _Reason}, Thresholds) ->
    ?LOG_SYS("~p exited with ~p", [_Pid, _Reason]),
    {noreply, Thresholds};
handle_info(_Info, Thresholds) ->
    ?LOG_SYS("Unhandled message: ~p~n", [_Info]),
    {noreply, Thresholds, ?TIMEOUT}.

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
-spec compact_nodes/1 :: (Thresholds) -> ok when
      Thresholds :: thresholds().
compact_nodes(Thresholds) ->
    case couch_mgr:admin_all_docs(<<"nodes">>) of
	{ok, []} ->
	    ?LOG_SYS("No Nodes to compact");
	{ok, Nodes} ->
	    [ begin
		  try
		      Data = get_node_data(wh_json:get_value(<<"id">>, Node), Thresholds),
		      [compact(D) || D <- Data]
		  catch
		      E:R ->
			  ST = erlang:get_stacktrace(),
			  ?LOG_SYS("Error getting node data for ~s: ~p:~p", [wh_json:get_value(<<"id">>, Node), E, R]),
			  _ = [?LOG("stacktrace: ~p", [ST1]) || ST1 <- ST],
			  []
		  end
	      end
	      || Node <- Nodes ];
	{error, _E} ->
	    ?LOG_SYS("Failed to lookup nodes: ~p", [_E])
    end.

-spec close_files_on_all_nodes/0 :: () -> no_return().
close_files_on_all_nodes() ->
    case couch_mgr:admin_all_docs(<<"nodes">>) of
	{ok, []} ->
	    ?LOG_SYS("No nodes to release files on");
	{ok, Nodes} ->
	    ?LOG_SYS("Closing files on nodes"),
	    [ close_files_on_node(wh_json:get_value(<<"id">>, Node)) || Node <- Nodes]
    end.

-spec close_files_on_node/1 :: (Node) -> no_return() when
      Node :: binary().
close_files_on_node(NodeBin) ->
    put(callid, NodeBin),
    [_Name, H] = binary:split(NodeBin, <<"@">>),
    Host = wh_util:to_list(H),
    Node = wh_util:to_atom(NodeBin, true),
    ?LOG_SYS("Trying to contact host ~s (node ~s)", [Host, _Name]),

    Cookie = couch_mgr:get_node_cookie(),
    ?LOG_SYS("Using cookie ~s on node ~s", [Cookie, Node]),
    try
	erlang:set_cookie(Node, Cookie),
	pong = net_adm:ping(Node),
	?LOG_SYS("Ponged with ~s", [Node]),
	[ close_pid(Node, Pid) || Pid <- rpc:call(Node, erlang, processes, []) ]
    catch
        _E:_R -> ?LOG_SYS("Failed to close files: ~p:~p", [_E, _R])
    end.

-spec close_pid/2 :: (Node, Pid) -> no_return() when
      Node :: atom(),
      Pid :: pid().
close_pid(Node, Pid) ->
    ?LOG_SYS("Looking up ~p on ~s", [Pid, Node]),
    case rpc:call(Node, erlang, process_info, [Pid, dictionary]) of
	{dictionary, Data} ->
	    ?LOG_SYS("Got dictionary data for ~p", [Pid]),
	    case props:get_value('$initial_call', Data) of
		{couch_file,init,1} -> ?LOG_SYS("Closing ~p", [Pid]), catch(rpc:call(Node, couch_file, close, [Pid]));
		_ -> ok
	    end;
	_ -> ok
    end.

-spec get_db_report/2 :: (From, Thresholds) -> [proplist(),...] when
      From :: {pid(), reference()},
      Thresholds :: thresholds().
get_db_report(From, Thresholds) ->
    case couch_mgr:admin_all_docs(<<"nodes">>) of
	{ok, []} ->
	    gen_server:reply(From, no_nodes),
	    ?LOG_SYS("No Nodes to compact");
	{ok, Nodes} ->
	    NodesData = [ begin
			      NodeId = wh_json:get_value(<<"id">>, Node),
			      Data = get_node_data(NodeId, Thresholds),
			      ?LOG_SYS("Got ~b entries for node ~s", [length(Data), NodeId]),
			      try {NodeId, [ get_report_data(D) || D <- lists:usort(fun sort_report_data/2, Data) ]}
			      catch E:R -> ?LOG_SYS("Error: ~p:~p", [E, R]), ?LOG("Stacktrace: ~p", [erlang:get_stacktrace()]), {NodeId, []}
			      end
			  end
			  || Node <- Nodes ],
	    gen_server:reply(From, NodesData);
	{error, _E}=E ->
	    gen_server:reply(From, E),
	    ?LOG_SYS("Failed to lookup nodes: ~p", [_E])
    end.

-type report_data_result() :: {db_data, binary(), non_neg_integer(), non_neg_integer(), boolean()} |
			      {design_data, binary(), binary(), non_neg_integer(), non_neg_integer(), boolean()}.
-spec get_report_data/1 :: (Data) -> report_data_result() when
      Data :: #db_data{} | #design_data{}.
get_report_data(#db_data{db_name=DBName, disk_size=DiskSize, data_size=DataSize, do_compaction=DoIt}) ->
    {db_data, DBName, DiskSize, DataSize, DoIt};
get_report_data(#design_data{db_name=DBName, design_name=Design, disk_size=DiskSize, data_size=DataSize, do_compaction=DoIt}) ->
    {design_data, DBName, Design, DiskSize, DataSize, DoIt}.

-spec sort_report_data/2 :: (A, B) -> boolean() when
      A :: #db_data{} | #design_data{},
      B :: #db_data{} | #design_data{}.
sort_report_data(#db_data{}, #design_data{}) ->
    true;
sort_report_data(#design_data{}, #db_data{}) ->
    false;
sort_report_data(#design_data{disk_size=DiskA}, #design_data{disk_size=DiskB}) ->
    DiskA > DiskB;
sort_report_data(#db_data{disk_size=DiskA}, #db_data{disk_size=DiskB}) ->
    DiskA > DiskB.

-spec get_node_data/2 :: (Node, Thresholds) -> [#db_data{} | #design_data{},...] | [] when
      Node :: binary(),
      Thresholds :: thresholds().
get_node_data(NodeBin, Thresholds) ->
    put(callid, NodeBin),
    [_Name, H] = binary:split(NodeBin, <<"@">>),
    Host = wh_util:to_list(H),
    Node = wh_util:to_atom(NodeBin, true),
    ?LOG_SYS("Trying to contact host ~s (node ~s)", [Host, _Name]),

    {User,Pass} = couch_mgr:get_creds(),
    {Port,AdminPort} = get_ports(Node),

    {Conn, AdminConn} = get_conns(Host, Port, User, Pass, AdminPort),
    get_dbs_and_designs(Node, Conn, AdminConn, Thresholds).

-spec get_conns/5 :: (Host, Port, User, Pass, AdminPort) -> {#server{}, #server{}} when
      Host :: string(),
      Port :: non_neg_integer(),
      User :: string(),
      Pass :: string(),
      AdminPort :: non_neg_integer().
get_conns(Host, Port, User, Pass, AdminPort) ->
    ?LOG_SYS("get_conns: H: ~s P: ~b U: ~s P: ~s AP: ~b", [Host, Port, User, Pass, AdminPort]),
    {couch_util:get_new_connection(Host, Port, User, Pass),
     couch_util:get_new_connection(Host, AdminPort, User, Pass)}.

-spec get_ports/1 :: (Node) -> {non_neg_integer(), non_neg_integer()} when
      Node :: atom().
get_ports(Node) ->
    Cookie = couch_mgr:get_node_cookie(),
    ?LOG_SYS("Using cookie ~s on node ~s", [Cookie, Node]),
    try
	erlang:set_cookie(Node, Cookie),
	get_ports(Node, net_adm:ping(Node))
    catch
	_:_ ->
	    {?DEFAULT_PORT, ?DEFAULT_ADMIN_PORT}
    end.

get_ports(Node, pong) ->
    ?LOG_SYS("Trying to find ports from node ~s", [Node]),
    Port = case rpc:call(Node, couch_config, get, ["chttpd", "port"]) of
	       {badrpc, _} ->
		   ?LOG_SYS("Failed to get port from RPC"),
		   couch_mgr:get_port();
	       P ->
		   ?LOG_SYS("Got port ~s", [P]),
		   wh_util:to_integer(P)
	   end,
    AdminPort = case rpc:call(Node, couch_config, get, ["httpd", "port"]) of
		    {badrpc, _} ->
			?LOG_SYS("Failed to get admin port from RPC"),
			couch_mgr:get_admin_port();
		    AP ->
			?LOG_SYS("Got admin port ~s", [AP]),
			wh_util:to_integer(AP)
		end,
    {Port, AdminPort};
get_ports(_Node, pang) ->
    ?LOG_SYS("Using same ports as couch_mgr"),
    {couch_mgr:get_port(), couch_mgr:get_admin_port()}.

-spec get_dbs_and_designs/4 :: (Node, Conn, AdminConn, Thresholds) -> [#db_data{} | #design_data{},...] | [] when
      Node :: atom(),
      Conn :: #server{},
      AdminConn :: #server{},
      Thresholds :: thresholds().
get_dbs_and_designs(Node, Conn, AdminConn, Thresholds) ->
    case get_dbs(Node, Conn, AdminConn, Thresholds) of
	[] -> ?LOG_SYS("No DB Data loaded for ~s", [Node]), [];
	DBs ->
	    ?LOG_SYS("Got DB data for ~b DBs", [length(DBs)]),
	    try
		get_design_docs(Node, Conn, AdminConn, DBs, Thresholds)
	    catch
		E:R ->
		    ST = erlang:get_stacktrace(),
		    ?LOG_SYS("error getting design docs: ~p:~p", [E, R]),
		    _ = [?LOG("stacktrace: ~p", [ST1]) || ST1 <- ST],
		    DBs
	    end
    end.

-spec get_dbs/4 :: (Node, Conn, AdminConn, Thresholds) -> [#db_data{} | {db_error, binary()},...] | [] when
      Node :: atom(),
      Conn :: #server{},
      AdminConn :: #server{},
      Thresholds :: thresholds().
get_dbs(Node, Conn, AdminConn, Thresholds) ->
    {ok, ShardDBs} = couch_util:db_info(AdminConn),
    ?LOG_SYS("Shards to check: ~b", [length(ShardDBs)]),
    [ DBData ||
	DB <- ShardDBs
	    ,(DBData = create_db_data(Node, Conn, AdminConn, binary:replace(DB, <<"/">>, <<"%2f">>, [global]), Thresholds)) =/= db_error
    ].

%% Sharded DB names from admin interface
-spec create_db_data/5 :: (Node, Conn, AdminConn, DBName, Thresholds) -> #db_data{} | db_error when
      Node :: atom(),
      Conn :: #server{},
      AdminConn :: #server{},
      DBName :: binary(),
      Thresholds :: thresholds().
create_db_data(Node, Conn, AdminConn, DBName, Thresholds) ->
    try
	?LOG_SYS("Create db data for ~s" , [DBName]),
	{ok, DBData} = get_db_data(AdminConn, DBName),

	DiskSize = wh_util:to_integer(wh_json:get_value(<<"disk_size">>, DBData, -1)),
	DataSize = wh_util:to_integer(wh_json:get_value([<<"other">>, <<"data_size">>], DBData, -1)),
	CompactIsRunning = wh_util:is_true(wh_json:get_value(<<"compact_running">>, DBData, false)),

	{_MDS, Ratio} = orddict:fold(fun(K, V, Acc) -> filter_thresholds(K, V, Acc, DiskSize) end, {?LARGEST_MDS,1}, Thresholds),

	?LOG_SYS("Data for ~s: Dataset: ~b Disksize: ~b", [DBName, DataSize, DiskSize]),
	?LOG_SYS("Using MDS: ~b with ratio ~b", [_MDS, Ratio]),
	?LOG_SYS("Compact is running already: ~s", [CompactIsRunning]),

	DoCompaction = try ((DiskSize div DataSize) > Ratio) andalso (not CompactIsRunning)
		       catch _:_ -> ?LOG_SYS("Err determining whether to compact"), true
		       end,

	?LOG_SYS("Do Compaction: ~s", [DoCompaction]),

	#db_data{db_name=DBName
		 ,node=Node
		 ,disk_size=DiskSize
		 ,data_size=DataSize
		 ,conn = Conn
		 ,admin_conn = AdminConn
		 ,do_compaction = DoCompaction
		}
    catch
	_:_ -> db_error
    end.

-spec get_design_docs/5 :: (Node, Conn, AdminConn, DBData, Thresholds) -> [#db_data{} | #design_data{},...] when
      Node :: atom(),
      Conn :: #server{},
      AdminConn :: #server{},
      DBData :: [#db_data{},...],
      Thresholds :: thresholds().
get_design_docs(Node, Conn, AdminConn, DBData, Thresholds) ->
    {ok, DBs} = couch_util:db_info(Conn),
    ?LOG_SYS("DBs to get views from: ~b", [length(DBs)]),

    DBandDocs = lists:flatten([ get_db_design_docs(Conn, DB) || DB <- DBs ]),

    lists:foldr(fun({DBName, DesignID}, Acc) ->
			case get_design_data(Conn, DBName, DesignID) of
			    {error, failed} ->
				?LOG_SYS("Failed to get design data for ~s / ~s", [DBName, DesignID]), Acc;
			    {ok, DDocData} ->
				DataSize = wh_util:to_integer(wh_json:get_value([<<"view_index">>, <<"data_size">>], DDocData, -1)),
				DiskSize = wh_util:to_integer(wh_json:get_value([<<"view_index">>, <<"disk_size">>], DDocData, -1)),
				CompactIsRunning = wh_util:is_true(wh_json:get_value(<<"compact_running">>, DBData, false)),

				?LOG_SYS("design info for ~s:~s: Dataset: ~b Disksize: ~b", [DesignID, DBName, DataSize, DiskSize]),
				?LOG_SYS("Compact is running already: ~s", [CompactIsRunning]),

				{_MDS, Ratio} = orddict:fold(fun(K, V, AccT) -> filter_thresholds(K, V, AccT, DiskSize) end, {?LARGEST_MDS,1}, Thresholds),

				?LOG_SYS("Using MDS: ~b with ratio ~b", [_MDS, Ratio]),
				?LOG_SYS("Compact is running already: ~s", [CompactIsRunning]),

				DoCompaction = try ((DiskSize div DataSize) > Ratio) andalso (not CompactIsRunning)
					       catch _:_ -> ?LOG_SYS("Err determining whether to compact"), true
					       end,

				?LOG_SYS("Do Compaction: ~s", [DoCompaction]),

				Shards = find_shards(DBName, DBData),

				[ #design_data{db_name=DBName
					       ,design_name=DesignID
					       ,node=Node
					       ,shards=Shards
					       ,disk_size=DiskSize
					       ,data_size=DataSize
					       ,conn=Conn
					       ,admin_conn=AdminConn
					       ,do_compaction = DoCompaction
					      }
				  | Acc]
			end
		end, DBData, DBandDocs).

-spec get_db_design_docs/2 :: (Conn, DB) -> [{binary(),binary()},...] when
      Conn :: #server{},
      DB :: binary().
get_db_design_docs(Conn, DB) ->
    Encoded = binary:replace(DB, <<"/">>, <<"%2f">>, [global]),
    {ok, DDocs} = get_ddocs(Conn, Encoded),
    ?LOG_SYS("Got DDocs for ~s: ~b", [Encoded, length(DDocs)]),
    [{Encoded, binary:replace(wh_json:get_value(<<"id">>, DDoc), <<"_design/">>, <<>>, [global])} || DDoc <- DDocs ].

-spec get_design_data/3 :: (Conn, DB, Design) -> {ok, json_object()} | {error, failed} when
      Conn :: #server{},
      DB :: binary(),
      Design :: binary().
get_design_data(Conn, DB, Design) ->
    get_design_data(Conn, DB, Design, 0).

-spec get_design_data/4 :: (Conn, DB, Design, Cnt) -> {ok, json_object()} | {error, failed} when
      Conn :: #server{},
      DB :: binary(),
      Design :: binary(),
      Cnt :: non_neg_integer().
get_design_data(C, _DB, _Design, Cnt) when Cnt > 10 ->
    ?LOG_SYS("Failed to find design data for ~s / ~s on ~s after 10 tries", [_DB, _Design, couchbeam:server_url(C)]),
    {error, failed};
get_design_data(Conn, DB, Design, Cnt) ->
    case couch_util:design_info(Conn, DB, Design) of
	{ok, _}=Resp -> Resp;
	_ -> get_design_data(Conn, DB, Design, Cnt+1)
    end.	    

-spec get_db_data/2 :: (AdminConn, DB) -> {ok, json_object()} | {error, failed} when
      AdminConn :: #server{},
      DB :: binary().
get_db_data(AdminConn, DB) ->
    get_db_data(AdminConn, DB, 0).

-spec get_db_data/3 :: (AdminConn, DB, Cnt) -> {ok, json_object()} | {error, failed} when
      AdminConn :: #server{},
      DB :: binary(),
      Cnt :: non_neg_integer().
get_db_data(AC, _DB, Cnt) when Cnt > 10 ->
    ?LOG_SYS("Failed to find data for db ~s on ~s after 10 tries", [_DB, couchbeam:server_url(AC)]),
    {error, failed};
get_db_data(AC, DB, Cnt) ->
    case couch_util:db_info(AC, DB) of
	{ok, _}=Resp -> ?LOG_SYS("Found db data for ~s in ~b tries", [DB, Cnt]), Resp;
	_ -> get_db_data(AC, DB, Cnt+1)
    end.	    

-spec get_ddocs/2 :: (Conn, DB) -> {ok, json_objects()} | {error, failed} when
      Conn :: #server{},
      DB :: binary().
get_ddocs(Conn, DB) ->
    get_ddocs(Conn, DB, 0).

-spec get_ddocs/3 :: (Conn, DB, Cnt) -> {ok, json_objects()} | {error, failed} when
      Conn :: #server{},
      DB :: binary(),
      Cnt :: non_neg_integer().
get_ddocs(C, _DB, Cnt) when Cnt > 10 ->
    ?LOG_SYS("Failed to find design docs for db ~s on ~s after 10 tries", [_DB, couchbeam:server_url(C)]),
    {error, failed};
get_ddocs(Conn, DB, Cnt) ->
    case couch_util:all_design_docs(Conn, DB, []) of
	{ok, _}=Resp -> ?LOG_SYS("Found ddocs for ~s in ~p tries", [DB, Cnt]), Resp;
	_ -> get_ddocs(Conn, DB, Cnt+1)
    end.

-spec compact/1 :: (Data) -> ok when
      Data :: #db_data{} | #design_data{}.
compact(#db_data{db_name=DBName, node=Node, admin_conn=AC, do_compaction=true}) ->
    timer:sleep(?SLEEP_BETWEEN_COMPACTIONS),
    ?LOG_SYS("Compact DB ~s on ~s: ~p and VC: ~p", [DBName, Node, couch_util:db_compact(AC, DBName), couch_util:db_view_cleanup(AC, DBName)]);
compact(#design_data{shards=Shards, node=Node, design_name=Design, admin_conn=AC, do_compaction=true}) ->
    timer:sleep(?SLEEP_BETWEEN_COMPACTIONS),
    [ ?LOG_SYS("Compact design ~s for ~s on ~s: ~p", [Design, DBName, Node, couch_util:design_compact(AC, DBName, Design)]) || DBName <- Shards ];
compact(_) -> ok.

-spec find_shards/2 :: (DBName, DBs) -> [binary(),...] when
      DBName :: binary(),
      DBs :: [#db_data{},...].
find_shards(DBName, DBs) ->
    [ Shard || #db_data{db_name=Shard} <- DBs, binary:match(Shard, DBName) =/= nomatch].

compact_a_db(DBName) ->
    ?LOG_SYS("Compacting ~s", [DBName]),

    case couch_mgr:admin_all_docs(<<"nodes">>) of
	{ok, []} ->
	    ?LOG_SYS("No Nodes to compact ~s", [DBName]);
	{ok, Nodes} ->
	    NodesData = [ get_node_data(wh_json:get_value(<<"id">>, Node), ?DEFAULT_THRESHOLDS) || Node <- Nodes ],
	    [ spawn(fun() -> [ compact(D) || D <- NodeData, is_the_db(binary:replace(DBName, <<"/">>, <<"%2f">>, [global]), D) ] end) || NodeData <- NodesData ];
	{error, _E} ->
	    ?LOG_SYS("Failed to lookup nodes: ~p", [_E])
    end.

-spec is_the_db/2 :: (DBName, Data) -> boolean() when
      DBName :: binary(),
      Data :: #db_data{} | #design_data{}.
is_the_db(DBName, #db_data{db_name=Shard}) ->
    ?LOG_SYS("Does ~s match ~s?", [Shard, DBName]),
    binary:match(Shard, DBName) =/= nomatch;
is_the_db(DBName, #design_data{db_name=Shard}) ->
    ?LOG_SYS("Does ~s match ~s?", [Shard, DBName]),
    binary:match(Shard, DBName) =/= nomatch;
is_the_db(_,_) ->
    false.

%% K and V come from the orddict
%% The acc is the current threshold to use
%% DiskSize is the size of the DB or design
-spec filter_thresholds/4 :: (K, V, Acc, DiskSize) -> {non_neg_integer(), non_neg_integer()} when
      K :: non_neg_integer(),
      V :: non_neg_integer(),
      Acc :: {non_neg_integer(), non_neg_integer()},
      DiskSize :: non_neg_integer().
filter_thresholds(K, V, {AccK, _}, DiskSize) when K > DiskSize andalso K < AccK ->
    {K,V};
filter_thresholds(_, _, Acc, _) -> Acc.
