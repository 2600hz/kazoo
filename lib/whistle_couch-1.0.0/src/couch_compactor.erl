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
-export([start_link/0, get_ratios/0, force_compaction/2, compact_db/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

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

-spec(get_ratios/0 :: () -> list(#db_data{}) | []).
get_ratios() ->
    gen_server:call(?SERVER, get_ratios, infinity).

-spec(force_compaction/2 :: (MDS :: integer(), CT :: integer()) -> ok).
force_compaction(MDS, CT) ->
    gen_server:cast(?SERVER, {force_compaction, MDS, CT}).

-spec compact_db/1 :: (DBName) -> ok when
      DBName :: binary().
compact_db(DBName) ->
    gen_server:cast(?SERVER, {compact_db, DBName}).

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
    {ok, ok, ?TIMEOUT}.

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
handle_call(get_ratios, From, State) ->
    %% ?LOG_SYS("Retrieving ratios"),
    %% spawn(fun() -> gen_server:reply(From, get_dbs_and_designs()) end),
    {noreply, State}.

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
handle_cast({force_compaction, MDS, CT}, State) ->
    spawn(fun() -> compact_nodes(MDS, CT) end),
    {noreply, State};
handle_cast({compact_db, DBName}, State) ->
    %% spawn(fun() -> compact_a_db(DBName) end),
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
handle_info(timeout, ok) ->
    ?LOG_SYS("Checking if compaction is needed"),
    spawn(fun() -> compact_nodes(?MIN_DISK_SIZE, ?COMPACT_THRESHOLD) end),
    {noreply, ok, ?TIMEOUT};

handle_info(_Info, State) ->
    ?LOG_SYS("Unhandled message: ~p~n", [_Info]),
    {noreply, State, ?TIMEOUT}.

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
-spec compact_nodes/2 :: (MDS, CT) -> ok when
      MDS :: pos_integer(),
      CT :: pos_integer().
compact_nodes(MDS, CT) ->
    %% rpc:call(couch@hostname, mochiweb_socket_server, get, [couch_httpd, port])
    case couch_mgr:admin_all_docs(<<"nodes">>) of
	{ok, []} ->
	    ?LOG_SYS("No Nodes to compact");
	{ok, Nodes} ->
	    NodesData = [ get_node_data(wh_json:get_value(<<"id">>, Node)) || Node <- Nodes ],
	    [ compact(D, MDS, CT) || NodeData <- NodesData, D <- NodeData ];
	{error, _E} ->
	    ?LOG_SYS("Failed to lookup nodes: ~p", [_E])
    end.

-spec get_node_data/1 :: (Node) -> [#db_data{} | #design_data{},...] | [] when
      Node :: binary().
get_node_data(Node) ->
    [_Name, H] = binary:split(Node, <<"@">>),
    Host = whistle_util:to_list(H),
    ?LOG_SYS("Trying to contact host ~s (node ~s)", [Host, _Name]),

    {User,Pass} = couch_mgr:get_creds(),
    Port = couch_mgr:get_port(),
    AdminPort = couch_mgr:get_admin_port(),

    {Conn, AdminConn} = get_conns(Host, Port, User, Pass, AdminPort),
    get_dbs_and_designs(Conn, AdminConn).

-spec get_conns/5 :: (Host, Port, User, Pass, AdminPort) -> {#server{}, #server{}} when
      Host :: string(),
      Port :: integer(),
      User :: string(),
      Pass :: string(),
      AdminPort :: integer().
get_conns(Host, Port, User, Pass, AdminPort) ->
    {couch_util:get_new_connection(Host, Port, User, Pass),
     couch_util:get_new_connection(Host, AdminPort, User, Pass)}.

-spec get_dbs_and_designs/2 :: (Conn, AdminConn) -> [#db_data{} | #design_data{},...] | [] when
      Conn :: #server{},
      AdminConn :: #server{}.
get_dbs_and_designs(Conn, AdminConn) ->
    case get_dbs(Conn, AdminConn) of
	[] -> [];
	DBs -> get_design_docs(Conn, AdminConn, DBs)
    end.

-spec get_dbs/2 :: (Conn, AdminConn) -> [#db_data{},...] | [] when
      Conn :: #server{},
      AdminConn :: #server{}.
get_dbs(Conn, AdminConn) ->
    {ok, ShardDBs} = couch_util:db_info(AdminConn),
    ?LOG_SYS("Shards to check: ~b", [length(ShardDBs)]),
    [ create_db_data(Conn, AdminConn, binary:replace(DB, <<"/">>, <<"%2f">>, [global])) || DB <- ShardDBs ].

%% Sharded DB names from admin interface
-spec create_db_data/3 :: (Conn, AdminConn, DBName) -> #db_data{} when
      Conn :: #server{},
      AdminConn :: #server{},
      DBName :: binary().
create_db_data(Conn, AdminConn, DBName) ->
    try
	?LOG_SYS("Create db data for ~s" , [DBName]),
	{ok, DBData} = get_db_data(AdminConn, DBName),
	?LOG_SYS("Data for ~s: Dataset: ~b Disksize: ~b", [DBName
							   ,wh_json:get_value([<<"other">>, <<"data_size">>], DBData, -1)
							   ,wh_json:get_value(<<"disk_size">>, DBData, -1)
							  ]),
	#db_data{db_name=DBName
		 ,disk_size=wh_json:get_value(<<"disk_size">>, DBData, -1)
		 ,data_size=wh_json:get_value([<<"other">>, <<"data_size">>], DBData, -1)
		 ,conn = Conn
		 ,admin_conn = AdminConn
		}
    catch
	_:_ -> {db_error, DBName}
    end.

-spec get_design_docs/3 :: (Conn, AdminConn, DBData) -> [#db_data{} | #design_data{},...] when
      Conn :: #server{},
      AdminConn :: #server{},
      DBData :: [#db_data{},...].
get_design_docs(Conn, AdminConn, DBData) ->
    {ok, DBs} = couch_util:db_info(Conn),
    ?LOG_SYS("DBs to get views from: ~b", [length(DBs)]),

    DBandDocs = lists:flatten([ get_db_design_docs(Conn, DB) || DB <- DBs ]),

    lists:foldr(fun({DBName, DesignID}, Acc) ->
			case get_design_data(Conn, DBName, DesignID) of
			    {error, failed} -> Acc;
			    {ok, DDocData} ->
				DataSize = wh_json:get_value([<<"view_index">>, <<"data_size">>], DDocData, -1),
				DiskSize = wh_json:get_value([<<"view_index">>, <<"disk_size">>], DDocData, -1),

				?LOG_SYS("design info for ~s:~s: Dataset: ~b Disksize: ~b", [DesignID, DBName, DataSize, DiskSize]),
				Shards = find_shards(DBName, DBData),

				[ #design_data{db_name=DBName, design_name=DesignID, shards=Shards
					       ,disk_size=DiskSize, data_size=DataSize
					       ,conn=Conn, admin_conn=AdminConn
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
get_design_data(_C, _DB, _Design, Cnt) when Cnt > 10 ->
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
get_db_data(_AC, _DB, Cnt) when Cnt > 10 ->
    ?LOG_SYS("Failed to find data for db ~s", [_DB]),
    {error, failed};
get_db_data(AC, DB, Cnt) ->
    case couch_util:db_info(AC, DB) of
	{ok, _}=Resp -> ?LOG_SYS("Found db data for ~s in ~b tries", [DB, Cnt]), Resp;
	{error, _E} -> get_db_data(AC, DB, Cnt+1)
    end.	    

-spec get_ddocs/2 :: (Conn, DB) -> {ok, [binary(),...] | []} | {error, failed} when
      Conn :: #server{},
      DB :: binary().
get_ddocs(Conn, DB) ->
    get_ddocs(Conn, DB, 0).

-spec get_ddocs/3 :: (Conn, DB, Cnt) -> {ok, [binary(),...] | []} | {error, failed} when
      Conn :: #server{},
      DB :: binary(),
      Cnt :: non_neg_integer().
get_ddocs(_C, _DB, Cnt) when Cnt > 10 ->
    ?LOG_SYS("Failed to get design docs for ~s", [_DB]),
    {error, failed};
get_ddocs(Conn, DB, Cnt) ->
    case couch_util:all_design_docs(Conn, DB) of
	{ok, _}=Resp -> ?LOG_SYS("Found ddocs for ~s in ~p tries", [DB, Cnt]), Resp;
	{error, _E} -> get_ddocs(Conn, DB, Cnt+1)
    end.

-spec compact/3 :: (Data, MDS, CT) -> ok when
      Data :: #db_data{} | #design_data{},
      MDS :: pos_integer(),
      CT :: pos_integer().
compact(#db_data{db_name=DBName, data_size=DataSize, disk_size=DiskSize, admin_conn=AC}, MDS, CT)
  when DiskSize > MDS andalso (DiskSize div DataSize) > CT ->
    timer:sleep(random:uniform(10)*1000), %sleep between 1 and 10 seconds
    ?LOG_SYS("Compact DB ~s: ~p and VC: ~p", [DBName, couch_util:db_compact(DBName, AC), couch_util:db_view_cleanup(DBName, AC)]);
compact(#design_data{shards=Shards, design_name=Design, data_size=DataSize, disk_size=DiskSize, admin_conn=AC}, MDS, CT)
  when DiskSize > MDS andalso (DiskSize div DataSize) > CT ->
    timer:sleep(random:uniform(10)*1000), %sleep between 1 and 10 seconds
    [ ?LOG_SYS("Compact design ~s for ~s: ~p", [Design, DBName, couch_util:design_compact(DBName, Design, AC)]) || DBName <- Shards ];
compact(_, _, _) -> ok.

find_shards(DBName, DBs) ->
    find_shards(DBName, DBs, []).

find_shards(_, [], Acc) -> Acc;
find_shards(DBName, [#db_data{db_name=Shard}|DBs], Acc) ->
    case binary:match(Shard, DBName) of
	nomatch -> find_shards(DBName, DBs, Acc);
	_ -> find_shards(DBName, DBs, [Shard | Acc])
    end.

%% compact_a_db(DBName) ->
%%     ?LOG_SYS("Compacting ~s", [DBName]),
%%     {ok, ShardDBs} = couch_mgr:admin_db_info(),
%%     ?LOG_SYS("Found shards: ~b", [length(ShardDBs)]),
%%     DBShards = [create_db_data(binary:replace(Shard, <<"/">>, <<"%2f">>, [global]))
%% 		|| Shard <- ShardDBs, binary:match(Shard, DBName) =/= nomatch],
%%     case DBShards of
%% 	[] ->
%% 	    ?LOG_SYS("Sad, no shards matched ~s", [DBName]),
%% 	    sad_face;
%% 	L ->
%% 	    ?LOG_SYS("Found ~b shards related", [length(L)]),
%% 	    DBandDDocs = get_db_design_docs(DBName),
%% 	    ?LOG_SYS("DB and design docs: ~b", [length(DBandDDocs)]),

%% 	    Compactable = lists:foldr(fun({Name, DesignID}, L0) ->
%% 					      {ok, DDocData} = get_design_data(Name, DesignID),

%% 					      DataSize = wh_json:get_value([<<"view_index">>, <<"data_size">>], DDocData, -1),
%% 					      DiskSize = wh_json:get_value([<<"view_index">>, <<"disk_size">>], DDocData, -1),

%% 					      ?LOG_SYS("design info for ~s:~s: Dataset: ~b Disksize: ~b", [DesignID, Name, DataSize, DiskSize]),

%% 					      [ #design_data{db_name=Name, design_name=DesignID, shards=find_shards(Name, L)
%% 							     ,disk_size=DiskSize, data_size=DataSize}
%% 						| L0]
%% 				      end, L, DBandDDocs),
%% 	    [compact(D, 1, 1) || D <- Compactable]
%%     end.
