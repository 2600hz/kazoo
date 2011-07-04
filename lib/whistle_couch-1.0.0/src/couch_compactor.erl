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

-include_lib("whistle/include/wh_log.hrl").
-include("wh_couch.hrl").

%% API
-export([start_link/0, get_ratios/0, force_compaction/2, view_cleanup/0]).

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

-spec(view_cleanup/0 :: () -> ok).
view_cleanup() ->
    gen_server:cast(?SERVER, view_cleanup).

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
    ?LOG_SYS("Retrieving ratios"),
    spawn(fun() -> gen_server:reply(From, get_dbs_and_designs()) end),
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
    spawn(fun() ->
		  case get_dbs_and_designs() of
		      [] -> ok;
		      Data -> [compact(D, MDS, CT) || D <- Data]
		  end
	  end),
    {noreply, State};
handle_cast(view_cleanup, State) ->
    {ok, DBs} = couch_mgr:db_info(),
    _ = [couch_mgr:db_view_cleanup(DB) || DB <- DBs ],
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
    spawn(fun() ->
		  %% changed to blocking + random sleep to not overload bigcouch with
		  %% compaction requests
		  case get_dbs_and_designs() of
		      [] -> ok;
		      Data -> [ compact(D, ?MIN_DISK_SIZE, ?COMPACT_THRESHOLD) || D <- Data ]
		  end
	  end),
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
-spec(get_dbs_and_designs/0 :: () -> list(#db_data{} | #design_data{}) | []).
get_dbs_and_designs() ->
    case get_dbs() of
	[] -> [];
	DBs -> get_design_docs(DBs)
    end.

-spec(get_dbs/0 :: () -> list(#db_data{}) | []).
get_dbs() ->
    {ok, ShardDBs} = couch_mgr:admin_db_info(),
    ?LOG_SYS("Shards to check: ~b", [length(ShardDBs)]),
    [ create_db_data(binary:replace(DB, <<"/">>, <<"%2f">>, [global])) || DB <- ShardDBs ].

%% Sharded DB names from admin interface
-spec(create_db_data/1 :: (DBName :: binary()) -> #db_data{}).
create_db_data(DBName) ->
    try
	{ok, DBData} = get_db_data(DBName),
	?LOG_SYS("Data for ~s: Dataset: ~b Disksize: ~b", [DBName
							   ,wh_json:get_value([<<"other">>, <<"data_size">>], DBData, -1)
							   ,wh_json:get_value(<<"disk_size">>, DBData, -1)
							  ]),
	#db_data{db_name=DBName
		  ,disk_size=wh_json:get_value(<<"disk_size">>, DBData, -1)
		  ,data_size=wh_json:get_value([<<"other">>, <<"data_size">>], DBData, -1)
		 }
    catch
	_:_ -> #db_data{db_name=DBName}
    end.

get_design_docs(L) ->
    {ok, DBs} = couch_mgr:db_info(),
    ?LOG_SYS("DBs to get views from: ~b", [length(DBs)]),

    DBandDocs = lists:flatten([ begin
				    Encoded = binary:replace(DB, <<"/">>, <<"%2f">>, [global]),
				    {ok, DDocs} = get_ddocs(Encoded),
				    ?LOG_SYS("Got DDocs for ~s: ~b", [Encoded, length(DDocs)]),
				    [{Encoded, binary:replace(wh_json:get_value(<<"id">>, DDoc), <<"_design/">>, <<>>, [global])} || DDoc <- DDocs ]
				end || DB <- DBs ]),

    lists:foldr(fun({DBName, DesignID}, L0) ->
			{ok, DDocData} = get_design_data(DBName, DesignID),

			DataSize = wh_json:get_value([<<"view_index">>, <<"data_size">>], DDocData, -1),
			DiskSize = wh_json:get_value([<<"view_index">>, <<"disk_size">>], DDocData, -1),

			?LOG_SYS("design info for ~s:~s: Dataset: ~b Disksize: ~b", [DesignID, DBName, DataSize, DiskSize]),
			Shards = find_shards(DBName, L),

			[ #design_data{db_name=DBName, design_name=DesignID, shards=Shards
				       ,disk_size=DiskSize, data_size=DataSize}
			  | L0]
		end, [], DBandDocs).

get_design_data(DB, Design) ->
    get_design_data(DB, Design, 0).
get_design_data(_DB, _Design, Cnt) when Cnt > 10 ->
    {error, failed};
get_design_data(DB, Design, Cnt) ->
    case couch_mgr:design_info(DB, Design) of
	{ok, _}=Resp -> Resp;
	_ -> get_design_data(DB, Design, Cnt+1)
    end.	    

get_db_data(DB) ->
    get_db_data(DB, 0).
get_db_data(_DB, Cnt) when Cnt > 10 ->
    ?LOG_SYS("Failed to find data for db ~s", [_DB]),
    {error, failed};
get_db_data(DB, Cnt) ->
    case couch_mgr:admin_db_info(DB) of
	{ok, _}=Resp -> ?LOG_SYS("Found db data for ~s in ~b tries", [DB, Cnt]), Resp;
	{error, _E} -> get_db_data(DB, Cnt+1)
    end.	    

get_ddocs(DB) ->
    get_ddocs(DB, 0).
get_ddocs(_DB, Cnt) when Cnt > 10 ->
    ?LOG_SYS("Failed to get design docs for ~s", [_DB]),
    {error, failed};
get_ddocs(DB, Cnt) ->
    case couch_mgr:all_design_docs(DB) of
	{ok, _}=Resp -> ?LOG_SYS("Found ddocs for ~s in ~p tries", [DB, Cnt]), Resp;
	{error, _E} -> get_ddocs(DB, Cnt+1)
    end.

compact(#db_data{db_name=DBName, data_size=DataSize, disk_size=DiskSize}, MDS, CT)
  when DiskSize > MDS andalso (DiskSize div DataSize) > CT ->
    timer:sleep(random:uniform(10)*1000), %sleep between 1 and 10 seconds
    ?LOG_SYS("Compact DB ~p: ~p and VC: ~p", [DBName, couch_mgr:admin_db_compact(DBName), couch_mgr:db_view_cleanup(DBName)]);
compact(#design_data{shards=Shards, design_name=Design, data_size=DataSize, disk_size=DiskSize}, MDS, CT)
  when DiskSize > MDS andalso (DiskSize div DataSize) > CT ->
    timer:sleep(random:uniform(10)*1000), %sleep between 1 and 10 seconds
    [ ?LOG_SYS("Compact design ~s for ~s: ~p", [Design, DBName, couch_mgr:admin_design_compact(DBName, Design)]) || DBName <- Shards ];
compact(_, _, _) -> ok.

find_shards(DBName, DBs) ->
    find_shards(DBName, DBs, []).

find_shards(_, [], Acc) -> Acc;
find_shards(DBName, [#db_data{db_name=Shard}|DBs], Acc) ->
    case binary:match(Shard, DBName) of
	nomatch -> find_shards(DBName, DBs, Acc);
	_ -> find_shards(DBName, DBs, [Shard | Acc])
    end.
