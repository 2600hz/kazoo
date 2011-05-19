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

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 1000 * 60 * 10). %% check every ten minutes
-define(COMPACT_THRESHOLD, 100000). %% ratio of DiskSize div DataSize

-record(design_data, {
	  db_name = <<>> :: binary()
	 ,design_name = <<>> :: binary()
         ,shards = [] :: list(binary()) | []
	 ,disk_size = 0 :: non_neg_integer()
         ,data_size = 0 :: non_neg_integer()
	 }).
-record(db_data, {
	  db_name = <<>> :: binary()
	 ,disk_size = 0 :: non_neg_integer()
	 ,data_size = 0 :: non_neg_integer()
	 }).

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
    {ok, queue:new(), 500}.

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
    {reply, ok, State}.

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
handle_info(timeout, Data) ->
    {ok, ShardDBs} = couch_mgr:admin_db_info(),

    OldDBs = [ Name || #db_data{db_name=Name} <- queue:to_list(Data)],
    NewDBs = [ Encoded || DB <- ShardDBs,
			  not lists:member(Encoded = binary:replace(DB, <<"/">>, <<"%2f">>, [global]), OldDBs)
	     ],

    Data1 = lists:foldr(fun(DBName, Q0) ->
				create_db_data(DBName, Q0)
			end, Data, NewDBs),

    Data2 = get_design_docs(Data1),

    logger:format_log(info, "COMPACTOR(~p): ~p~n", [self(), queue:len(Data2)]),

    [ compact(D) || D <- queue:to_list(Data2) ],
    
    {noreply, Data2, ?TIMEOUT};

handle_info(_Info, State) ->
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

%% Sharded DB names from admin interface
-spec(create_db_data/2 :: (DBName :: binary(), Q :: queue()) -> queue()).
create_db_data(DBName, Q) ->
    try
	{ok, DBData} = couch_mgr:admin_db_info(DBName),
	queue:in(#db_data{db_name=DBName
			  ,disk_size=wh_json:get_value(<<"disk_size">>, DBData, 0)
			  ,data_size=wh_json:get_value([<<"other">>, <<"data_size">>], DBData, 0)
			 }, Q)
    catch
	_:_ -> Q
    end.

get_design_docs(DataQ) ->
    {ok, DBs} = couch_mgr:db_info(),
    DBandDocs = lists:flatten([ begin
				    Encoded = binary:replace(DB, <<"/">>, <<"%2f">>, [global]),
				    {ok, DDocs} = couch_mgr:all_design_docs(Encoded),
				    [{Encoded, binary:replace(wh_json:get_value(<<"id">>, DDoc), <<"_design/">>, <<>>, [global])} || DDoc <- DDocs ]
				end || DB <- DBs ]),

    DataL = queue:to_list(DataQ),

    OldDBandDocs = [ {DBName, Design} || #design_data{db_name=DBName, design_name=Design} <- DataL],
    NewDBandDocs = [ Pair || Pair <- DBandDocs, not lists:member(Pair, OldDBandDocs) ],

    lists:foldr(fun({DBName, DesignID}, Q0) ->
			{ok, DDocData} = couch_mgr:design_info(DBName, DesignID),
			DataSize = wh_json:get_value([<<"view_index">>, <<"data_size">>], DDocData, 0),
			DiskSize = wh_json:get_value([<<"view_index">>, <<"disk_size">>], DDocData, 0),

			Shards = find_shards(DBName, DataL),

			queue:in(#design_data{db_name=DBName, design_name=DesignID, shards=Shards
					      ,disk_size=DiskSize, data_size=DataSize}, Q0)
		end, DataQ, NewDBandDocs).

compact(#db_data{db_name=DBName, data_size=DataSize, disk_size=DiskSize}) when DiskSize div DataSize > ?COMPACT_THRESHOLD ->
    logger:format_log(info, "compact db ~p: ~p", [DBName, couch_mgr:admin_db_compact(DBName)]);
compact(#design_data{shards=Shards, design_name=Design, data_size=DataSize, disk_size=DiskSize}) when DiskSize div DataSize > ?COMPACT_THRESHOLD ->
    [ logger:format_log(info, "compact ds: ~p:~p: ~p~n", [DBName, Design, couch_mgr:admin_design_compact(DBName, Design)]) || DBName <- Shards ];
compact(_) -> ok.

find_shards(DBName, DBs) ->
    find_shards(DBName, DBs, []).

find_shards(DBName, [#db_data{db_name=Shard}|DBs], Acc) ->
    case binary:match(Shard, DBName) of
	nomatch -> find_shards(DBName, DBs, Acc);
	_ -> find_shards(DBName, DBs, [Shard | Acc])
    end;
find_shards(DBName, [_|DBs], Acc) ->
    find_shards(DBName, DBs, Acc);
find_shards(_, [], Acc) -> Acc.
