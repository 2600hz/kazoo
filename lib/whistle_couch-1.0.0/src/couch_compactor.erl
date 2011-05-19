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
-define(TIMEOUT, 1000 * 60 * 60). %% check every hour
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
    {ok, ok, 500}.

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
handle_info(timeout, ok) ->
    {ok, ShardDBs} = couch_mgr:admin_db_info(),

    DBs = [ binary:replace(DB, <<"/">>, <<"%2f">>, [global]) || DB <- ShardDBs ],

    Data1 = lists:foldr(fun(DBName, Acc) ->
				create_db_data(DBName, Acc)
			end, [], DBs),

    Data2 = get_design_docs(Data1),

    logger:format_log(info, "COMPACTOR(~p): ~p~n", [self(), length(Data2)]),

    [ compact(D) || D <- Data2 ],
    
    {noreply, ok, ?TIMEOUT};

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
-spec(create_db_data/2 :: (DBName :: binary(), L :: list()) -> list()).
create_db_data(DBName, L) ->
    try
	{ok, DBData} = couch_mgr:admin_db_info(DBName),
	[#db_data{db_name=DBName
		  ,disk_size=wh_json:get_value(<<"disk_size">>, DBData, 0)
		  ,data_size=wh_json:get_value([<<"other">>, <<"data_size">>], DBData, 0)
		 }
	 | L]
    catch
	_:_ -> L
    end.

get_design_docs(L) ->
    {ok, DBs} = couch_mgr:db_info(),
    DBandDocs = lists:flatten([ begin
				    Encoded = binary:replace(DB, <<"/">>, <<"%2f">>, [global]),
				    {ok, DDocs} = couch_mgr:all_design_docs(Encoded),
				    [{Encoded, binary:replace(wh_json:get_value(<<"id">>, DDoc), <<"_design/">>, <<>>, [global])} || DDoc <- DDocs ]
				end || DB <- DBs ]),

    lists:foldr(fun({DBName, DesignID}, L0) ->
			{ok, DDocData} = couch_mgr:design_info(DBName, DesignID),
			DataSize = wh_json:get_value([<<"view_index">>, <<"data_size">>], DDocData, 0),
			DiskSize = wh_json:get_value([<<"view_index">>, <<"disk_size">>], DDocData, 0),

			Shards = find_shards(DBName, L),

			[ #design_data{db_name=DBName, design_name=DesignID, shards=Shards
				       ,disk_size=DiskSize, data_size=DataSize}
			  | L0]
		end, L, DBandDocs).

compact(#db_data{db_name=DBName, data_size=DataSize, disk_size=DiskSize}) when DiskSize div DataSize > ?COMPACT_THRESHOLD ->
    timer:sleep(random:uniform(10)*1000), %sleep between 1 and 10 seconds
    logger:format_log(info, "compact db ~p: ~p", [DBName, couch_mgr:admin_db_compact(DBName)]);
compact(#design_data{shards=Shards, design_name=Design, data_size=DataSize, disk_size=DiskSize}) when DiskSize div DataSize > ?COMPACT_THRESHOLD ->
    timer:sleep(random:uniform(10)*1000), %sleep between 1 and 10 seconds
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
