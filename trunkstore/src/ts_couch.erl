%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Manage CouchDB connections
%%% @end
%%% Created : 16 Sep 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ts_couch).

-behaviour(gen_server).

%% API
-export([start_link/0, db_info/1]).

%% Document manipulation
-export([new_doc/0, add_to_doc/3, rm_from_doc/2, save_doc/2, open_doc/2]).

%% Views
-export([has_view/2, count/2, count/3, get_all_results/2, get_results/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

-include("ts.hrl").

-define(SERVER, ?MODULE). 

%% Connection = {CouchPid, MRef}
%% Databases = [{ {ProcessPid, DbName}, DbPid, MRef}]
%% Views = [{ {ProcessPid, DesignDoc, ViewOptions}, ViewPid, MRef}]
%% ViewOptions :: Proplist() -> See http://wiki.apache.org/couchdb/HTTP_view_API#Querying_Options
-record(state, {connection, databases=[], views=[]}).

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

%% get info about Db from Couch
db_info(DbName) ->
    gen_server:call(?MODULE, {db_info, to_list(DbName)}).

%% create a new Document - a tuple with a proplist
new_doc() ->
    {[]}.

%% open a document given a docid
%% returns not_found or the Document
open_doc(DbName, DocId) ->
    gen_server:call(?MODULE, {open_doc, to_list(DbName), to_binary(DocId)}).

%% add a K/V pair to a Document
add_to_doc(Key, Value, Doc) ->
    couchbeam_doc:extend(to_binary(Key), Value, Doc).

%% remove a K from the Document
rm_from_doc(Key, Doc) ->
    couchbeam_doc:delete_value(to_binary(Key), Doc).

%% save a Document to the DB
save_doc(DbName, Doc) ->
    gen_server:call(?MODULE, {save_doc, to_list(DbName), Doc}).

%% Does the DB have a View in the specified design doc? - must be called before views can be accessed
has_view(DbName, DesignDoc) ->
    gen_server:call(?MODULE, {has_view, to_list(DbName), to_list(DesignDoc)}).

%% Count how many rows are in the view
count(DbName, DesignDoc) ->
    count(DbName, DesignDoc, []).
count(DbName, DesignDoc, ViewOptions) ->
    gen_server:call(?MODULE, {count, to_list(DbName), to_list(DesignDoc), ViewOptions}).

%% get the results of the view
%% {Total, Offset, Meta, Rows}
get_all_results(DbName, DesignDoc) ->
    get_results(DbName, DesignDoc, []).

%% {Total, Offset, Meta, Rows}
get_results(DbName, DesignDoc, ViewOptions) ->
    gen_server:call(?MODULE, {get_results, to_list(DbName), to_list(DesignDoc), ViewOptions}).

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
    process_flag(trap_exit, true),
    {ok, #state{connection=get_new_connection()}}.

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
handle_call({count, DbName, DesignDoc, ViewOptions}, {From, _Ref}
	    ,#state{connection={Conn,_MRefConn}, databases=DBs, views=Vs}=State) ->
    {DP, State1} = case find_db_pid(DBs, DbName, From) of
		       nopid ->
			   case open_db(Conn, DbName, From, State) of
			       {ok, DbPid, State0} ->
				   {DbPid, State0};
			       ErrD ->
				   {ErrD, State}
			   end;
		       DbPid ->
			   {DbPid, State}
		   end,
    case DP of
	Pid when is_pid(Pid) ->
	    case find_view_pid(Vs, DesignDoc, From) of
		nopid ->
		    case open_view(DP, DesignDoc, From, State1) of
			{ok, ViewPid, State2} ->
			    {reply, couchbeam_view:count(ViewPid, ViewOptions), State2};
			ErrV ->
			    {reply, ErrV, State1}
		    end;
		ViewPid ->
		    {reply, couchbeam_view:count(ViewPid, ViewOptions), State1}
	    end;
	Err ->
	    {reply, {error, Err}, State1}
    end;
handle_call({get_results, DbName, DesignDoc, ViewOptions}, {From, _Ref}
	    ,#state{connection={Conn,_MRefConn}, databases=DBs, views=Vs}=State) ->
    format_log(info, "TS_COUCH(~p): get_results ~p ~p ~p~n", [self(), DbName, DesignDoc, ViewOptions]),
    {DP, State1} = case find_db_pid(DBs, DbName, From) of
		       nopid ->
			   case open_db(Conn, DbName, From, State) of
			       {ok, DbPid, State0} ->
				   {DbPid, State0};
			       ErrD ->
				   {ErrD, State}
			   end;
		       DbPid ->
			   {DbPid, State}
		   end,
    format_log(info, "TS_COUCH(~p): get_results DP ~p~n", [self(), DP]),
    case DP of
	Pid when is_pid(Pid) ->
	    case find_view_pid(Vs, DesignDoc, From) of
		nopid ->
		    case open_view(DP, DesignDoc, From, State1) of
			{ok, ViewPid, State2} ->
			    Res = couchbeam_view:fetch(ViewPid, ViewOptions),
			    format_log(info, "TS_COUCH(~p): get_results ViewPid created ~p.~nFetch returned ~p~n"
				       ,[self(), ViewPid, Res]),
			    case Res of
				{ok, {Json}} -> {reply, get_value(<<"rows">>, Json), State2};
				_ -> {reply, [], State2}
			    end;
			ErrV ->
			    format_log(info, "TS_COUCH(~p): get_results ErrV ~p~n", [self(), ErrV]),
			    {reply, ErrV, State1}
		    end;
		ViewPid ->
		    Res = couchbeam_view:fetch(ViewPid, ViewOptions),
		    format_log(info, "TS_COUCH(~p): get_results ViewPid found ~p.~nFetch returned ~p~n"
			       ,[self(), ViewPid, Res]),
		    case Res of
			{ok, {Json}} -> {reply, get_value(<<"rows">>, Json), State1};
			_ -> {reply, [], State1}
		    end
	    end;
	Err ->
	    format_log(info, "TS_COUCH(~p): get_results Err ~p~n", [self(), Err]),
	    {reply, {error, Err}, State1}
    end;
handle_call({has_view, DbName, DesignDoc}, {From, _Ref}
	    ,#state{connection={Conn,_MRefConn}, databases=DBs, views=Vs}=State) ->
    format_log(info, "TS_COUCH(~p): has_view ~p ~p~n"
	       ,[self(), DbName, DesignDoc]),
    {DP, State1} = case find_db_pid(DBs, DbName, From) of
		       nopid ->
			   case open_db(Conn, DbName, From, State) of
			       {ok, DbPid, State0} ->
				   {DbPid, State0};
			       ErrD ->
				   {ErrD, State}
			   end;
		       DbPid ->
			   {DbPid, State}
		   end,
    format_log(info, "TS_COUCH(~p): has_view DBPid ~p~n", [self(), DP]),
    case DP of
	Pid when is_pid(Pid) ->
	    case find_view_pid(Vs, DesignDoc, From) of
		nopid ->
		    case open_view(DP, DesignDoc, From, State1) of
			{ok, _ViewPid, State2} ->
			    format_log(info, "TS_COUCH(~p): has_view true~n", [self()]),
			    {reply, true, State2};
			ErrV ->
			    format_log(info, "TS_COUCH(~p): has_view ErrV: ~p~n", [self(), ErrV]),
			    {reply, false, State1}
		    end;
		_ViewPid ->
		    format_log(info, "TS_COUCH(~p): has_view true~n", [self()]),
		    {reply, true, State1}
	    end;
	Err ->
	    format_log(info, "TS_COUCH(~p): has_view Err ~p~n", [self(), Err]),
	    {reply, false, State1}
    end;
handle_call({open_doc, DbName, DocId}, {From, _Ref}, #state{connection={Conn,_MRefConn}, databases=DBs}=State) ->
    case find_db_pid(DBs, DbName, From) of
	nopid ->
	    case open_db(Conn, DbName, From, State) of
		{ok, DbPid, State1} ->
		    {reply, couchbeam_db:open_doc(DbPid, DocId), State1};
		Err ->
		    {reply, Err, State}
	    end;
	DbPid ->
	    {reply, couchbeam_db:open_doc(DbPid, DocId), State}
    end;
handle_call({save_doc, DbName, Doc}, {From, _Ref}, #state{connection={Conn,_MRefConn}, databases=DBs}=State) ->
    case find_db_pid(DBs, DbName, From) of
	nopid ->
	    case open_db(Conn, DbName, From, State) of
		{ok, DbPid, State1} ->
		    {reply, couchbeam_db:save_doc(DbPid, Doc), State1};
		Err ->
		    {reply, Err, State}
	    end;
	DbPid ->
	    {reply, couchbeam_db:save_doc(DbPid, Doc), State}
    end;
handle_call({db_info, DbName}, {From, _Ref}, #state{connection={Conn,_MRefConn}, databases=DBs}=State) ->
    case lists:keyfind({From, DbName}, 1, DBs) of
	{{From, DbName}, DbPid, _MRef} ->
	    {reply, couchbeam_db:info(DbPid), State};
	false ->
	    case open_db(Conn, DbName, From, State) of
		{ok, DbPid, State1} ->
		    {reply, couchbeam_db:info(DbPid), State1};
		{error, _Reason}=Err ->
		    {reply, Err, State}
	    end
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_info({'DOWN', MRefConn, process, CouchPid, Reason}
	    ,#state{connection={CouchPid,MRefConn}, databases=DBs, views=Vs}=State) ->
    format_log(error, "TS_COUCH(~p): CouchConn(~p) went down: ~p~n", [self(), CouchPid, Reason]),
    close_all_databases(DBs),
    close_all_views(Vs),
    {noreply, State#state{connection=get_new_connection(), databases=[]}};
handle_info({'DOWN', _Ref, process, DbPid, _Reason}, #state{databases=DBs}=State) ->
    format_log(error, "TS_COUCH(~p): Db(~p) went down: ~p~n", [self(), DbPid, _Reason]),
    case lists:keyfind(DbPid, 2, DBs) of
        {{_ProcessPid, DbName}, DbPid, MRef} ->
            case erlang:is_process_alive(DbPid) of
		true ->
		    couchbeam_db:close(DbName);
		false ->
		    ok
	    end,
	    erlang:demonitor(MRef),
	    {noreply, #state{databases = lists:keydelete(DbPid, 2, DBs)}};
        false ->
	    {noreply, State}
    end;
handle_info({'EXIT', _Pid, Reason}, State) ->
    format_log(error, "TS_COUCH(~p): EXIT received for ~p with reason ~p~n", [self(), _Pid, Reason]),
    {stop, Reason, State};
handle_info(_Info, State) ->
    format_log(error, "TS_COUCH(~p): Unexpected info ~p~n", [self(), _Info]),
    {noreply, State}.

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
close_all_databases(DBs) ->
    lists:foreach(fun({{_ProcessPid, DbName}, DbPid, MRef}) ->
			  case is_process_alive(DbPid) of
			      true ->
				  couchbeam_db:close(DbName),
				  erlang:demonitor(MRef);
			      false ->
				  ok
			  end
		  end, DBs).

close_all_views(Vs) ->
    lists:foreach(fun({_Key, ViewPid, MRef}) ->
			  case is_process_alive(ViewPid) of
			      true ->
				  couchbeam_view:close_view(ViewPid),
				  erlang:demonitor(MRef);
			      false ->
				  ok
			  end
		  end, Vs).

get_new_connection() ->
    get_new_connection(?COUCH_PARAMS).
get_new_connection(Params) ->
    couchbeam:start(),
    Conn = couchbeam_server:start_connection_link(Params),
    MRefConn = erlang:monitor(process, Conn),
    format_log(info, "TS_COUCH(~p): Starting up conn ~p (~p) using~n~p~n", [self(), Conn, MRefConn, Params]),
    {Conn, MRefConn}.

-spec(open_db/4 :: (Conn :: pid(), DbName :: string(), From :: pid(), State :: tuple()) -> tuple()).
open_db(Conn, DbName, From, #state{databases=DBs}=State) ->
    case couchbeam_db:open_or_create(Conn, DbName) of
	DbPid when is_pid(DbPid) ->
	    format_log(info, "TS_COUCH(~p): Created DbPid(~p : ~p) for ~p~n", [self(), DbName, DbPid, From]),
	    MRef = erlang:monitor(process, DbPid),
	    {ok, DbPid, State#state{databases=[ {{From, DbName}, DbPid, MRef} | DBs]}};
	{ok, Error} ->
	    format_log(error, "TS_COUCH(~p): Unable to create ~p (~p)~n", [self(), DbName, Error]),
	    {error, Error, State};
	Else ->
	    format_log(error, "TS_COUCH(~p): Error creating ~p (~p)~n", [self(), DbName, Else]),
	    {error, Else, State}
    end.

-spec(find_db_pid/3 :: (DBs :: list(), DbName :: string(), From :: pid()) -> pid() | nopid).
find_db_pid(DBs, DbName, From) ->
    case lists:keyfind({From, DbName}, 1, DBs) of
	{_Key, DbPid, _MRef} ->
	    DbPid;
	false ->
	    nopid
    end.

-spec(open_view/4 :: (DbPid :: pid(), DesignDoc :: string(), From :: pid(), State :: tuple()) -> tuple()).
open_view(DbPid, DesignDoc, From, #state{views=Vs}=State) ->
    case couchbeam_db:view(DbPid, DesignDoc) of
	ViewPid when is_pid(ViewPid) ->
	    MRef = erlang:monitor(process, ViewPid),
	    format_log(info, "TS_COUCH(~p): Created ViewPid(~p : ~p) for ~p~n"
		       ,[self(), DesignDoc, ViewPid, From]),
	    {ok, ViewPid, State#state{views=[ {{From, DesignDoc}, ViewPid, MRef} | Vs]}};
	{ok, Error} ->
	    format_log(error, "TS_COUCH(~p): Unable to create ~p(~p)~n", [self(), DesignDoc, Error]),
	    {error, Error, State};
	Else ->
	    format_log(error, "TS_COUCH(~p): Error creating ~p(~p)~n", [self(), DesignDoc, Else]),
	    {error, Else, State}
    end.

-spec(find_view_pid/3 :: (Vs :: list(), DesignDoc :: string(), From :: pid()) -> pid() | nopid).
find_view_pid(Vs, DesignDoc, From) ->
    case lists:keyfind({From, DesignDoc}, 1, Vs) of
	{_Key, ViewPid, _MRef} ->
	    ViewPid;
	false ->
	    nopid
    end.

-spec(to_list/1 :: (X :: atom() | list() | binary()) -> list()).
to_list(X) when is_binary(X) ->
    binary_to_list(X);
to_list(X) when is_atom(X) ->
    atom_to_list(X);
to_list(X) when is_list(X) ->
    X.

-spec(to_binary/1 :: (X :: atom() | list() | binary()) -> binary()).
to_binary(X) when is_atom(X) ->
    list_to_binary(atom_to_list(X));
to_binary(X) when is_list(X) ->
    list_to_binary(X);
to_binary(X) when is_binary(X) ->
    X.
