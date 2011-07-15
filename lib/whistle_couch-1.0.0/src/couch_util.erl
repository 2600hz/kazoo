%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%% Util functions used by whistle_couch
%%% @end
%%% Created :  8 Jul 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(couch_util).

-export([get_new_connection/4, open_db/2, get_view/3]).

%% DB operations
-export([db_compact/2, db_create/2, db_delete/2, db_replicate/2
	 ,db_view_cleanup/2, db_info/1, db_info/2
	]).

%% View-related
-export([design_compact/3, design_info/3, all_design_docs/2]).

-include("wh_couch.hrl").

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_new_connection/4 :: (Host, Port, User, Pass) -> #server{} when
      Host :: string(),
      Port :: integer(),
      User :: string(),
      Pass :: string().
get_new_connection(Host, Port, "", "") ->
    get_new_conn(Host, Port, ?IBROWSE_OPTS);
get_new_connection(Host, Port, User, Pass) ->
    get_new_conn(Host, Port, [{basic_auth, {User, Pass}} | ?IBROWSE_OPTS]).

-spec get_new_conn/3 :: (Host, Port, Opts) -> #server{} when
      Host :: string(),
      Port :: integer(),
      Opts :: proplist().
get_new_conn(Host, Port, Opts) ->
    Conn = couchbeam:server_connection(Host, Port, "", Opts),
    ?LOG_SYS("New connection to Host ~s, testing", [Host]),
    {ok, _Version} = couchbeam:server_info(Conn),
    ?LOG_SYS("Connected successfully to ~s: ~p", [Host, _Version]),
    Conn.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% returns the #db{} record
%% @end
%%--------------------------------------------------------------------
-spec open_db/2 :: (DbName, Conn) -> tuple(error, term()) | #db{} when
      DbName :: binary(),
      Conn :: #server{}.
open_db(DbName, #server{}=Conn) ->
    case couchbeam:open_db(Conn, DbName) of
        {error, _}=E -> E;
        {ok, Db} -> Db
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% get_view, if Db/DesignDoc is known, return {#view{}, Views},
%% else returns {#view{}, [{{#db{}, DesignDoc, ViewOpts}, #view{}} | Views]}
%% @end
%%--------------------------------------------------------------------
-spec get_view/3 :: (Db, DesignDoc, ViewOptions) -> #view{} | tuple(error, view_not_found) when
      Db :: #db{},
      DesignDoc :: binary(),
      ViewOptions :: list().
get_view(#db{}=Db, DesignDoc, ViewOptions) ->
    case couchbeam:view(Db, DesignDoc, ViewOptions) of
	{error, _Error}=E -> E;
	{ok, View} -> View
    end.

%%% DB-related functions ---------------------------------------------
-spec db_compact/2 :: (DbName, Conn) -> boolean() when
      DbName :: binary(),
      Conn :: #server{}.
db_compact(DbName, #server{}=Conn) ->
    case couchbeam:compact(couch_util:open_db(DbName, Conn)) of
	{error, _E} ->
	    ?LOG("Error admin compacting ~s: ~p", [DbName, _E]),
	    false;
	ok ->
	    true
    end.

-spec db_create/2 :: (DbName, Conn) -> boolean() when
      DbName :: binary(),
      Conn :: #server{}.
db_create(DbName, #server{}=Conn) ->
    case couchbeam:create_db(Conn, whistle_util:to_list(DbName)) of
	{error, _} -> false;
	{ok, _} -> true
    end.

-spec db_delete/2 :: (DbName, Conn) -> boolean() when
      DbName :: binary(),
      Conn :: #server{}.
db_delete(DbName, #server{}=Conn) ->
    case couchbeam:delete_db(Conn, whistle_util:to_list(DbName)) of
	{error, _} -> false;
	{ok, _} -> true
    end.

-spec db_replicate/2 :: (Conn, JSON) -> tuple(ok, term()) | tuple(error, term()) when
      Conn :: #server{},
      JSON :: json_object().
db_replicate(#server{}=Conn, JSON) ->
    couchbeam:replicate(Conn, JSON).

-spec db_view_cleanup/2 :: (DbName, Conn) -> boolean() when
      DbName :: binary(),
      Conn :: #server{}.
db_view_cleanup(DbName, #server{}=Conn) ->
    case couchbeam:view_cleanup(couch_util:open_db(DbName, Conn)) of
	{error, _E} -> false;
	ok -> true
    end.

-spec db_info/1 :: (Conn) -> {ok, [binary(),...] | []} | {error, term()} when
      Conn :: #server{}.
db_info(#server{}=Conn) ->
    case couchbeam:all_dbs(Conn) of
	{error, _}=E -> E;
	{ok, _}=Info -> Info
    end.

-spec db_info/2 :: (Conn, DbName) -> {ok, json_object()} | {error, term()} when
      Conn :: #server{},
      DbName :: binary().
db_info(#server{}=Conn, DbName) ->
    couchbeam:db_info(?MODULE:open_db(DbName, Conn)).

%%% View-related functions -----------------------------------------------------
design_compact(DbName, Design, Conn) ->
    case couchbeam:compact(couch_util:open_db(DbName, Conn), Design) of
	{error, _E} -> false;
	ok -> true
    end.

-spec design_info/3 :: (Conn, DBName, Design) -> {ok, json_object()} | {error, term()} when
      Conn :: #server{},
      DBName :: binary(),
      Design :: binary().
design_info(#server{}=Conn, DBName, Design) ->
    DB = ?MODULE:open_db(DBName, Conn),
    couchbeam:design_info(DB, Design).

-spec all_design_docs/2 :: (Conn, DBName) -> {ok, [binary(),...] | []} | {error, term()} when
      Conn :: #server{},
      DBName :: binary().
all_design_docs(#server{}=Conn, DBName) ->
    case ?MODULE:open_db(DBName, Conn) of
	{error, _}=E -> E;
	Db ->
	    {ok, View} = couchbeam:view(Db, "_design_docs", []),
	    case couchbeam_view:fetch(View) of
		{ok, {struct, Prop}} ->
		    Rows = props:get_value(<<"rows">>, Prop, []),
		    {ok, Rows};
		{error, _Error}=E -> E
	    end
    end.
