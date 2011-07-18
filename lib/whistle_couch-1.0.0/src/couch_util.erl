%%%-----------------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%% Util functions used by whistle_couch
%%% @end
%%% Created :  8 Jul 2011 by James Aimonetti <james@2600hz.org>
%%%-----------------------------------------------------------------------------
-module(couch_util).

-export([get_new_connection/4, get_db/2]).

%% DB operations
-export([db_compact/2, db_create/2, db_create/3, db_delete/2
	 ,db_replicate/2, db_view_cleanup/2, db_info/1, db_info/2
	 ,db_exists/2
	]).

%% Doc related
-export([open_doc/4, lookup_doc_rev/3, save_doc/4, save_docs/4, del_doc/3
	 ,del_docs/3, ensure_saved/4
	]).

%% View-related
-export([design_compact/3, design_info/3, all_design_docs/2, get_results/4
	,all_docs/2
	]).

%% Attachment-related
-export([fetch_attachment/4, put_attachment/5, put_attachment/6, delete_attachment/4
	 ,delete_attachment/5
	]).

-include("wh_couch.hrl").

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%------------------------------------------------------------------------------
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

%%% DB-related functions ---------------------------------------------
-spec db_compact/2 :: (Conn, DbName) -> boolean() when
      Conn :: #server{},
      DbName :: binary().
db_compact(#server{}=Conn, DbName) ->
    Db = get_db(Conn, DbName),
    do_db_compact(Db).

-spec db_create/2 :: (Conn, DbName) -> boolean() when
      Conn :: #server{},
      DbName :: binary().
db_create(#server{}=Conn, DbName) ->
    db_create(Conn, DbName, []).

-spec db_create/3 :: (Conn, DbName, Options) -> boolean() when
      Conn :: #server{},
      DbName :: binary(),
      Options :: [{q,integer()} | {n,integer()},...] | [].
db_create(#server{}=Conn, DbName, Options) ->
    case couchbeam:create_db(Conn, whistle_util:to_list(DbName), [], Options) of
	{error, _} -> false;
	{ok, _} -> true
    end.

-spec db_delete/2 :: (Conn, DbName) -> boolean() when
      Conn :: #server{},
      DbName :: binary().
db_delete(#server{}=Conn, DbName) ->
    case couchbeam:delete_db(Conn, whistle_util:to_list(DbName)) of
	{error, _} -> false;
	{ok, _} -> true
    end.

-spec db_replicate/2 :: (Conn, JSON) -> tuple(ok, json_object()) | tuple(error, term()) when
      Conn :: #server{},
      JSON :: json_object().
db_replicate(#server{}=Conn, JSON) ->
    couchbeam:replicate(Conn, JSON).

-spec db_view_cleanup/2 :: (Conn, DbName) -> boolean() when
      Conn :: #server{},
      DbName :: binary().
db_view_cleanup(#server{}=Conn, DbName) ->
    Db = get_db(Conn, DbName),
    do_db_view_cleanup(Db).

-spec db_info/1 :: (Conn) -> {ok, [binary(),...] | []} | {error, term()} when
      Conn :: #server{}.
db_info(#server{}=Conn) ->
    case couchbeam:all_dbs(Conn) of
	{error, {ok, "504", _, _}} ->
	    db_info(Conn);
	{error, _}=E -> E;
	{ok, _}=OK -> OK
    end.

-spec db_info/2 :: (Conn, DbName) -> {ok, json_object()} | {error, term()} when
      Conn :: #server{},
      DbName :: binary().
db_info(#server{}=Conn, DbName) ->
    couchbeam:db_info(get_db(Conn, DbName)).

-spec db_exists/2 :: (Conn, DbName) -> boolean() when
      Conn :: #server{},
      DbName :: binary().
db_exists(#server{}=Conn, DbName) ->
    couchbeam:db_exists(Conn, whistle_util:to_list(DbName)).

%% Internal DB-related functions -----------------------------------------------

-spec do_db_compact/1 :: (Db) -> boolean() when
      Db :: #db{}.
do_db_compact(#db{}=Db) ->
    case couchbeam:compact(Db) of
	ok -> true;
	{error, {ok, "504", _, _}} ->
	    do_db_compact(Db);
	{error, _E} ->
	    false
    end.

-spec do_db_view_cleanup/1 :: (Db) -> boolean() when
      Db :: #db{}.
do_db_view_cleanup(#db{}=Db) ->
    case couchbeam:view_cleanup(Db) of
	ok -> true;
	{error, {ok, "504", _, _}} ->
	    do_db_view_cleanup(Db);
	{error, _} -> false
    end.

%%% View-related functions -----------------------------------------------------
-spec design_compact/3 :: (Conn, DbName, Design) -> boolean() when
      Conn :: #server{},
      DbName :: binary(),
      Design :: binary().
design_compact(#server{}=Conn, DbName, Design) ->
    case couchbeam:compact(get_db(Conn, DbName), Design) of
	{error, _E} -> false;
	ok -> true
    end.

-spec design_info/3 :: (Conn, DBName, Design) -> {ok, json_object()} | {error, atom()} when
      Conn :: #server{},
      DBName :: binary(),
      Design :: binary().
design_info(#server{}=Conn, DBName, Design) ->
    Db = get_db(Conn, DBName),
    do_get_design_info(Db, Design).

-spec all_design_docs/2 :: (Conn, DBName) -> {ok, json_objects()} | {error, atom()} when
      Conn :: #server{},
      DBName :: binary().
all_design_docs(#server{}=Conn, DBName) ->
    Db = get_db(Conn, DBName),
    {ok, View} = couchbeam:view(Db, "_design_docs", []),
    do_fetch_results(View).

-spec all_docs/2 :: (Conn, DbName) -> {ok, json_objects()} | {error, atom()} when
      Conn :: #server{},
      DbName :: binary().
all_docs(#server{}=Conn, DbName) ->
    {ok, View} = couchbeam:all_docs(get_db(Conn, DbName)),
    do_fetch_results(View).

-spec get_results/4 :: (Conn, DbName, DesignDoc, ViewOptions) -> {ok, json_objects()} | {error, atom()} when
      Conn :: #server{},
      DbName :: binary(),
      DesignDoc :: binary(),
      ViewOptions :: proplist().
get_results(#server{}=Conn, DbName, DesignDoc, ViewOptions) ->    
    Db = get_db(Conn, DbName),
    View = get_view(Db, DesignDoc, ViewOptions),
    do_fetch_results(View).

%% Design Doc/View internal functions

-spec do_fetch_results/1 :: (View) -> {ok, json_objects() | [binary(),...]} | {error, atom()} when
      View :: #view{}.
do_fetch_results(#view{}=View) ->
    case couchbeam_view:fetch(View) of
	{ok, {struct, Prop}} ->
	    Rows = props:get_value(<<"rows">>, Prop, []),
	    {ok, Rows};
	{error, {ok, "504", _, _}} ->
	    ?LOG_SYS("get_results returned a 504, retrying"),
	    do_fetch_results(View);
	{error, _Error}=E -> E
    end.

-spec do_get_design_info/2 :: (Db, Design) -> {ok, json_object()} | {error, atom()} when
      Db :: #db{},
      Design :: binary().
do_get_design_info(#db{}=Db, Design) ->
    case couchbeam:design_info(Db, Design) of
	{ok, _}=OK -> OK;
	{error, {ok, "504", _, _}} ->
	    do_get_design_info(Db, Design);
	{error, _}=E -> E
    end.

%% Document related functions --------------------------------------------------
-spec open_doc/4 :: (Conn, DbName, DocId, Options) -> {ok, json_object()} | {error, atom()} when
      Conn :: #server{},
      DbName :: binary(),
      DocId :: binary(),
      Options :: proplist().
open_doc(#server{}=Conn, DbName, DocId, Options) ->
    Db = get_db(Conn, DbName),
    do_fetch_doc(Db, DocId, Options).

save_doc(#server{}=Conn, DbName, Doc, Options) ->
    Db = get_db(Conn, DbName),
    do_save_doc(Db, Doc, Options).

save_docs(#server{}=Conn, DbName, Docs, Options) ->
    Db = get_db(Conn, DbName),
    do_save_docs(Db, Docs, Options).

-spec lookup_doc_rev/3 :: (Conn, DbName, DocId) -> {ok, binary()} | {error, atom()} when
      Conn :: #server{},
      DbName :: binary(),
      DocId :: binary().
lookup_doc_rev(#server{}=Conn, DbName, DocId) ->
    Db = get_db(Conn, DbName),
    do_fetch_rev(Db, DocId).

-spec ensure_saved/4 :: (Conn, DbName, Doc, Opts) -> {ok, json_object()} | {error, atom()} when
      Conn :: #server{},
      DbName :: binary(),
      Doc :: json_object(),
      Opts :: proplist().
ensure_saved(#server{}=Conn, DbName, Doc, Opts) ->
    Db = get_db(Conn, DbName),
    do_ensure_saved(Db, Doc, Opts).

-spec del_doc/3 :: (Conn, DbName, Doc) -> {ok, json_object()} | {error, atom()} when
      Conn :: #server{},
      DbName :: binary(),
      Doc :: json_object().
del_doc(#server{}=Conn, DbName, Doc) ->
    Db = get_db(Conn, DbName),
    do_delete_doc(Db, Doc).

-spec del_docs/3 :: (Conn, DbName, Doc) -> {ok, json_objects()} | {error, atom()} when
      Conn :: #server{},
      DbName :: binary(),
      Doc :: json_objects().
del_docs(#server{}=Conn, DbName, Doc) ->
    Db = get_db(Conn, DbName),
    do_delete_docs(Db, Doc).

%% Internal Doc functions

-spec do_delete_doc/2 :: (Db, Doc) -> {ok, json_object()} | {error, atom()} when
      Db :: #db{},
      Doc :: json_object().
do_delete_doc(#db{}=Db, Doc) ->
    case couchbeam:delete_doc(Db, Doc) of
	{ok, _}=OK -> OK;
	{error, {ok, "504", _, _}} ->
	    do_delete_doc(Db, Doc);
	{error, _}=E -> E
    end.

-spec do_delete_docs/2 :: (Db, Docs) -> {ok, json_objects()} | {error, atom()} when
      Db :: #db{},
      Docs :: json_objects().
do_delete_docs(#db{}=Db, Docs) ->
    case couchbeam:delete_docs(Db, Docs) of
	{ok, _}=OK -> OK;
	{error, {ok, "504", _, _}} ->
	    do_delete_docs(Db, Docs);
	{error, _}=E -> E
    end.

-spec do_ensure_saved/3 :: (Db, Doc, Opts) -> {ok, json_object()} | {error, atom()} when
      Db :: #db{},
      Doc :: json_object(),
      Opts :: proplist().
do_ensure_saved(#db{}=Db, Doc, Opts) ->
    case do_save_doc(Db, Doc, Opts) of
	{ok, _}=Saved -> Saved;
	{error, conflict} ->
	    Id = wh_json:get_value(<<"_id">>, Doc, <<>>),
	    {ok, Rev} = do_fetch_rev(Db, Id),
	    do_ensure_saved(Db, wh_json:set_value(<<"_rev">>, Rev, Doc), Opts);
	{error, _}=E -> E
    end.

-spec do_fetch_rev/2 :: (Db, DocId) -> {ok, binary()} | {error, atom()} when
      Db :: #db{},
      DocId :: binary().
do_fetch_rev(#db{}=Db, DocId) ->
    case couchbeam:lookup_doc_rev(Db, DocId) of
	{ok, _}=OK -> OK;
	{error, {ok, "504", _, _}} ->
	    do_fetch_rev(Db, DocId);
	{error, _}=E -> E
    end.

-spec do_fetch_doc/3 :: (Db, DocId, Options) -> {ok, json_object()} | {error, atom()} when
      Db :: #db{},
      DocId :: binary(),
      Options :: proplist().
do_fetch_doc(#db{}=Db, DocId, Options) ->
    case couchbeam:open_doc(Db, DocId, Options) of
	{ok, _}=OK -> OK;
	{error, {ok, "504", _, _}} ->
	    do_fetch_doc(Db, DocId, Options);
	{error, _}=E -> E
    end.

-spec do_save_doc/3 :: (Db, Doc, Options) -> {ok, json_object()} | {error, atom()} when
      Db :: #db{},
      Doc :: json_object(),
      Options :: proplist().
do_save_doc(#db{}=Db, Doc, Options) ->
    case couchbeam:save_doc(Db, Doc, Options) of
	{ok, _}=OK -> OK;
	{error, {ok, "504", _, _}} ->
	    do_save_doc(Db, Doc, Options);
	{error, _}=E -> E
    end.

-spec do_save_docs/3 :: (Db, Docs, Options) -> {ok, json_objects()} | {error, atom()} when
      Db :: #db{},
      Docs :: json_objects(),
      Options :: proplist().
do_save_docs(#db{}=Db, Docs, Options) ->
    case couchbeam:save_docs(Db, Docs, Options) of
	{ok, _}=OK -> OK;
	{error, {ok, "504", _, _}} ->
	    do_save_docs(Db, Docs, Options);
	{error, _}=E -> E
    end.

%% Attachment-related functions ------------------------------------------------
-spec fetch_attachment/4 :: (Conn, DbName, DocId, AName) -> {ok, binary()} | {error, atom()} when
      Conn :: #server{},
      DbName :: binary(),
      DocId :: binary(),
      AName :: binary().
fetch_attachment(#server{}=Conn, DbName, DocId, AName) ->
    Db = get_db(Conn, DbName),
    do_fetch_attachment(Db, DocId, AName).

-spec put_attachment/5 :: (Conn, DbName, DocId, AName, Contents) -> {ok, json_object()} | {error, atom()} when
      Conn :: #server{},
      DbName :: binary(),
      DocId :: binary(),
      AName :: binary(),
      Contents :: binary().
put_attachment(#server{}=Conn, DbName, DocId, AName, Contents) ->
    Db = get_db(Conn, DbName),
    {ok, Rev} = do_fetch_rev(Db, DocId),
    do_put_attachment(Db, DocId, AName, Contents, [{rev, Rev}]).

-spec put_attachment/6 :: (Conn, DbName, DocId, AName, Contents, Options) -> {ok, json_object()} | {error, atom()} when
      Conn :: #server{},
      DbName :: binary(),
      DocId :: binary(),
      AName :: binary(),
      Contents :: binary(),
      Options :: proplist().
put_attachment(#server{}=Conn, DbName, DocId, AName, Contents, Options) ->
    Db = get_db(Conn, DbName),
    case props:get_value(rev, Options) of
	undefined ->
	    {ok, Rev} = do_fetch_rev(Db, DocId),
	    do_put_attachment(Db, DocId, AName, Contents, [{rev, Rev} | Options]);
	_ ->
	    do_put_attachment(Db, DocId, AName, Contents, Options)
    end.

-spec delete_attachment/4 :: (Conn, DbName, DocId, AName) -> {ok, json_object()} | {error, atom()} when
      Conn :: #server{},
      DbName :: binary(),
      DocId :: binary(),
      AName :: binary().
delete_attachment(#server{}=Conn, DbName, DocId, AName) ->
    Db = get_db(Conn, DbName),
    {ok, Rev} = do_fetch_rev(Db, DocId),
    do_del_attachment(Db, DocId, AName, [{rev, Rev}]).

-spec delete_attachment/5 :: (Conn, DbName, DocId, AName, Options) -> {ok, json_object()} | {error, atom()} when
      Conn :: #server{},
      DbName :: binary(),
      DocId :: binary(),
      AName :: binary(),
      Options :: proplist().
delete_attachment(#server{}=Conn, DbName, DocId, AName, Options) ->
    Db = get_db(Conn, DbName),
    case props:get_value(rev, Options) of
	undefined ->
	    {ok, Rev} = do_fetch_rev(Db, DocId),
	    do_del_attachment(Db, DocId, AName, [{rev, Rev} | Options]);
	_ ->
	    do_del_attachment(Db, DocId, AName, Options)
    end.

%% Internal Attachment-related functions ---------------------------------------
-spec do_fetch_attachment/3 :: (Db, DocId, AName) -> {ok, binary()} | {error, atom()} when
      Db :: #db{},
      DocId :: binary(),
      AName :: binary().
do_fetch_attachment(#db{}=Db, DocId, AName) ->
    case couchbeam:fetch_attachment(Db, DocId, AName) of
	{ok, _}=OK -> OK;
	{error, {ok, "504", _, _}} ->
	    do_fetch_attachment(Db, DocId, AName);
	{error, _}=E -> E
    end.

-spec do_put_attachment/5 :: (Db, DocId, AName, Contents, Options) -> {ok, json_object()} | {error, atom()} when
      Db :: #db{},
      DocId :: binary(),
      AName :: binary(),
      Contents :: binary(),
      Options :: proplist().
do_put_attachment(#db{}=Db, DocId, AName, Contents, Options) ->
    case couchbeam:put_attachment(Db, DocId, AName, Contents, Options) of
	{ok, _}=OK -> OK;
	{error, {ok, "504", _, _}} ->
	    do_put_attachment(Db, DocId, AName, Contents, Options);
	{error, _}=E -> E
    end.

-spec do_del_attachment/4 :: (Db, DocId, AName, Options) -> {ok, json_object()} | {error, atom()} when
      Db :: #db{},
      DocId :: binary(),
      AName :: binary(),
      Options :: proplist().
do_del_attachment(#db{}=Db, DocId, AName, Options) ->
    case couchbeam:delete_attachment(Db, DocId, AName, Options) of
	{ok, _}=OK -> OK;
	{error, {ok, "504", _, _}} ->
	    do_del_attachment(Db, DocId, AName, Options);
	{error, _}=E -> E
    end.

%% Helpers for getting Couchbeam records ---------------------------------------

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% returns the #db{} record
%% @end
%%------------------------------------------------------------------------------
-spec get_db/2 :: (Conn, DbName) -> #db{} when
      Conn :: #server{},
      DbName :: binary().
get_db(#server{}=Conn, DbName) ->
    {ok, Db} = couchbeam:open_db(Conn, DbName),
    Db.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% get_view, if Db/DesignDoc is known, return {#view{}, Views},
%% else returns {#view{}, [{{#db{}, DesignDoc, ViewOpts}, #view{}} | Views]}
%% @end
%%------------------------------------------------------------------------------
-spec get_view/3 :: (Db, DesignDoc, ViewOptions) -> #view{} when
      Db :: #db{},
      DesignDoc :: binary(),
      ViewOptions :: list().
get_view(#db{}=Db, DesignDoc, ViewOptions) ->
    {ok, View} = couchbeam:view(Db, DesignDoc, ViewOptions),
    View.
