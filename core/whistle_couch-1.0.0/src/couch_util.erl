%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%% Util functions used by whistle_couch
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-----------------------------------------------------------------------------
-module(couch_util).

-export([get_new_connection/4
         ,get_db/2
         ,server_url/1
         ,db_url/2
         ,server_info/1
         ,format_error/1
        ]).

%% DB operations
-export([db_compact/2
         ,db_create/2
         ,db_create/3
         ,db_delete/2
         ,db_replicate/2
         ,db_view_cleanup/2
         ,db_info/1
         ,db_info/2
         ,db_exists/2
        ]).

%% Doc related
-export([open_cache_doc/4
         ,cache_db_doc/3
         ,flush_cache_doc/4
         ,flush_cache_docs/0, flush_cache_docs/1
         ,open_doc/4
         ,lookup_doc_rev/3
         ,save_doc/4
         ,save_docs/4
         ,del_doc/3
         ,del_docs/3
         ,ensure_saved/4
        ]).

%% View-related
-export([design_compact/3
         ,design_info/3
         ,all_design_docs/3
         ,get_results/4
         ,get_results_count/4
         ,all_docs/3
        ]).

%% Attachment-related
-export([fetch_attachment/4
         ,stream_attachment/5
         ,put_attachment/5
         ,put_attachment/6
         ,delete_attachment/4
         ,delete_attachment/5
        ]).

%% Settings-related
-export([max_bulk_insert/0]).

-include_lib("wh_couch.hrl").

%% Throttle how many docs we bulk insert to BigCouch
-define(MAX_BULK_INSERT, 2000).
-define(RETRY_504(F), retry504s(fun() -> F end)).

-type db_create_options() :: [{'q',integer()} | {'n',integer()},...] | [].
-export_type([db_create_options/0, couchbeam_errors/0]).

-type ddoc() :: ne_binary() | 'all_docs' | 'design_docs'.

%%------------------------------------------------------------------------------
%% @public
%% @doc How many documents are chunked when doing a bulk save
%% @end
%%------------------------------------------------------------------------------
-spec max_bulk_insert() -> ?MAX_BULK_INSERT.
max_bulk_insert() -> ?MAX_BULK_INSERT.

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_new_connection(nonempty_string() | ne_binary(), pos_integer(), string(), string()) ->
                                server() |
                                {'error', 'timeout'}.
get_new_connection(Host, Port, "", "") ->
    get_new_conn(Host, Port, ?IBROWSE_OPTS);
get_new_connection(Host, Port, User, Pass) ->
    get_new_conn(Host, Port, [{'basic_auth', {User, Pass}} | ?IBROWSE_OPTS]).

-spec get_new_conn(nonempty_string() | ne_binary(), pos_integer(), wh_proplist()) -> server().
get_new_conn(Host, Port, Opts) ->
    Conn = couchbeam:server_connection(wh_util:to_list(Host), Port, "", Opts),
    lager:debug("new connection to host ~s:~b, testing: ~p", [Host, Port, Conn]),
    case server_info(Conn) of
        {'ok', ConnData} ->
            CouchVersion = wh_json:get_value(<<"version">>, ConnData),
            BigCouchVersion = wh_json:get_value(<<"bigcouch">>, ConnData),
            lager:info("connected successfully to ~s:~b", [Host, Port]),
            lager:debug("responding CouchDB version: ~p", [CouchVersion]),
            lager:debug("responding BigCouch version: ~p", [BigCouchVersion]),
            Conn;
        {'error', {'conn_failed', {'error', 'timeout'}}} ->
            lager:warning("connection timed out for ~s:~p", [Host, Port]),
            {'error', 'timeout'}
    end.

server_info(#server{}=Conn) -> couchbeam:server_info(Conn).

-spec server_url(server()) -> ne_binary().
server_url(#server{host=Host, port=Port, options=Options}) ->
    UserPass = case props:get_value('basic_auth', Options) of
                   'undefined' -> <<>>;
                   {U, P} -> list_to_binary([U, <<":">>, P])
               end,
    Protocol = case wh_util:is_true(props:get_value('is_ssl', Options)) of
                   'false' -> <<"http">>;
                   'true' -> <<"https">>
               end,

    list_to_binary([Protocol, <<"://">>, UserPass
                    ,<<"@">>, wh_util:to_binary(Host)
                    ,<<":">>, wh_util:to_binary(Port)
                    ,<<"/">>
                   ]).

-spec db_url(server(), ne_binary()) -> ne_binary().
db_url(#server{}=Conn, DbName) ->
    Server = server_url(Conn),
    list_to_binary([Server, DbName]).

%%% DB-related functions ---------------------------------------------
-spec db_compact(server(), ne_binary()) -> boolean().
db_compact(#server{}=Conn, DbName) ->
    Db = get_db(Conn, DbName),
    do_db_compact(Db).

-spec db_create(server(), ne_binary()) -> boolean().
db_create(#server{}=Conn, DbName) ->
    db_create(Conn, DbName, []).

-spec db_create(server(), ne_binary(), db_create_options()) -> boolean().
db_create(#server{}=Conn, DbName, Options) ->
    case couchbeam:create_db(Conn, wh_util:to_list(DbName), [], Options) of
        {'error', _} -> 'false';
        {'ok', _} -> 'true'
    end.

-spec db_delete(server(), ne_binary()) -> boolean().
db_delete(#server{}=Conn, DbName) ->
    case couchbeam:delete_db(Conn, wh_util:to_list(DbName)) of
        {'error', _} -> 'false';
        {'ok', _} -> 'true'
    end.

-spec db_replicate(server(), wh_json:object() | wh_proplist()) ->
                                {'ok', wh_json:object()} |
                                couchbeam_error().
db_replicate(#server{}=Conn, Prop) when is_list(Prop) ->
    couchbeam:replicate(Conn, wh_json:from_list(Prop));
db_replicate(#server{}=Conn, JObj) ->
    couchbeam:replicate(Conn, JObj).

-spec db_view_cleanup(server(), ne_binary()) -> boolean().
db_view_cleanup(#server{}=Conn, DbName) ->
    Db = get_db(Conn, DbName),
    do_db_view_cleanup(Db).

-spec db_info(server()) ->
                     {'ok', ne_binaries()} |
                     couchbeam_error().
db_info(#server{}=Conn) ->
    ?RETRY_504(couchbeam:all_dbs(Conn)).

-spec db_info(server(), ne_binary()) ->
                     {'ok', wh_json:object()} |
                     couchbeam_error().
db_info(#server{}=Conn, DbName) ->
    ?RETRY_504(couchbeam:db_info(get_db(Conn, DbName))).

-spec db_exists(server(), ne_binary()) -> boolean().
db_exists(#server{}=Conn, DbName) ->
    couchbeam:db_exists(Conn, wh_util:to_list(DbName)).

%% Internal DB-related functions -----------------------------------------------

-spec do_db_compact(db()) -> boolean().
do_db_compact(#db{}=Db) ->
    do_db_compact(Db, 0).
do_db_compact(_Db, Retries) when Retries > 3 ->
    lager:debug("retries exceeded"),
    'false';
do_db_compact(Db, Retries) ->
    case ?RETRY_504(couchbeam:compact(Db)) of
        'ok' -> 'true';
        {'conn_failed', {'error', 'timeout'}} ->
            lager:debug("connection timed out, trying again"),
            do_db_compact(Db, Retries+1);
        {'error', _E} ->
            lager:debug("failed to compact: ~p", [_E]),
            'false'
    end.

-spec do_db_view_cleanup(db()) -> boolean().
do_db_view_cleanup(#db{}=Db) ->
    'ok' =:= ?RETRY_504(couchbeam:view_cleanup(Db)).

%%% View-related functions -----------------------------------------------------
-spec design_compact(server(), ne_binary(), ne_binary()) -> boolean().
design_compact(#server{}=Conn, DbName, Design) ->
    case couchbeam:compact(get_db(Conn, DbName), Design) of
        {'error', _E} -> 'false';
        'ok' -> 'true'
    end.

-spec design_info(server(), ne_binary(), ne_binary()) ->
                         {'ok', wh_json:object()} |
                         couchbeam_error().
design_info(#server{}=Conn, DBName, Design) ->
    Db = get_db(Conn, DBName),
    do_get_design_info(Db, Design).

-spec all_design_docs(server(), ne_binary(), wh_proplist()) ->
                             {'ok', wh_json:objects()} |
                             couchbeam_error().
all_design_docs(#server{}=Conn, DBName, Options) ->
    Db = get_db(Conn, DBName),
    do_fetch_results(Db, 'design_docs', Options).

-spec all_docs(server(), ne_binary(), wh_proplist()) ->
                      {'ok', wh_json:objects()} |
                      couchbeam_error().
all_docs(#server{}=Conn, DbName, Options) ->
    Db = get_db(Conn, DbName),
    do_fetch_results(Db, 'all_docs', Options).

-spec get_results(server(), ne_binary(), ne_binary(), wh_proplist()) ->
                         {'ok', wh_json:objects() | ne_binaries()} |
                         couchbeam_error().
get_results(#server{}=Conn, DbName, DesignDoc, ViewOptions) ->
    Db = get_db(Conn, DbName),
    do_fetch_results(Db, DesignDoc, ViewOptions).

%% This function assumes a "reduce" function that returns the count of docs exists
%% Need to see how to get couchbeam to return the "rows" property instead of the result
%% list; that would be better, but for not, setting the view's "reduce" to the _count
%% function will suffice (provided a reduce isn't already defined).
-spec get_results_count(server(), ne_binary(), ne_binary(), wh_proplist()) ->
                               {'ok', integer()} |
                               couchbeam_error().
get_results_count(#server{}=Conn, DbName, DesignDoc, ViewOptions) ->
    Db = get_db(Conn, DbName),
    do_fetch_results_count(Db, DesignDoc, ViewOptions).

%% Design Doc/View internal functions
-spec do_fetch_results(couchbeam_db(), ddoc(), wh_proplist()) ->
                              {'ok', wh_json:objects() | ne_binaries()} |
                              couchbeam_error().
do_fetch_results(Db, DesignDoc, Options) ->
    ?RETRY_504(
       case couchbeam_view:fetch(Db, DesignDoc, Options) of
           {'ok', JObj} -> {'ok', wh_json:get_value(<<"rows">>, JObj, JObj)};
           {'error', _T, E} -> {'error', format_error(E)};
           {'error', E} -> {'error', format_error(E)}
       end
      ).

format_error({'failure', 404}) -> 'not_found';
format_error({'http_error', {'status', 504}}) -> 'gateway_timeout';
format_error({'conn_failed', {'error', 'timeout'}}) -> 'connection_timeout';
format_error({'conn_failed', {'error', 'enetunreach'}}) -> 'network_unreachable';
format_error(E) ->
    lager:debug("unformatted error: ~p", [E]),
    E.

-spec do_fetch_results_count(couchbeam_db(), ddoc(), wh_proplist()) ->
                                    {'ok', integer() | 'undefined'} |
                                    couchbeam_error().
do_fetch_results_count(Db, DesignDoc, Options) ->
    ?RETRY_504(
       case couchbeam_view:fetch(Db, DesignDoc
                                 ,[{'reduce', 'true'}
                                   | props:delete('reduce', Options)
                                  ]
                                )
       of
           {'ok', [JObj]} ->
               {'ok', wh_json:get_integer_value(<<"value">>, JObj)};
           {'ok', JObj} ->
               case wh_json:get_integer_value(<<"total_rows">>, JObj) of
                   'undefined' -> {'ok', length(wh_json:get_value(<<"rows">>, JObj, []))};
                   N -> {'ok', N}
               end;
           {'error', _, E} -> {'error', E};
           Other -> Other
       end
      ).

-spec do_get_design_info(couchbeam_db(), ne_binary()) ->
                                {'ok', wh_json:object()} |
                                couchbeam_error().
do_get_design_info(#db{}=Db, Design) ->
    ?RETRY_504(couchbeam:design_info(Db, Design)).

%% Document related functions --------------------------------------------------

-spec open_cache_doc(server(), ne_binary(), ne_binary(), wh_proplist()) ->
                                  {'ok', wh_json:object()} |
                                  couchbeam_error().
open_cache_doc(#server{}=Conn, DbName, DocId, Options) ->
    case wh_cache:peek_local(?WH_COUCH_CACHE, {?MODULE, DbName, DocId}) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            case open_doc(Conn, DbName, DocId, Options) of
                {'error', _}=E -> E;
                {'ok', JObj}=Ok ->
                    CacheProps = [{'origin', {'db', DbName, DocId}}],
                    wh_cache:store_local(?WH_COUCH_CACHE, {?MODULE, DbName, DocId}, JObj, CacheProps),
                    Ok
            end
    end.

-spec cache_db_doc(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
cache_db_doc(DbName, DocId, Doc) ->
    wh_cache:store_local(?WH_COUCH_CACHE, {?MODULE, DbName, DocId}, Doc).

-spec flush_cache_doc(server(), ne_binary(), ne_binary(), wh_proplist()) -> 'ok'.
flush_cache_doc(#server{}=Conn, DbName, DocId, _Options) ->
    wh_cache:erase_local(?WH_COUCH_CACHE, {?MODULE, Conn, DbName, DocId}).

flush_cache_docs() -> wh_cache:flush_local(?WH_COUCH_CACHE).
flush_cache_docs(DbName) ->
    Filter = fun({?MODULE, _, DbName1, _}=K, _) when DbName1 =:= DbName ->
                     wh_cache:erase_local(?WH_COUCH_CACHE, K),
                     'true';
                (_, _) -> 'false'
             end,
    wh_cache:filter_local(?WH_COUCH_CACHE, Filter).

-spec open_doc(server(), ne_binary(), ne_binary(), wh_proplist()) ->
                      {'ok', wh_json:object()} |
                      couchbeam_error().
open_doc(#server{}=Conn, DbName, DocId, Options) ->
    Db = get_db(Conn, DbName),
    do_fetch_doc(Db, DocId, Options).

-spec save_doc(server(), ne_binary(), wh_json:object(), wh_proplist()) ->
                      {'ok', wh_json:object()} |
                      couchbeam_error().
save_doc(#server{}=Conn, DbName, Doc, Options) ->
    Db = get_db(Conn, DbName),
    do_save_doc(Db, Doc, Options).

-spec save_docs(server(), ne_binary(), wh_json:objects(), wh_proplist()) ->
                       {'ok', wh_json:objects()} |
                       couchbeam_error().
save_docs(#server{}=Conn, DbName, Docs, Options) ->
    Db = get_db(Conn, DbName),
    do_save_docs(Db, Docs, Options).

-spec lookup_doc_rev(server(), ne_binary(), ne_binary()) ->
                            {'ok', binary()} |
                            couchbeam_error().
lookup_doc_rev(#server{}=Conn, DbName, DocId) ->
    case do_fetch_rev(get_db(Conn, DbName), DocId) of
        ?NE_BINARY = Rev -> {'ok', Rev};
        {'error', _}=E -> E
    end.

-spec ensure_saved(server(), ne_binary(), wh_json:object(), wh_proplist()) ->
                          {'ok', wh_json:object()} |
                          couchbeam_error().
ensure_saved(#server{}=Conn, DbName, Doc, Opts) ->
    Db = get_db(Conn, DbName),
    do_ensure_saved(Db, Doc, Opts).

-spec del_doc(server(), ne_binary(), wh_json:object() | ne_binary()) ->
                     {'ok', wh_json:objects()} |
                     couchbeam_error().
del_doc(#server{}=Conn, DbName, DocId) when is_binary(DocId) ->
    case lookup_doc_rev(Conn, DbName, DocId) of
        {'error', _}=Err -> Err;
        {'ok', Rev} ->
            del_doc(Conn, DbName, wh_json:from_list([{<<"_id">>, DocId}
                                                     ,{<<"_rev">>, Rev}
                                                    ]))
    end;
del_doc(#server{}=Conn, DbName, Doc) ->
    Db = get_db(Conn, DbName),
    do_delete_doc(Db, Doc).

-spec del_docs(server(), ne_binary(), wh_json:objects()) ->
                      {'ok', wh_json:objects()}.
del_docs(#server{}=Conn, DbName, Doc) ->
    Db = get_db(Conn, DbName),
    do_delete_docs(Db, Doc).

%% Internal Doc functions
-spec do_delete_doc(couchbeam_db(), wh_json:object()) ->
                           {'ok', wh_json:objects()}.
do_delete_doc(#db{}=Db, Doc) ->
    do_delete_docs(Db, [Doc]).

-spec do_delete_docs(couchbeam_db(), wh_json:objects()) ->
                            {'ok', wh_json:objects()}.
do_delete_docs(#db{}=Db, Docs) ->
    do_save_docs(Db, [prepare_doc_for_del(Doc) || Doc <- Docs], []).

%% Save only the minimal amount of document needed, since couch can't fully
%% delete the doc off the node's disk
%% See https://wiki.apache.org/couchdb/FUQ, point 4 for Documents
%% and http://grokbase.com/t/couchdb/user/11cpvasem0/database-size-seems-off-even-after-compaction-runs
-spec prepare_doc_for_del(wh_json:object()) -> wh_json:object().
prepare_doc_for_del(Doc) ->
    wh_json:from_list([{<<"_id">>, wh_json:get_value(<<"_id">>, Doc)}
                       ,{<<"_rev">>, wh_json:get_value(<<"_rev">>, Doc)}
                       ,{<<"_deleted">>, 'true'}
                      ]).

-spec do_ensure_saved(couchbeam_db(), wh_json:object(), wh_proplist()) ->
                             {'ok', wh_json:object()} |
                             couchbeam_error().
do_ensure_saved(#db{}=Db, Doc, Opts) ->
    case do_save_doc(Db, Doc, Opts) of
        {'ok', _}=Saved -> Saved;
        {'error', 'conflict'} ->
            case do_fetch_rev(Db, wh_json:get_value(<<"_id">>, Doc, <<>>)) of
                ?NE_BINARY = Rev ->
                    do_ensure_saved(Db, wh_json:set_value(<<"_rev">>, Rev, Doc), Opts);
                {'error', 'not_found'} ->
                    do_ensure_saved(Db, wh_json:delete_key(<<"_rev">>, Doc), Opts)
            end;
        {'error', _}=E -> E
    end.

-spec do_fetch_rev(couchbeam_db(), ne_binary()) ->
                          ne_binary() |
                          couchbeam_error().
do_fetch_rev(#db{}=Db, DocId) -> ?RETRY_504(couchbeam:lookup_doc_rev(Db, DocId)).

-spec do_fetch_doc(couchbeam_db(), ne_binary(), wh_proplist()) ->
                          {'ok', wh_json:object()} |
                          couchbeam_error().
do_fetch_doc(#db{}=Db, DocId, Options) ->
    ?RETRY_504(couchbeam:open_doc(Db, DocId, Options)).

-spec do_save_doc(couchbeam_db(), wh_json:object() | wh_json:objects(), wh_proplist()) ->
                         {'ok', wh_json:object()} |
                         couchbeam_error().
do_save_doc(#db{}=Db, Docs, Options) when is_list(Docs) ->
    do_save_docs(Db, Docs, Options);
do_save_doc(#db{}=Db, Doc, Options) ->
    case ?RETRY_504(couchbeam:save_doc(Db, maybe_set_docid(Doc), Options)) of
        {'ok', JObj}=Ok ->
            _ = case couch_mgr:change_notice() of
                    'true' -> spawn(fun() -> publish_doc(Db, JObj) end);
                    'false' -> 'ok'
                end,
            Ok;
        Else -> Else
    end.

-spec do_save_docs(couchbeam_db(), wh_json:objects(), wh_proplist()) ->
                          {'ok', wh_json:objects()} |
                          couchbeam_error().
do_save_docs(#db{}=Db, Docs, Options) ->
    do_save_docs(Db, Docs, Options, []).

-spec maybe_set_docid(wh_json:object()) -> wh_json:object().
maybe_set_docid(Doc) ->
    case wh_json:get_value(<<"_id">>, Doc) of
        'undefined' -> wh_json:set_value(<<"_id">>, couch_mgr:get_uuid(), Doc);
        _ -> Doc
    end.

-spec do_save_docs(couchbeam_db(), wh_json:objects(), wh_proplist(), wh_json:objects()) ->
                          {'ok', wh_json:objects()} |
                          couchbeam_error().
do_save_docs(#db{}=Db, Docs, Options, Acc) ->
    case catch(lists:split(?MAX_BULK_INSERT, Docs)) of
        {'EXIT', _} ->
            case ?RETRY_504(couchbeam:save_docs(Db, [maybe_set_docid(D) || D <- Docs], Options)) of
                {'ok', Res} ->
                    JObjs = Res++Acc,
                    _ = case couch_mgr:change_notice() of
                            'true' -> publish_doc_change(Db, Docs, JObjs);
                            'false' -> 'ok'
                        end,
                    {'ok', JObjs};
                {'error', _}=E -> E
            end;
        {Save, Cont} ->
            case ?RETRY_504(couchbeam:save_docs(Db, [maybe_set_docid(D) || D <- Save], Options)) of
                {'ok', Res} -> do_save_docs(Db, Cont, Options, Res++Acc);
                {'error', _}=E -> E
            end
    end.

publish_doc_change(Db, Docs, JObjs) ->
    spawn(fun() ->
                  case lists:any(fun(Doc) -> wh_json:is_true(<<"_deleted">>, Doc) end, Docs) of
                      'true' -> publish_doc('deleted', Db, JObjs);
                      'false' -> [publish_doc(Db, JObj) || JObj <- JObjs]
                  end
          end).

%% Attachment-related functions ------------------------------------------------
-spec fetch_attachment(server(), ne_binary(), ne_binary(), ne_binary()) ->
                              {'ok', binary()} |
                              couchbeam_error().
fetch_attachment(#server{}=Conn, DbName, DocId, AName) ->
    Db = get_db(Conn, DbName),
    do_fetch_attachment(Db, DocId, AName).

-spec stream_attachment(server(), ne_binary(), ne_binary(), ne_binary(), pid()) ->
                               {'ok', reference()} |
                               couchbeam_error().
stream_attachment(#server{}=Conn, DbName, DocId, AName, Caller) ->
    do_stream_attachment(get_db(Conn, DbName), DocId, AName, Caller).

-spec put_attachment(server(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) ->
                            {'ok', wh_json:object()} |
                            couchbeam_error().
-spec put_attachment(server(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                            {'ok', wh_json:object()} |
                            couchbeam_error().
put_attachment(#server{}=Conn, DbName, DocId, AName, Contents) ->
    put_attachment(#server{}=Conn, DbName, DocId, AName, Contents, []).

put_attachment(#server{}=Conn, DbName, DocId, AName, Contents, Options) ->
    Db = get_db(Conn, DbName),
    do_put_attachment(Db, DocId, AName, Contents, maybe_add_rev(Db, DocId, Options)).

-spec delete_attachment(server(), ne_binary(), ne_binary(), ne_binary()) ->
                               {'ok', wh_json:object()} |
                               couchbeam_error().
-spec delete_attachment(server(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                               {'ok', wh_json:object()} |
                               couchbeam_error().
delete_attachment(#server{}=Conn, DbName, DocId, AName) ->
    delete_attachment(#server{}=Conn, DbName, DocId, AName, []).

delete_attachment(#server{}=Conn, DbName, DocId, AName, Options) ->
    Db = get_db(Conn, DbName),
    do_del_attachment(Db, DocId, AName,  maybe_add_rev(Db, DocId, Options)).

%% Internal Attachment-related functions ---------------------------------------
-spec do_fetch_attachment(couchbeam_db(), ne_binary(), ne_binary()) ->
                                 {'ok', binary()} |
                                 couchbeam_error().
do_fetch_attachment(#db{}=Db, DocId, AName) ->
    ?RETRY_504(couchbeam:fetch_attachment(Db, DocId, AName)).

-spec do_stream_attachment(couchbeam_db(), ne_binary(), ne_binary(), pid()) ->
                                  {'ok', reference()} |
                                  couchbeam_error().
do_stream_attachment(#db{}=Db, DocId, AName, Caller) ->
    couchbeam:stream_fetch_attachment(Db, DocId, AName, Caller).

-spec do_put_attachment(couchbeam_db(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                               {'ok', wh_json:object()} |
                               couchbeam_error().
do_put_attachment(#db{}=Db, DocId, AName, Contents, Options) ->
    ?RETRY_504(couchbeam:put_attachment(Db, DocId, AName, Contents, Options)).

-spec do_del_attachment(couchbeam_db(), ne_binary(), ne_binary(), wh_proplist()) ->
                               {'ok', wh_json:object()} |
                               couchbeam_error().
do_del_attachment(#db{}=Db, DocId, AName, Options) ->
    Doc = wh_util:to_binary(http_uri:encode(wh_util:to_list(DocId))),
    ?RETRY_504(couchbeam:delete_attachment(Db, Doc, AName, Options)).

%% Helpers for getting Couchbeam records ---------------------------------------

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% returns the #db{} record
%% @end
%%------------------------------------------------------------------------------
-spec get_db(server(), ne_binary()) -> db().
get_db(#server{}=Conn, DbName) ->
    {'ok', Db} = couchbeam:open_db(Conn, DbName),
    Db.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_rev(couchbeam_db(), ne_binary(), wh_proplist()) -> wh_proplist().
maybe_add_rev(Db, DocId, Options) ->
    case props:get_value('rev', Options) =:= 'undefined'
        andalso do_fetch_rev(Db, DocId)
    of
        ?NE_BINARY = Rev -> [{'rev', Rev} | Options];
        _Else -> Options
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Send the query function in an anon fun with arity 0; if it returns 504, retry
%% until 3 failed retries occur.
%% @end
%%------------------------------------------------------------------------------
-type retry504_ret() :: 'ok' | ne_binary() |
                        {'ok', wh_json:object() | wh_json:objects() |
                         binary() | [binary(),...] | boolean() | integer()
                        } |
                        couchbeam_error() |
                        {'error', 'timeout'}.

-spec retry504s(fun(() -> retry504_ret())) -> retry504_ret().
-spec retry504s(fun(() -> retry504_ret()), 0..3) -> retry504_ret().
retry504s(Fun) when is_function(Fun, 0) ->
    retry504s(Fun, 0).
retry504s(_Fun, 3) ->
    wh_counter:inc(<<"couch.requests.failures">>),
    lager:debug("504 retry failed"),
    {'error', 'timeout'};
retry504s(Fun, Cnt) ->
    case catch Fun() of
        {'error', {'ok', "504", _, _}} ->
            timer:sleep(100 * (Cnt+1)),
            retry504s(Fun, Cnt+1);
        {'error', {'ok', ErrCode, _Hdrs, _Body}} ->
            wh_counter:inc(<<"couch.requests.failures">>),
            {'error', wh_util:to_integer(ErrCode)};
        {'error', _Other}=E ->
            wh_counter:inc(<<"couch.requests.failures">>),
            E;
        {'ok', _Other}=OK ->
            wh_counter:inc(<<"couch.requests.successes">>),
            OK;
        {'EXIT', _E} ->
            ST = erlang:get_stacktrace(),
            lager:debug("exception running fun: ~p", [_E]),
            wh_util:log_stacktrace(ST),
            retry504s(Fun, Cnt+1);
        OK -> OK
    end.

-spec publish_doc(couchbeam_db(), wh_json:object()) -> 'ok'.
publish_doc(Db, Doc) ->
    Action = case wh_json:is_true(<<"pvt_deleted">>, Doc) of
                 'true' -> 'deleted';
                 'false' ->
                     case wh_json:get_value(<<"_rev">>, Doc) of
                         <<"1-", _/binary>> -> 'created';
                         _Else -> 'edited'
                     end
             end,
    publish_doc(Action, Db, Doc).

-spec publish_doc(wapi_conf:action(), couchbeam_db(), wh_json:object() | wh_json:objects()) -> 'ok'.
publish_doc(Action, #db{name=DbName}, Doc) -> publish_docs(Action, wh_util:to_binary(DbName), Doc).

publish_docs(Action, Db, Doc) when not is_list(Doc) ->
    publish_docs(Action, Db, [Doc]);
publish_docs(Action, Db, Docs) ->
    _ = [publish(Action, Db, Doc) || Doc <- Docs],
    'ok'.

-spec publish(wapi_conf:action(), ne_binary(), wh_json:object()) ->
                     'ok' |
                     {'error', 'pool_full' | 'poolboy_fault'}.
publish(Action, Db, Doc) ->
    case doc_id(Doc) of
        'undefined' -> 'ok';
        <<"_design/", _/binary>> = _D -> 'ok';
        Id -> publish(Action, Db, Doc, Id)
    end.

publish(Action, Db, Doc, Id) ->
    Type = doc_type(Db, Doc),
    Props =
        [{<<"ID">>, Id}
         ,{<<"Type">>, Type}
         ,{<<"Database">>, Db}
         ,{<<"Rev">>, doc_rev(Doc)}
         ,{<<"Account-ID">>, doc_acct_id(Db, Doc)}
         ,{<<"Date-Modified">>, wh_json:get_binary_value(<<"pvt_created">>, Doc)}
         ,{<<"Date-Created">>, wh_json:get_binary_value(<<"pvt_modified">>, Doc)}
         | wh_api:default_headers(<<"configuration">>, <<"doc_", (wh_util:to_binary(Action))/binary>>
                                      ,<<"whistle_couch">>, <<"1.0.0">>)
        ],
    Fun = fun(P) -> wapi_conf:publish_doc_update(Action, Db, Type, Id, P) end,
    whapps_util:amqp_pool_send(Props, Fun).

doc_rev(Doc) ->
    case wh_json:get_value(<<"_rev">>, Doc) of
        'undefined' -> wh_json:get_value(<<"rev">>, Doc);
        Rev -> Rev
    end.

doc_id(Doc) ->
    case wh_json:get_ne_value(<<"_id">>, Doc) of
        'undefined' -> wh_json:get_value(<<"id">>, Doc);
        Id -> Id
    end.

doc_type(Db, Doc) ->
    case wh_json:get_value(<<"pvt_type">>, Doc) of
        'undefined' ->
            {'ok', D} = couch_mgr:open_cache_doc(Db, doc_id(Doc)),
            wh_json:get_binary_value(<<"pvt_type">>, D, <<"undefined">>);
        T -> wh_util:to_binary(T)
    end.

doc_acct_id(Db, Doc) ->
    case wh_json:get_value(<<"pvt_account_id">>, Doc) of
        'undefined' -> wh_util:format_account_id(Db, 'raw');
        Id -> Id
    end.
