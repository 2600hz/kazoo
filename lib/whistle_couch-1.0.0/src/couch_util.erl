%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP, INC
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
-export_type([db_create_options/0]).

%%------------------------------------------------------------------------------
%% @public
%% @doc How many documents are chunked when doing a bulk save
%% @end
%%------------------------------------------------------------------------------
-spec max_bulk_insert/0 :: () -> ?MAX_BULK_INSERT.
max_bulk_insert() ->
    ?MAX_BULK_INSERT.

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_new_connection/4 :: (nonempty_string(), pos_integer(), string(), string()) -> server().
get_new_connection(Host, Port, "", "") ->
    get_new_conn(Host, Port, ?IBROWSE_OPTS);
get_new_connection(Host, Port, User, Pass) ->
    get_new_conn(Host, Port, [{basic_auth, {User, Pass}} | ?IBROWSE_OPTS]).

-spec get_new_conn/3 :: (nonempty_string(), pos_integer(), wh_proplist()) -> server().
get_new_conn(Host, Port, Opts) ->
    Conn = couchbeam:server_connection(Host, Port, "", Opts),
    lager:info("new connection to host ~s:~b, testing: ~p", [Host, Port, Conn]),
    {'ok', ConnData} = couchbeam:server_info(Conn),

    CouchVersion = wh_json:get_value(<<"version">>, ConnData),
    BigCouchVersion = wh_json:get_value(<<"bigcouch">>, ConnData),
    lager:info("connected successfully to ~s:~b", [Host, Port]),
    lager:info("responding CouchDB version: ~s", [CouchVersion]),
    is_binary(BigCouchVersion) andalso lager:info("responding BigCouch version: ~s", [BigCouchVersion]),

    Conn.

-spec server_url/1 :: (server()) -> ne_binary().
server_url(#server{host=Host, port=Port, options=Options}) ->
    UserPass = case props:get_value(basic_auth, Options) of
                   undefined -> <<>>;
                   {U, P} -> list_to_binary([U, <<":">>, P])
               end,
    Protocol = case wh_util:is_true(props:get_value(is_ssl, Options)) of
                   false -> <<"http">>;
                   true -> <<"https">>
               end,

    list_to_binary([Protocol, <<"://">>, UserPass
                    ,<<"@">>, wh_util:to_binary(Host)
                    ,<<":">>, wh_util:to_binary(Port)
                    ,<<"/">>
                   ]).

-spec db_url/2 :: (server(), ne_binary()) -> ne_binary().
db_url(#server{}=Conn, DbName) ->
    Server = server_url(Conn),
    list_to_binary([Server, DbName]).

%%% DB-related functions ---------------------------------------------
-spec db_compact/2 :: (server(), ne_binary()) -> boolean().
db_compact(#server{}=Conn, DbName) ->
    Db = get_db(Conn, DbName),
    do_db_compact(Db).

-spec db_create/2 :: (server(), ne_binary()) -> boolean().
db_create(#server{}=Conn, DbName) ->
    db_create(Conn, DbName, []).

-spec db_create/3 :: (server(), ne_binary(), db_create_options()) -> boolean().
db_create(#server{}=Conn, DbName, Options) ->
    case couchbeam:create_db(Conn, wh_util:to_list(DbName), [], Options) of
        {'error', _} -> false;
        {'ok', _} -> true
    end.

-spec db_delete/2 :: (server(), ne_binary()) -> boolean().
db_delete(#server{}=Conn, DbName) ->
    case couchbeam:delete_db(Conn, wh_util:to_list(DbName)) of
        {'error', _} -> false;
        {'ok', _} -> true
    end.

-spec db_replicate/2 :: (server(), wh_json:json_object() | wh_proplist()) ->
                                {'ok', wh_json:json_object()} |
                                couchbeam_error().
db_replicate(#server{}=Conn, Prop) when is_list(Prop) ->
    couchbeam:replicate(Conn, wh_json:from_list(Prop));
db_replicate(#server{}=Conn, JObj) ->
    couchbeam:replicate(Conn, JObj).

-spec db_view_cleanup/2 :: (server(), ne_binary()) -> boolean().
db_view_cleanup(#server{}=Conn, DbName) ->
    Db = get_db(Conn, DbName),
    do_db_view_cleanup(Db).

-spec db_info/1 :: (server()) ->
                           {'ok', [ne_binary(),...] | []} |
                           couchbeam_error().
db_info(#server{}=Conn) ->
    ?RETRY_504(couchbeam:all_dbs(Conn)).

-spec db_info/2 :: (server(), ne_binary()) ->
                           {'ok', wh_json:json_object()} |
                           couchbeam_error().
db_info(#server{}=Conn, DbName) ->
    ?RETRY_504(couchbeam:db_info(get_db(Conn, DbName))).

-spec db_exists/2 :: (server(), ne_binary()) -> boolean().
db_exists(#server{}=Conn, DbName) ->
    couchbeam:db_exists(Conn, wh_util:to_list(DbName)).

%% Internal DB-related functions -----------------------------------------------

-spec do_db_compact/1 :: (db()) -> boolean().
do_db_compact(#db{}=Db) ->
    Resp = ?RETRY_504(couchbeam:compact(Db)),
    Resp =:= 'ok'.

-spec do_db_view_cleanup/1 :: (db()) -> boolean().
do_db_view_cleanup(#db{}=Db) ->
    Resp = ?RETRY_504(couchbeam:view_cleanup(Db)),
    Resp =:= 'ok'.

%%% View-related functions -----------------------------------------------------
-spec design_compact/3 :: (server(), ne_binary(), ne_binary()) -> boolean().
design_compact(#server{}=Conn, DbName, Design) ->
    case couchbeam:compact(get_db(Conn, DbName), Design) of
        {'error', _E} -> false;
        ok -> true
    end.

-spec design_info/3 :: (server(), ne_binary(), ne_binary()) ->
                               {'ok', wh_json:json_object()} |
                               couchbeam_error().
design_info(#server{}=Conn, DBName, Design) ->
    Db = get_db(Conn, DBName),
    do_get_design_info(Db, Design).

-spec all_design_docs/3 :: (server(), ne_binary(), wh_proplist()) ->
                                   {'ok', wh_json:json_objects()} |
                                   couchbeam_error().
all_design_docs(#server{}=Conn, DBName, Options) ->
    Db = get_db(Conn, DBName),
    do_fetch_results(Db, 'design_docs', Options).

-spec all_docs/3 :: (server(), ne_binary(), wh_proplist()) ->
                            {'ok', wh_json:json_objects()} |
                            couchbeam_error().
all_docs(#server{}=Conn, DbName, Options) ->
    Db = get_db(Conn, DbName),
    do_fetch_results(Db, 'all_docs', Options).

-spec get_results/4 :: (server(), ne_binary(), ne_binary(), wh_proplist()) ->
                               {'ok', wh_json:json_objects() | [ne_binary(),...]} |
                               couchbeam_error().
get_results(#server{}=Conn, DbName, DesignDoc, ViewOptions) ->
    Db = get_db(Conn, DbName),
    do_fetch_results(Db, DesignDoc, ViewOptions).

%% Design Doc/View internal functions

-spec do_fetch_results/3 :: (db(), ne_binary() | 'all_docs' | 'design_docs', wh_proplist()) ->
                                    {'ok', wh_json:json_objects() | [ne_binary(),...]} |
                                    couchbeam_error().
do_fetch_results(Db, DesignDoc, Options) ->
    ?RETRY_504(case couchbeam_view:fetch(Db, DesignDoc, Options) of
                   {'ok', JObj} -> {'ok', wh_json:get_value(<<"rows">>, JObj, JObj)};
                   Other -> Other
               end
              ).

-spec do_get_design_info/2 :: (db(), ne_binary()) ->
                                      {'ok', wh_json:json_object()} |
                                      couchbeam_error().
do_get_design_info(#db{}=Db, Design) ->
    ?RETRY_504(couchbeam:design_info(Db, Design)).

%% Document related functions --------------------------------------------------

-spec open_cache_doc/4 :: (server(), ne_binary(), ne_binary(), wh_proplist()) ->
                                  {'ok', wh_json:json_object()} |
                                  couchbeam_error().
open_cache_doc(#server{}=Conn, DbName, DocId, Options) ->
    case wh_cache:peek({?MODULE, Conn, DbName, DocId}) of
        {ok, _}=Ok -> Ok;
        {error, not_found} ->
            case open_doc(Conn, DbName, DocId, Options) of
                {error, _}=E -> E;
                {ok, JObj}=Ok ->
                    wh_cache:store({?MODULE, Conn, DbName, DocId}, JObj, 900),
                    Ok
            end
    end.

-spec open_doc/4 :: (server(), ne_binary(), ne_binary(), wh_proplist()) ->
                            {'ok', wh_json:json_object()} |
                            couchbeam_error().
open_doc(#server{}=Conn, DbName, DocId, Options) ->
    Db = get_db(Conn, DbName),
    do_fetch_doc(Db, DocId, Options).

-spec save_doc/4 :: (server(), ne_binary(), wh_json:json_object(), wh_proplist()) ->
                            {'ok', wh_json:json_object()} |
                            couchbeam_error().
save_doc(#server{}=Conn, DbName, Doc, Options) ->
    Db = get_db(Conn, DbName),
    do_save_doc(Db, Doc, Options).

-spec save_docs/4 :: (server(), ne_binary(), wh_json:json_objects(), wh_proplist()) ->
                             {'ok', wh_json:json_objects()} |
                             couchbeam_error().
save_docs(#server{}=Conn, DbName, Docs, Options) ->
    Db = get_db(Conn, DbName),
    do_save_docs(Db, Docs, Options).

-spec lookup_doc_rev/3 :: (server(), ne_binary(), ne_binary()) ->
                                  {'ok', binary()} |
                                  couchbeam_error().
lookup_doc_rev(#server{}=Conn, DbName, DocId) ->
    case do_fetch_rev(get_db(Conn, DbName), DocId) of
        ?NE_BINARY = Rev -> {ok, Rev};
        {error, _}=E -> E
    end.

-spec ensure_saved/4 :: (server(), ne_binary(), wh_json:json_object(), wh_proplist()) ->
                                {'ok', wh_json:json_object()} |
                                couchbeam_error().
ensure_saved(#server{}=Conn, DbName, Doc, Opts) ->
    Db = get_db(Conn, DbName),
    do_ensure_saved(Db, Doc, Opts).

-spec del_doc/3 :: (server(), ne_binary(), wh_json:json_object() | ne_binary()) ->
                           {'ok', wh_json:json_objects()} |
                           couchbeam_error().
del_doc(#server{}=Conn, DbName, DocId) when is_binary(DocId) ->
    case lookup_doc_rev(Conn, DbName, DocId) of
        {'error', _}=Err -> Err;
        {'ok', Rev} ->
            del_doc(Conn, DbName, wh_json:from_list([{<<"_id">>, DocId}, {<<"_rev">>, Rev}]))
    end;
del_doc(#server{}=Conn, DbName, Doc) ->
    Db = get_db(Conn, DbName),
    do_delete_doc(Db, Doc).

-spec del_docs/3 :: (server(), ne_binary(), wh_json:json_objects()) ->
                            {'ok', wh_json:json_objects()}.
del_docs(#server{}=Conn, DbName, Doc) ->
    Db = get_db(Conn, DbName),
    do_delete_docs(Db, Doc).

%% Internal Doc functions
-spec do_delete_doc/2 :: (db(), wh_json:json_object()) ->
                                 {'ok', wh_json:json_objects()}.
do_delete_doc(#db{}=Db, Doc) ->
    do_delete_docs(Db, [Doc]).

-spec do_delete_docs/2 :: (db(), wh_json:json_objects()) ->
                                  {'ok', wh_json:json_objects()}.
do_delete_docs(#db{}=Db, Docs) ->
    do_save_docs(Db, [wh_json:set_value(<<"_deleted">>, true, Doc) || Doc <- Docs], []).

-spec do_ensure_saved/3 :: (db(), wh_json:json_object(), wh_proplist()) ->
                                   {'ok', wh_json:json_object()} |
                                   couchbeam_error().
do_ensure_saved(#db{}=Db, Doc, Opts) ->
    case do_save_doc(Db, Doc, Opts) of
        {'ok', _}=Saved -> lager:debug("saved doc"), Saved;
        {'error', conflict} ->
            case do_fetch_rev(Db, wh_json:get_value(<<"_id">>, Doc, <<>>)) of
                ?NE_BINARY = Rev ->
                    do_ensure_saved(Db, wh_json:set_value(<<"_rev">>, Rev, Doc), Opts);
                {'error', not_found} ->
                    do_ensure_saved(Db, wh_json:delete_key(<<"_rev">>, Doc), Opts)
            end;
        {'error', _}=E -> E
    end.

-spec do_fetch_rev/2 :: (db(), ne_binary()) ->
                                ne_binary() |
                                couchbeam_error().
do_fetch_rev(#db{}=Db, DocId) ->
    ?RETRY_504(couchbeam:lookup_doc_rev(Db, DocId)).

-spec do_fetch_doc/3 :: (db(), ne_binary(), wh_proplist()) ->
                                {'ok', wh_json:json_object()} |
                                couchbeam_error().
do_fetch_doc(#db{}=Db, DocId, Options) ->
    ?RETRY_504(couchbeam:open_doc(Db, DocId, Options)).

-spec do_save_doc/3 :: (db(), wh_json:json_object() | wh_json:json_objects(), wh_proplist()) ->
                               {'ok', wh_json:json_object()} |
                               couchbeam_error().
do_save_doc(#db{}=Db, Docs, Options) when is_list(Docs) ->
    do_save_docs(Db, Docs, Options);
do_save_doc(#db{}=Db, Doc, Options) ->
    ?RETRY_504(couchbeam:save_doc(Db, maybe_set_docid(Doc), Options)).

-spec do_save_docs/3 :: (db(), wh_json:json_objects(), wh_proplist()) ->
                                {'ok', wh_json:json_objects()} |
                                couchbeam_error().
do_save_docs(#db{}=Db, Docs, Options) ->
    do_save_docs(Db, Docs, Options, []).

-spec maybe_set_docid/1 :: (wh_json:json_object()) -> wh_json:json_object().
maybe_set_docid(Doc) ->
    case wh_json:get_value(<<"_id">>, Doc) of
        undefined -> wh_json:set_value(<<"_id">>, couch_mgr:get_uuid(), Doc);
        _ -> Doc
    end.

-spec do_save_docs/4 :: (db(), wh_json:json_objects(), wh_proplist(), wh_json:json_objects()) ->
                                {'ok', wh_json:json_objects()} |
                                couchbeam_error().
do_save_docs(#db{}=Db, Docs, Options, Acc) ->
    case catch(lists:split(?MAX_BULK_INSERT, Docs)) of
        {'EXIT', _} ->
            case ?RETRY_504(couchbeam:save_docs(Db, [maybe_set_docid(D) || D <- Docs], Options)) of
                {ok, Res} -> {ok, Res++Acc};
                {error, _}=E -> E
            end;
        {Save, Cont} ->
            case ?RETRY_504(couchbeam:save_docs(Db, [maybe_set_docid(D) || D <- Save], Options)) of
                {ok, Res} -> do_save_docs(Db, Cont, Options, Res++Acc);
                {error, _}=E -> E
            end
    end.

%% Attachment-related functions ------------------------------------------------
-spec fetch_attachment/4 :: (server(), ne_binary(), ne_binary(), ne_binary()) ->
                                    {'ok', binary()} |
                                    couchbeam_error().
fetch_attachment(#server{}=Conn, DbName, DocId, AName) ->
    Db = get_db(Conn, DbName),
    do_fetch_attachment(Db, DocId, AName).

-spec stream_attachment/5 :: (server(), ne_binary(), ne_binary(), ne_binary(), pid()) ->
                                     {'ok', reference()} |
                                     couchbeam_error().
stream_attachment(#server{}=Conn, DbName, DocId, AName, Caller) ->
    do_stream_attachment(get_db(Conn, DbName), DocId, AName, Caller).

-spec put_attachment/5 :: (server(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) ->
                                  {'ok', wh_json:json_object()} |
                                  couchbeam_error().
-spec put_attachment/6 :: (server(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                                  {'ok', wh_json:json_object()} |
                                  couchbeam_error().
put_attachment(#server{}=Conn, DbName, DocId, AName, Contents) ->
    Db = get_db(Conn, DbName),
    Rev = do_fetch_rev(Db, DocId),
    do_put_attachment(Db, DocId, AName, Contents, [{<<"rev">>, Rev}]).
put_attachment(#server{}=Conn, DbName, DocId, AName, Contents, Options) ->
    Db = get_db(Conn, DbName),
    case props:get_value(rev, Options) of
        undefined ->
            Rev = do_fetch_rev(Db, DocId),
            do_put_attachment(Db, DocId, AName, Contents, [{<<"rev">>, Rev} | Options]);
        _ ->
            do_put_attachment(Db, DocId, AName, Contents, Options)
    end.

-spec delete_attachment/4 :: (server(), ne_binary(), ne_binary(), ne_binary()) ->
                                     {'ok', wh_json:json_object()} |
                                     couchbeam_error().
-spec delete_attachment/5 :: (server(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                                     {'ok', wh_json:json_object()} |
                                     couchbeam_error().
delete_attachment(#server{}=Conn, DbName, DocId, AName) ->
    Db = get_db(Conn, DbName),
    case do_fetch_rev(Db, DocId) of
        {'error', _}=E -> E;
        ?NE_BINARY = Rev -> do_del_attachment(Db, DocId, AName, [{<<"rev">>, Rev}])
    end.

delete_attachment(#server{}=Conn, DbName, DocId, AName, Options) ->
    Db = get_db(Conn, DbName),
    case props:get_value(rev, Options) of
        undefined ->
            case do_fetch_rev(Db, DocId) of
                {'error', _}=E -> E;
                Rev -> do_del_attachment(Db, DocId, AName, [{<<"rev">>, Rev} | Options])
            end;
        _ ->
            do_del_attachment(Db, DocId, AName, Options)
    end.

%% Internal Attachment-related functions ---------------------------------------
-spec do_fetch_attachment/3 :: (db(), ne_binary(), ne_binary()) ->
                                       {'ok', binary()} |
                                       couchbeam_error().
do_fetch_attachment(#db{}=Db, DocId, AName) ->
    ?RETRY_504(couchbeam:fetch_attachment(Db, DocId, AName)).

-spec do_stream_attachment/4 :: (db(), ne_binary(), ne_binary(), pid()) ->
                                        {'ok', reference()} |
                                        couchbeam_error().
do_stream_attachment(#db{}=Db, DocId, AName, Caller) ->
    couchbeam:stream_fetch_attachment(Db, DocId, AName, Caller).

-spec do_put_attachment/5 :: (db(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                                     {'ok', wh_json:json_object()} |
                                     couchbeam_error().
do_put_attachment(#db{}=Db, DocId, AName, Contents, Options) ->
    ?RETRY_504(couchbeam:put_attachment(Db, DocId, AName, Contents, Options)).

-spec do_del_attachment/4 :: (db(), ne_binary(), ne_binary(), wh_proplist()) ->
                                     {'ok', wh_json:json_object()} |
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
-spec get_db/2 :: (server(), ne_binary()) -> db().
get_db(#server{}=Conn, DbName) ->
    {'ok', Db} = couchbeam:open_db(Conn, DbName),
    Db.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Send the query function in an anon fun with arity 0; if it returns 504, retry
%% until 3 failed retries occur.
%% @end
%%------------------------------------------------------------------------------
-type retry504_ret() :: 'ok' | ne_binary() |
                        {'ok', wh_json:json_object() | wh_json:json_objects() |
                         binary() | [binary(),...] | boolean()} |
                        couchbeam_error() |
                        {'error', 'timeout'}.

-spec retry504s/1 :: (fun(() -> retry504_ret())) -> retry504_ret().
-spec retry504s/2 :: (fun(() -> retry504_ret()), 0..3) -> retry504_ret().
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
            _ = [lager:debug("st: ~p", [S]) || S <- ST],
            retry504s(Fun, Cnt+1);
        OK -> OK
    end.
