%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(crossbar_doc).

-export([load/2, load/3
         ,load_from_file/2
         ,load_merge/3, merge/3
         ,load_view/3, load_view/4
         ,load_attachment/3, load_docs/2
         ,save/1, save/2
         ,delete/1, delete/2
         ,save_attachment/4, save_attachment/5
         ,delete_attachment/3
         ,ensure_saved/1, ensure_saved/2
         ,public_fields/1
         ,private_fields/1, is_private_key/1
         ,rev_to_etag/1
         ,current_doc_vsn/0
         ,update_pvt_parameters/2
        ]).

-include("include/crossbar.hrl").

-define(CROSSBAR_DOC_VSN, <<"1">>).
-define(PVT_FUNS, [fun add_pvt_vsn/2
                   ,fun add_pvt_account_id/2
                   ,fun add_pvt_account_db/2
                   ,fun add_pvt_created/2
                   ,fun add_pvt_modified/2
                  ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns the version number attached to created/updated documents.
%% Indicates what pvt fields are created/updated when saving.
%%
%% Failure here returns 410, 500, or 503
%% @end
%%--------------------------------------------------------------------
-spec current_doc_vsn/0 :: () -> ne_binary().
current_doc_vsn() -> ?CROSSBAR_DOC_VSN.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to load the context with account details,
%% including the account db name and the account doc.
%%
%% Failure here returns 410, 500, or 503
%% @end
%%--------------------------------------------------------------------
-spec load/2 :: (ne_binary() | [ne_binary(),...], #cb_context{}) -> #cb_context{}.
-spec load/3 :: (ne_binary() | [ne_binary(),...], #cb_context{}, wh_proplist()) -> #cb_context{}.
load(_DocId, #cb_context{db_name=undefined}=Context) ->
    crossbar_util:response_db_missing(Context);
load(DocId, #cb_context{}=Context) ->
    load(DocId, Context, []).

load(_DocId, #cb_context{db_name=undefined}=Context, _Opts) ->
    crossbar_util:response_db_missing(Context);
load(?NE_BINARY = DocId, #cb_context{db_name=DB}=Context, Opts) ->
    case couch_mgr:open_doc(DB, DocId, Opts) of
        {error, db_not_reachable} ->
            lager:debug("loading doc ~s from ~s failed: db not reachable", [DocId, DB]),
            crossbar_util:response_datastore_timeout(Context);
        {error, not_found} ->
            lager:debug("loading doc ~s from ~s failed: doc not found", [DocId, DB]),
            crossbar_util:response_bad_identifier(DocId, Context);
        {ok, Doc} ->
            lager:debug("loaded doc ~s from ~s", [DocId, DB]),
            case wh_util:is_true(wh_json:get_value(<<"pvt_deleted">>, Doc)) of
                true -> crossbar_util:response_bad_identifier(DocId, Context);
                false ->
                    Context1 = Context#cb_context{doc=Doc
                                                  ,resp_status=success
                                                  ,resp_data=public_fields(Doc)
                                                  ,resp_etag=rev_to_etag(Doc)
                                                 },
                    cb_context:store(db_doc, Doc, Context1)
            end
    end;
load([_|_]=IDs, #cb_context{db_name=DB}=Context, Opts) ->
    Opts1 = [{keys, IDs}, include_docs | Opts],

    case couch_mgr:all_docs(DB, Opts1) of
        {error, db_not_reachable} ->
            lager:debug("loading docs from ~s failed: db not reachable", [DB]),
            crossbar_util:response_datastore_timeout(Context);
        {ok, Vs} ->
            lager:debug("loaded docs from ~s", [DB]),
            Context#cb_context{
              doc=[wh_json:get_value(<<"doc">>, V)
                   || V <- Vs,
                      wh_json:is_json_object(V),
                      %% if unset, default to true (undeleted); if set, it better be false
                      wh_json:is_false([<<"doc">>, <<"pvt_deleted">>], V, true)
                  ]
              ,resp_status=success
             }
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% "Load" is a poor word choice given the other function names...
%% This function creates a crossbar document from the contents of
%% a file.
%%
%% Failure here returns 410, 500, or 503
%% @end
%%--------------------------------------------------------------------
-spec load_from_file/2 :: (ne_binary(), ne_binary()) ->
                                  {'ok', wh_json:json_object()} |
                                  {'error', atom()}.
load_from_file(Db, File) ->
    couch_mgr:load_doc_from_file(Db, crossbar, File).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to merge the submitted data with the private
%% fields of an existing account document, if successful it will
%% load the context with the account details
%%
%% Failure here returns 410, 500, or 503
%% @end
%%--------------------------------------------------------------------
-spec load_merge/3 :: (ne_binary(), wh_json:json_object(), #cb_context{}) -> #cb_context{}.
load_merge(_DocId, _Data, #cb_context{db_name=undefined}=Context) ->
    lager:debug("db missing from #cb_context for doc ~s", [_DocId]),
    crossbar_util:response_db_missing(Context);
load_merge(DocId, DataJObj, #cb_context{db_name=DBName}=Context) ->
    case load(DocId, Context) of
        #cb_context{resp_status=success, doc=Doc}=Context1 ->
            lager:debug("loaded doc ~s from ~s, merging", [DocId, DBName]),
            merge(DataJObj, Doc, Context1);
        Else ->
            lager:debug("loading doc ~s from ~s failed unexpectedly", [DocId, DBName]),
            Else
    end.

-spec merge/3 :: (wh_json:json_object(), wh_json:json_object(), #cb_context{}) -> #cb_context{}.
merge(DataJObj, Doc, Context) ->
    PrivJObj = private_fields(Doc),
    Doc1 = wh_json:merge_jobjs(PrivJObj, DataJObj),
    Context#cb_context{doc=Doc1
                       ,resp_status=success
                       ,resp_data=public_fields(Doc1)
                       ,resp_etag=rev_to_etag(Doc1)
                      }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to load the context with the results of a view
%% run against the accounts database.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec load_view/3 :: (ne_binary(), proplist(), #cb_context{}) -> #cb_context{}.
load_view(_View, _Options, #cb_context{db_name=undefined}=Context) ->
    lager:debug("db missing from #cb_context for view ~s", [_View]),
    crossbar_util:response_db_missing(Context);
load_view(View, Options, #cb_context{db_name=DB, query_json=RJ}=Context) ->
    HasFilter = has_filter(RJ),
    ViewOptions = case HasFilter of
                      false -> Options;
                      true -> [include_docs
                               | props:delete(include_docs, Options)
                              ]
                  end,
    case couch_mgr:get_results(DB, View, ViewOptions) of
        {error, invalid_view_name} ->
            lager:debug("loading view ~s from ~s failed: invalid view", [View, DB]),
            crossbar_util:response_missing_view(Context);
        {error, not_found} ->
            lager:debug("loading view ~s from ~s failed: not found", [View, DB]),
            crossbar_util:response_missing_view(Context);
        {ok, Docs} when HasFilter ->
            lager:debug("loaded view ~s from ~s, running query filter", [View, DB]),
            Filtered = [Doc || Doc <- Docs,
                               Doc =/= undefined,
                               filter_doc(wh_json:get_value(<<"doc">>, Doc), RJ)
                       ],
            Context#cb_context{
              doc=Filtered
              ,resp_status=success
              ,resp_etag=rev_to_etag(Filtered)
             };
        {ok, Docs} ->
            lager:debug("loaded view ~s from ~s", [View, DB]),
            Context#cb_context{
              doc=Docs
              ,resp_status=success
              ,resp_etag=rev_to_etag(Docs)
             };
        _Else ->
            lager:debug("loading view ~s from ~s failed: unexpected ~p", [View, DB, _Else]),
            Context#cb_context{doc=[]}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to load the context with the results of a view
%% run against the accounts database.  The results are then filtered by
%% the supplied function
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-type filter_fun() :: fun((wh_json:json_object(), wh_json:json_objects()) ->
                                 wh_json:json_objects()).
-spec load_view/4 :: (ne_binary(), proplist(), #cb_context{}, filter_fun()) -> #cb_context{}.
load_view(View, Options, Context, Filter) when is_function(Filter, 2) ->
    case load_view(View, Options, Context) of
        #cb_context{resp_status=success, doc=Doc} = Context1 ->
            Context1#cb_context{resp_data=
                                    [D || D <- lists:foldl(Filter, [], Doc),
                                          D =/= undefined
                                    ]
                               };
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to load the context with the results of all the
%% docs in the supplied Db, with the fold function weeding out those not
%% desired by returning 'undefined' or not adding it to the Accumulator
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec load_docs/2 :: (#cb_context{}, filter_fun()) -> #cb_context{}.
load_docs(#cb_context{db_name=Db}=Context, Filter) when is_function(Filter, 2) ->
    case couch_mgr:all_docs(Db) of
        {error, db_not_reachable} ->
            crossbar_util:response_datastore_timeout(Context);
        {error, not_found} ->
            crossbar_util:response_db_missing(Context);
        {ok, Docs} ->
            Context#cb_context{resp_status=success
                               ,resp_data=[D || D <- lists:foldl(Filter, [], Docs),
                                                D =/= undefined
                                          ]
                              };
        _Else ->
            Context
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to load the context with the binary payload
%% stored as an attachment
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec load_attachment/3 :: (ne_binary() | wh_json:json_object(), ne_binary(), #cb_context{}) ->
                                   #cb_context{}.
load_attachment(_DocId, _AName, #cb_context{db_name = undefined}=Context) ->
    lager:debug("loading attachment ~s from doc ~s failed: no db", [_DocId, _AName]),
    crossbar_util:response_db_missing(Context);
load_attachment(DocId, AName, #cb_context{db_name=DB}=Context) when is_binary(DocId) ->
    case couch_mgr:fetch_attachment(DB, DocId, AName) of
        {error, db_not_reachable} ->
            lager:debug("loading attachment ~s from doc ~s from db ~s failed: db not reachable", [AName, DocId, DB]),
            crossbar_util:response_datastore_timeout(Context);
        {error, not_found} ->
            lager:debug("loading attachment ~s from doc ~s from db ~s failed: attachment not found", [AName, DocId, DB]),
            crossbar_util:response_bad_identifier(DocId, Context);
        {ok, AttachBin} ->
            lager:debug("loaded attachment ~s from doc ~s from db ~s", [AName, DocId, DB]),
            #cb_context{resp_status=success, doc=Doc} = Context1 = load(DocId, Context),
            Context1#cb_context{resp_status=success
                                ,doc=Doc
                                ,resp_data=AttachBin
                                ,resp_etag=rev_to_etag(Doc)
                               };
        _Else ->
            lager:debug("loading attachment ~s from doc ~s from db ~s failed: unexpected ~p", [AName, DocId, DB, _Else]),
            Context
    end;
load_attachment(Doc, AName, #cb_context{}=Context) ->
    load_attachment(find_doc_id(Doc), AName, Context).

-spec find_doc_id/1 :: (wh_json:json_object()) -> api_binary().
find_doc_id(Doc) ->
    find_doc_id(Doc, wh_json:get_value(<<"_id">>, Doc)).
find_doc_id(Doc, undefined) ->
    wh_json:get_value(<<"id">>, Doc);
find_doc_id(_Doc, Id) -> Id.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to save the provided document to the accounts
%% database. The result is loaded into the context record.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec save/1 :: (#cb_context{}) -> #cb_context{}.
-spec save/2 :: (#cb_context{}, proplist()) -> #cb_context{}.
save(#cb_context{}=Context) ->
    save(Context, []).

save(#cb_context{db_name=undefined}=Context, _) ->
    lager:debug("db undefined, cannot save"),
    crossbar_util:response_db_missing(Context);
save(#cb_context{db_name=DB, doc=[_|_]=JObjs, req_verb=Verb, resp_headers=RespHs}=Context, Options) ->
    JObjs0 = update_pvt_parameters(JObjs, Context),
    case couch_mgr:save_docs(DB, JObjs0, Options) of
        {error, db_not_reachable} ->
            lager:debug("failed to save json: db not reachable"),
            crossbar_util:response_datastore_timeout(Context);
        {ok, JObjs1} when Verb =:= <<"put">> ->
            _ = send_document_change(created, DB, JObjs1, Options),
            Context#cb_context{doc=JObjs1
                               ,resp_status=success
                               ,resp_headers=
                                   [{<<"Location">>, wh_json:get_value(<<"_id">>, JObj1)}
                                    || JObj1 <- JObjs
                                   ] ++ RespHs
                               ,resp_data=[public_fields(JObj1) || JObj1 <- JObjs]
                               ,resp_etag=rev_to_etag(JObjs1)
                              };
        {ok, JObjs2} ->
            _ = send_document_change(edited, DB, JObjs2, Options),
            Context#cb_context{doc=JObjs2
                               ,resp_status=success
                               ,resp_data=[public_fields(JObj2) || JObj2 <- JObjs]
                               ,resp_etag=rev_to_etag(JObjs2)
                              }
    end;
save(#cb_context{doc=[]}=Context, _Options) ->
    lager:debug("no docs to save"),
    Context#cb_context{resp_status=success};
save(#cb_context{db_name=DB, doc=JObj, req_verb=Verb, resp_headers=RespHs}=Context, Options) ->
    JObj0 = update_pvt_parameters(JObj, Context),
    case couch_mgr:save_doc(DB, JObj0, Options) of
        {error, db_not_reachable} ->
            lager:debug("failed to save json: db not reachable"),
            crossbar_util:response_datastore_timeout(Context);
        {error, conflict} ->
            lager:debug("failed to save json: conflicts with existing doc"),
            crossbar_util:response_conflicting_docs(Context);
        {ok, JObj1} when Verb =:= <<"put">> ->
            _ = send_document_change(created, DB, JObj1, Options),
            Context#cb_context{doc=JObj1
                               ,resp_status=success
                               ,resp_headers=[{<<"Location">>, wh_json:get_value(<<"_id">>, JObj1)} | RespHs]
                               ,resp_data=public_fields(JObj1)
                               ,resp_etag=rev_to_etag(JObj1)
                              };
        {ok, JObj2} ->
            _ = send_document_change(edited, DB, JObj2, Options),
            Context#cb_context{doc=JObj2
                               ,resp_status=success
                               ,resp_data=public_fields(JObj2)
                               ,resp_etag=rev_to_etag(JObj2)
                              };
        _Else ->
            lager:debug("save failed: unexpected return from datastore: ~p", [_Else]),
            Context
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to save the provided document to the accounts
%% database. The result is loaded into the context record.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec ensure_saved/1 :: (#cb_context{}) -> #cb_context{}.
-spec ensure_saved/2 :: (#cb_context{}, proplist()) -> #cb_context{}.
ensure_saved(#cb_context{}=Context) ->
    ensure_saved(Context, []).

ensure_saved(#cb_context{db_name=undefined}=Context, _) ->
    lager:debug("db undefined, cannot ensure save"),
    crossbar_util:response_db_missing(Context);
ensure_saved(#cb_context{db_name=DB, doc=JObj, req_verb=Verb, resp_headers=RespHs}=Context, Options) ->
    JObj0 = update_pvt_parameters(JObj, Context),
    case couch_mgr:ensure_saved(DB, JObj0, Options) of
        {error, db_not_reachable} ->
            lager:debug("failed to save json: db not reachable"),
            crossbar_util:response_datastore_timeout(Context);
        {ok, JObj1} when Verb =:= <<"put">> ->
            lager:debug("saved a put request, setting location headers"),
            _ = send_document_change(created, DB, JObj1, Options),
            Context#cb_context{doc=JObj1
                               ,resp_status=success
                               ,resp_headers=[{<<"Location">>, wh_json:get_value(<<"_id">>, JObj1)} | RespHs]
                               ,resp_data=public_fields(JObj1)
                               ,resp_etag=rev_to_etag(JObj1)
                              };
        {ok, JObj2} ->
            lager:debug("saved json doc"),
            _ = send_document_change(edited, DB, JObj2, Options),
            Context#cb_context{doc=JObj2
                               ,resp_status=success
                               ,resp_data=public_fields(JObj2)
                               ,resp_etag=rev_to_etag(JObj2)
                              };
        _Else ->
            lager:debug("save failed: unexpected return from datastore: ~p", [_Else]),
            Context
    end.

-spec send_document_change/3 :: (wapi_conf:conf_action()
                                 ,ne_binary()
                                 ,wh_json:json_object() | wh_json:json_objects()
                                ) -> 'ok' | pid().
-spec send_document_change/4 :: (wapi_conf:conf_action()
                                 ,ne_binary()
                                 ,wh_json:json_object() | wh_json:json_objects()
                                 ,wh_proplist()
                                ) -> 'ok' | pid().
-spec send_document_change/5 :: (wapi_conf:conf_action()
                                 ,ne_binary()
                                 ,wh_json:json_object() | wh_json:json_objects()
                                 ,wh_proplist()
                                 ,boolean()
                                ) -> 'ok' | pid().
send_document_change(Action, Db, Docs) ->
    send_document_change(Action, Db, Docs, []).

send_document_change(Action, Db, Docs, Options) when is_list(Docs) ->
    [send_document_change(Action, Db, Doc, Options) || Doc <- Docs];
send_document_change(Action, Db, Doc, Options) ->
    send_document_change(Action, Db, Doc, Options, props:get_value(publish_doc, Options, true)).

send_document_change(_,_,_,_,false) -> ok;
send_document_change(Action, Db, Doc, _Options, true) ->
    CallID = get(callid),
    spawn(fun() ->
                  put(callid, CallID),
                  case wh_json:get_value(<<"_id">>, Doc) of
                      undefined ->
                          Id = wh_json:get_value(<<"id">>, Doc),
                          case wh_json:get_value(<<"error">>, Doc) of
                              undefined ->
                                  {ok, Doc1} = couch_mgr:open_doc(Db, Id),
                                  publish_doc(Action, Db, Doc1, Id);
                              _E -> ok
                          end;
                      Id -> publish_doc(Action, Db, Doc, Id)
                  end
          end).

publish_doc(Action, Db, Doc, Id) ->
    ActionBin = wh_util:to_binary(Action),
    Type = wh_json:get_binary_value(<<"pvt_type">>, Doc, <<"undefined">>),
    Change =
        [{<<"ID">>, Id}
         ,{<<"Rev">>, wh_json:get_value(<<"_rev">>, Doc)}
         ,{<<"Doc">>, Doc}
         ,{<<"Type">>, Type}
         ,{<<"Account-DB">>, wh_json:get_value(<<"pvt_account_db">>, Doc)}
         ,{<<"Account-ID">>, wh_json:get_value(<<"pvt_account_id">>, Doc)}
         ,{<<"Date-Modified">>, wh_json:get_binary_value(<<"pvt_created">>, Doc)}
         ,{<<"Date-Created">>, wh_json:get_binary_value(<<"pvt_modified">>, Doc)}
         ,{<<"Version">>, wh_json:get_binary_value(<<"pvt_vsn">>, Doc)}
         | wh_api:default_headers(<<"configuration">>, <<"doc_", ActionBin/binary>>, ?APP_NAME, ?APP_VERSION)
        ],
    wapi_conf:publish_doc_update(Action, Db, Type, Id, Change).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save the Contents as an attachment on the document.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec save_attachment/4 :: (ne_binary(), ne_binary(), ne_binary(), #cb_context{}) -> #cb_context{}.
save_attachment(DocId, AName, Contents, Context) ->
    save_attachment(DocId, AName, Contents, Context, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save the Contents as an attachment on the document with options
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec save_attachment/5 :: (ne_binary(), ne_binary(), ne_binary(), #cb_context{}, proplist()) -> #cb_context{}.
save_attachment(_DocId, _AName, _, #cb_context{db_name=undefined}=Context, _) ->
    lager:debug("saving attachment ~s to doc ~s failed: no db specified", [_AName, _DocId]),
    crossbar_util:response_db_missing(Context);
save_attachment(DocId, AName, Contents, #cb_context{db_name=DB}=Context, Options) ->
    Opts1 = case props:get_value(rev, Options) of
                undefined ->
                    {ok, Rev} = couch_mgr:lookup_doc_rev(DB, DocId),
                    [{rev, Rev} | Options];
                O -> O
            end,
    case couch_mgr:put_attachment(DB, DocId, AName, Contents, Opts1) of
        {error, db_not_reachable} ->
            lager:debug("saving attachment ~s to doc ~s to db ~s failed: db not reachable", [AName, DocId, DB]),
            crossbar_util:response_datastore_timeout(Context);
        {error, conflict} ->
            lager:debug("saving attachment ~s to doc ~s to db ~s failed: conflict", [AName, DocId, DB]),
            crossbar_util:response_conflicting_docs(Context);
        {ok, _Res} ->
            lager:debug("saved attachment ~s to doc ~s to db ~s", [AName, DocId, DB]),
            {ok, Rev1} = couch_mgr:lookup_doc_rev(DB, DocId),
            Context#cb_context{resp_status=success
                               ,resp_data=[]
                               ,resp_etag=rev_to_etag(Rev1)
                              };
        _Else ->
            lager:debug("saving attachment ~s to doc ~s to db ~s failed: unexpected: ~p", [AName, DocId, DB, _Else]),
            crossbar_util:response_datastore_conn_refused(Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will attempt to remove an account document from the
%% account database.  This is preformed as a soft-delete and enforced
%% by the views.  Clean up process remove old data based on the delete
%% flag and last modified date
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec delete/1 :: (#cb_context{}) -> #cb_context{}.
-spec delete/2 :: (#cb_context{}, 'permanent') -> #cb_context{}.
delete(#cb_context{db_name=undefined, doc=JObj}=Context) ->
    lager:debug("deleting ~s failed, no db", [wh_json:get_value(<<"_id">>, JObj)]),
    crossbar_util:response_db_missing(Context);
delete(#cb_context{db_name=DB, doc=JObj}=Context) ->
    JObj0 = update_pvt_parameters(JObj, Context),
    JObj1 = wh_json:set_value(<<"pvt_deleted">>, true, JObj0),
    case couch_mgr:save_doc(DB, JObj1) of
        {error, db_not_reachable} ->
            lager:debug("deleting ~s from ~s failed, db not reachable", [wh_json:get_value(<<"_id">>, JObj), DB]),
            crossbar_util:response_datastore_timeout(Context);
        {ok, _Doc} ->
            lager:debug("deleted ~s from ~s", [wh_json:get_value(<<"_id">>, JObj), DB]),
            _ = send_document_change(deleted, DB, JObj1),
            Context#cb_context{doc = wh_json:new()
                               ,resp_status=success
                               ,resp_data=[]
                              };
        _Else ->
            lager:debug("deleting ~s from ~s failed: unexpected ~p", [wh_json:get_value(<<"_id">>, JObj), DB, _Else]),
            Context
    end.

delete(#cb_context{db_name=undefined, doc=JObj}=Context, permanent) ->
    lager:debug("permanent deleting ~s failed, no db", [wh_json:get_value(<<"_id">>, JObj)]),
    crossbar_util:response_db_missing(Context);
delete(#cb_context{db_name=DB, doc=JObj}=Context, permanent) ->
    case couch_mgr:del_doc(DB, JObj) of
        {error, db_not_reachable} ->
            lager:debug("permanent deleting ~s from ~s failed, db not reachable", [wh_json:get_value(<<"_id">>, JObj), DB]),
            crossbar_util:response_datastore_timeout(Context);
        {ok, _Doc} ->
            lager:debug("permanently deleted ~s from ~s", [wh_json:get_value(<<"_id">>, JObj), DB]),
            _ = send_document_change(deleted, DB, wh_json:set_value(<<"pvt_deleted">>, true, JObj)),
            Context#cb_context{doc = wh_json:new()
                               ,resp_status=success
                               ,resp_data=[]
                              };
        _Else ->
            lager:debug("permanently deleting ~s from ~s failed: unexpected ~p", [wh_json:get_value(<<"_id">>, JObj), DB, _Else]),
            Context
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will attempt to remove an attachment from a document.
%% Unlike the delete function, this is NOT a soft-delete.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec delete_attachment/3 :: (ne_binary(), ne_binary(), #cb_context{}) -> #cb_context{}.
delete_attachment(_DocId, _AName, #cb_context{db_name=undefined}=Context) ->
    lager:debug("deleting attachment ~s from doc ~s failed: no db", [_AName, _DocId]),
    crossbar_util:response_db_missing(Context);
delete_attachment(DocId, AName, #cb_context{db_name=DB}=Context) ->
    case couch_mgr:delete_attachment(DB, DocId, AName) of
        {error, db_not_reachable} ->
            lager:debug("deleting attachment ~s from doc ~s from ~s failed: db not reachable", [AName, DocId, DB]),
            crossbar_util:response_datastore_timeout(Context);
        {error, not_found} ->
            lager:debug("deleting attachment ~s from doc ~s from ~s failed: not found", [AName, DocId, DB]),
            crossbar_util:response_bad_identifier(DocId, Context);
        {ok, _Res} ->
            lager:debug("deleted attachment ~s from doc ~s from ~s", [AName, DocId, DB]),
            {ok, Rev} = couch_mgr:lookup_doc_rev(DB, DocId),
            Context#cb_context{resp_status=success
                               ,resp_data=[]
                               ,resp_etag=rev_to_etag(Rev)
                              };
        _Else ->
            lager:debug("deleting attachment ~s from doc ~s from ~s failed: unexpected ~p", [AName, DocId, DB, _Else]),
            Context
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will filter any private fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec public_fields/1 :: (wh_json:json_object() | wh_json:json_objects()) ->
                                 wh_json:json_object() | wh_json:json_objects().
public_fields(JObjs)->
    wh_json:public_fields(JObjs).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will filter any public fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec private_fields/1 :: (wh_json:json_object() | wh_json:json_objects()) ->
                                  wh_json:json_object() | wh_json:json_objects().
private_fields(JObjs)->
    wh_json:private_fields(JObjs).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will return a boolean, true if the provided key is
%% considered private; otherwise false
%% @end
%%--------------------------------------------------------------------
-spec is_private_key/1 :: (binary()) -> boolean().
is_private_key(<<"_", _/binary>>) -> true;
is_private_key(<<"pvt_", _/binary>>) -> true;
is_private_key(_) -> false.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will attempt to convert a revision tag on the provided
%% document into a usable ETag for the response
%% @end
%%--------------------------------------------------------------------
-spec rev_to_etag/1 :: (wh_json:json_object() | wh_json:json_objects() | ne_binary()) ->
                               'undefined' |
                               'automatic' |
                               string().
rev_to_etag([_|_])-> automatic;
rev_to_etag([]) -> undefined;
rev_to_etag(?NE_BINARY = Rev) -> wh_util:to_list(Rev);
rev_to_etag(JObj) ->
    case wh_json:get_value(<<"_rev">>, JObj) of
        undefined -> undefined;
        Rev -> wh_util:to_list(Rev)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is used to update the private timestamps, and db
%% parameters on all crossbar documents
%% @end
%%--------------------------------------------------------------------
-spec update_pvt_parameters/2 :: (wh_json:json_object() | wh_json:json_objects(), #cb_context{}) ->
                                         wh_json:json_object() | wh_json:json_objects().
update_pvt_parameters(JObjs, Context) when is_list(JObjs) ->
    [update_pvt_parameters(JObj, Context) || JObj <- JObjs];
update_pvt_parameters(JObj0, #cb_context{db_name=DBName}) ->
    lists:foldl(fun(Fun, JObj) -> Fun(JObj, DBName) end, JObj0, ?PVT_FUNS).

add_pvt_vsn(JObj, _) ->
    case wh_json:get_value(<<"pvt_vsn">>, JObj) of
        undefined ->
            wh_json:set_value(<<"pvt_vsn">>, ?CROSSBAR_DOC_VSN, JObj);
        _ ->
            JObj
    end.

add_pvt_account_db(JObj, DBName) ->
    case wh_json:get_value(<<"pvt_account_db">>, JObj) of
        undefined ->
            wh_json:set_value(<<"pvt_account_db">>, DBName, JObj);
        _Else -> JObj
    end.

add_pvt_account_id(JObj, DBName) ->
    case wh_json:get_value(<<"pvt_account_id">>, JObj) of
        undefined ->
            wh_json:set_value(<<"pvt_account_id">>, wh_util:format_account_id(DBName, raw), JObj);
        _Else -> JObj
    end.

add_pvt_created(JObj, _) ->
    case wh_json:get_value(<<"_rev">>, JObj) of
        undefined ->
            Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            wh_json:set_value(<<"pvt_created">>, Timestamp, JObj);
        _ ->
            JObj
    end.

add_pvt_modified(JObj, _) ->
    Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    wh_json:set_value(<<"pvt_modified">>, Timestamp, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a context or query parameter json object determines if the
%% request has a filter defined
%% @end
%%--------------------------------------------------------------------
-spec has_filter/1 :: (wh_json:json_object()) -> boolean().
has_filter(JObj) ->
    lists:any(fun is_filter_key/1, wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a proplist element from the query string, determines if it
%% represents a filter parameter
%% @end
%%--------------------------------------------------------------------
-spec is_filter_key/1 :: ({binary(), term()}) -> boolean().
is_filter_key({<<"filter_", _/binary>>, _}) -> true;
is_filter_key({<<"created_from">>, _}) -> true;
is_filter_key({<<"created_to">>, _}) -> true;
is_filter_key({<<"modified_from">>, _}) -> true;
is_filter_key({<<"modified_to">>, _}) -> true;
is_filter_key(_) -> false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if all of the requested props are found, false if one is not found
%% @end
%%--------------------------------------------------------------------
-spec filter_doc/2 :: (wh_json:json_object(), wh_json:json_object()) -> boolean().
filter_doc(Doc, Query) ->
    lists:all(fun({K, V}) -> filter_prop(Doc, K, V) end, wh_json:to_proplist(Query)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true or false if the prop is found inside the doc
%% @end
%%--------------------------------------------------------------------
-spec filter_prop/3 :: (wh_json:json_object(), ne_binary(), term()) -> boolean().
filter_prop(Doc, <<"filter_not_", Key/binary>>, Val) ->
    not (wh_json:get_binary_value(binary:split(Key, <<".">>), Doc, <<>>) =:= wh_util:to_binary(Val));
filter_prop(Doc, <<"filter_", Key/binary>>, Val) ->
    wh_json:get_binary_value(binary:split(Key, <<".">>), Doc, <<>>) =:= wh_util:to_binary(Val);
filter_prop(Doc, <<"has_key">>, Key) ->
    wh_json:get_value(binary:split(Key, <<".">>), Doc) =/= undefined;
filter_prop(Doc, <<"has_value">>, Key) ->
    wh_json:get_ne_value(binary:split(Key, <<".">>), Doc) =/= undefined;
filter_prop(Doc, <<"created_from">>, Val) ->
    wh_util:to_integer(wh_json:get_value(<<"pvt_created">>, Doc)) >= wh_util:to_integer(Val);
filter_prop(Doc, <<"created_to">>, Val) ->
    wh_util:to_integer(wh_json:get_value(<<"pvt_created">>, Doc)) =< wh_util:to_integer(Val);
filter_prop(Doc, <<"modified_from">>, Val) ->
    wh_util:to_integer(wh_json:get_value(<<"pvt_modified">>, Doc)) >= wh_util:to_integer(Val);
filter_prop(Doc, <<"modified_to">>, Val) ->
    wh_util:to_integer(wh_json:get_value(<<"pvt_modified">>, Doc)) =< wh_util:to_integer(Val);
filter_prop(_, _, _) ->
    true.

%% ADD Unit Tests for private/public field filtering and merging
