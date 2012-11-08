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
         ,load_merge/2, load_merge/3
         ,merge/3
         ,load_view/3, load_view/4
         ,load_attachment/3, load_docs/2
         ,save/1, save/2
         ,delete/1, delete/2
         ,save_attachment/4, save_attachment/5
         ,delete_attachment/3
         ,ensure_saved/1, ensure_saved/2
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
-spec load/2 :: (api_binary() | api_binaries(), cb_context:context()) -> cb_context:context().
-spec load/3 :: (api_binary() | api_binaries(), cb_context:context(), wh_proplist()) -> cb_context:context().

load(DocId, #cb_context{}=Context) ->
    load(DocId, Context, []).

load(?NE_BINARY = DocId, #cb_context{db_name=Db}=Context, Opts) ->
    case couch_mgr:open_doc(Db, DocId, Opts) of
        {error, Error} -> handle_couch_mgr_errors(Error, DocId, Context);
        {ok, JObj} ->
            lager:debug("loaded doc ~s from ~s", [DocId, Db]),
            case wh_util:is_true(wh_json:get_value(<<"pvt_deleted">>, JObj)) of
                true -> cb_context:add_system_error(bad_identifier, [{details, DocId}],  Context);
                false -> cb_context:store(db_doc, JObj, handle_couch_mgr_success(JObj, Context))
            end
    end;
load([_|_]=IDs, #cb_context{db_name=Db}=Context, Opts) ->
    Opts1 = [{keys, IDs}, include_docs | Opts],
    case couch_mgr:all_docs(Db, Opts1) of
        {error, Error} -> handle_couch_mgr_errors(Error, IDs, Context);
        {ok, JObjs} -> handle_couch_mgr_success(extract_included_docs(JObjs), Context)
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
-spec load_from_file/2 :: (ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} | {'error', atom()}.
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
-spec load_merge/2 :: (ne_binary(), cb_context:context()) -> cb_context:context().
-spec load_merge/3 :: (ne_binary(), wh_json:json_object(), cb_context:context()) -> cb_context:context().

load_merge(DocId, #cb_context{doc=DataJObj}=Context) ->
    load_merge(DocId, DataJObj, Context).

load_merge(DocId, DataJObj, #cb_context{db_name=DbName}=Context) ->
    case load(DocId, Context) of
        #cb_context{resp_status=success, doc=JObj}=Context1 ->
            lager:debug("loaded doc ~s from ~s, merging", [DocId, DbName]),
            merge(DataJObj, JObj, Context1);
        Else -> Else
    end.

-spec merge/3 :: (wh_json:json_object(), wh_json:json_object(), cb_context:context()) -> cb_context:context().
merge(DataJObj, JObj, Context) ->
    PrivJObj = wh_json:private_fields(JObj),
    handle_couch_mgr_success(wh_json:merge_jobjs(PrivJObj, DataJObj), Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to load the context with the results of a view
%% run against the accounts database.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec load_view/3 :: (ne_binary(), wh_proplist(), cb_context:context()) -> cb_context:context().
load_view(View, Options, #cb_context{db_name=Db, query_json=RJ}=Context) ->
    HasFilter = has_filter(RJ),
    ViewOptions = case HasFilter of
                      false -> Options;
                      true -> [include_docs
                               | props:delete(include_docs, Options)
                              ]
                  end,
    case couch_mgr:get_results(Db, View, ViewOptions) of
        {error, Error} -> handle_couch_mgr_errors(Error, View, Context);
        {ok, JObjs} when HasFilter ->
            lager:debug("loaded view ~s from ~s, running query filter", [View, Db]),
            Filtered = [JObj
                        || JObj <- JObjs
                               ,filter_doc(wh_json:get_value(<<"doc">>, JObj), RJ)
                       ],
            handle_couch_mgr_success(Filtered, Context);
        {ok, JObjs} ->
            lager:debug("loaded view ~s from ~s", [View, Db]),
            handle_couch_mgr_success(JObjs, Context)
    end.

%%--------------------------------------------------------------------
% @public
%% @doc
%% This function attempts to load the context with the results of a view
%% run against the accounts database.  The results are then filtered by
%% the supplied function
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-type filter_fun() :: fun((wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects()).
-spec load_view/4 :: (ne_binary(), wh_proplist(), cb_context:context(), filter_fun()) -> cb_context:context().
load_view(View, Options, Context, Filter) when is_function(Filter, 2) ->
    case load_view(View, Options, Context) of
        #cb_context{resp_status=success, doc=JObjs} = Context1 ->
            Filtered = [JObj
                        || JObj <- lists:foldl(Filter, [], JObjs)
                               ,(not wh_util:is_empty(JObj))
                       ],
            handle_couch_mgr_success(Filtered, Context1);
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
-spec load_docs/2 :: (cb_context:context(), filter_fun()) -> cb_context:context().
load_docs(#cb_context{db_name=Db}=Context, Filter) when is_function(Filter, 2) ->
    case couch_mgr:all_docs(Db) of
        {error, Error} -> handle_couch_mgr_errors(Error, <<"all_docs">>, Context);
        {ok, JObjs} ->
            Filtered = [JObj 
                        || JObj <- lists:foldl(Filter, [], JObjs)
                               ,(not wh_util:is_empty(JObj))
                       ],
            handle_couch_mgr_success(Filtered, Context)
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
-spec load_attachment/3 :: (ne_binary() | wh_json:json_object(), ne_binary(), cb_context:context()) -> cb_context:context().
load_attachment(DocId, AName, #cb_context{db_name=Db}=Context) when is_binary(DocId) ->
    case couch_mgr:fetch_attachment(Db, DocId, AName) of
        {error, Error} -> handle_couch_mgr_errors(Error, DocId, Context);
        {ok, AttachBin} ->
            lager:debug("loaded attachment ~s from doc ~s from db ~s", [AName, DocId, Db]),
            #cb_context{resp_status=success, doc=Doc} = Context1 = load(DocId, Context),
            Context1#cb_context{resp_status=success
                                ,doc=Doc
                                ,resp_data=AttachBin
                                ,resp_etag=rev_to_etag(Doc)
                               }
    end;
load_attachment(Doc, AName, #cb_context{}=Context) ->
    load_attachment(find_doc_id(Doc), AName, Context).

-spec find_doc_id/1 :: (wh_json:json_object()) -> api_binary().
find_doc_id(JObj) ->
    case wh_json:get_ne_value(<<"_id">>, JObj) of
        undefined -> wh_json:get_ne_value(<<"id">>, JObj);
        DocId -> DocId
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
-spec save/1 :: (cb_context:context()) -> cb_context:context().
-spec save/2 :: (cb_context:context(), wh_proplist()) -> cb_context:context().

save(#cb_context{}=Context) ->
    save(Context, []).

save(#cb_context{doc=[]}=Context, _Options) ->
    lager:debug("no docs to save"),
    Context#cb_context{resp_status=success};
save(#cb_context{db_name=Db, doc=[_|_]=JObjs, req_verb=Verb}=Context, Options) ->
    JObjs0 = update_pvt_parameters(JObjs, Context),
    case couch_mgr:save_docs(Db, JObjs0, Options) of
        {error, Error} -> 
            IDs = [wh_json:get_value(<<"_id">>, J) || J <- JObjs],
            handle_couch_mgr_errors(Error, IDs, Context);
        {ok, JObj1} ->
            _ = case Verb =:= <<"put">> of
                    true -> send_document_change(created, Db, JObj1, Options);
                    false -> send_document_change(edited, Db, JObj1, Options)
                end,
            handle_couch_mgr_success(JObj1, Context)
    end;
save(#cb_context{db_name=Db, doc=JObj, req_verb=Verb}=Context, Options) ->
    JObj0 = update_pvt_parameters(JObj, Context),
    case couch_mgr:save_doc(Db, JObj0, Options) of
        {error, Error} -> 
            DocId = wh_json:get_value(<<"_id">>, JObj0),
            handle_couch_mgr_errors(Error, DocId, Context);
        {ok, JObj1} ->
            _ = case Verb =:= <<"put">> of
                    true -> send_document_change(created, Db, JObj1, Options);
                    false -> send_document_change(edited, Db, JObj1, Options)
                end,
            handle_couch_mgr_success(JObj1, Context)
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
-spec ensure_saved/1 :: (cb_context:context()) -> cb_context:context().
-spec ensure_saved/2 :: (cb_context:context(), wh_proplist()) -> cb_context:context().

ensure_saved(#cb_context{}=Context) ->
    ensure_saved(Context, []).

ensure_saved(#cb_context{db_name=Db, doc=JObj, req_verb=Verb}=Context, Options) ->
    JObj0 = update_pvt_parameters(JObj, Context),
    case couch_mgr:ensure_saved(Db, JObj0, Options) of
        {error, Error} -> 
            DocId = wh_json:get_value(<<"_id">>, JObj0),
            handle_couch_mgr_errors(Error, DocId, Context);
        {ok, JObj1} ->
            _ = case Verb =:= <<"put">> of
                    true -> send_document_change(created, Db, JObj1, Options);
                    false -> send_document_change(edited, Db, JObj1, Options)
                end,
            handle_couch_mgr_success(JObj1, Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save the Contents as an attachment on the document.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec save_attachment/4 :: (ne_binary(), ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
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
-spec save_attachment/5 :: (ne_binary(), ne_binary(), ne_binary(), cb_context:context(), wh_proplist()) -> cb_context:context().
save_attachment(DocId, AName, Contents, #cb_context{db_name=Db}=Context, Options) ->
    Opts1 = case props:get_value(rev, Options) of
                undefined ->
                    {ok, Rev} = couch_mgr:lookup_doc_rev(Db, DocId),
                    [{rev, Rev} | Options];
                O -> O
            end,
    case couch_mgr:put_attachment(Db, DocId, AName, Contents, Opts1) of
        {error, Error} -> handle_couch_mgr_errors(Error, AName, Context);
        {ok, _Res} ->
            lager:debug("saved attachment ~s to doc ~s to db ~s", [AName, DocId, Db]),
            {ok, Rev1} = couch_mgr:lookup_doc_rev(Db, DocId),
            Context#cb_context{doc=wh_json:new()
                               ,resp_status=success
                               ,resp_data=wh_json:new()
                               ,resp_etag=rev_to_etag(Rev1)
                              }
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
-spec delete/1 :: (cb_context:context()) -> cb_context:context().
-spec delete/2 :: (cb_context:context(), 'permanent') -> cb_context:context().

delete(#cb_context{db_name=Db, doc=JObj}=Context) ->
    JObj0 = update_pvt_parameters(JObj, Context),
    JObj1 = wh_json:set_value(<<"pvt_deleted">>, true, JObj0),
    case couch_mgr:save_doc(Db, JObj1) of
        {error, not_found} -> handle_couch_mgr_success(wh_json:new(), Context);
        {error, Error} -> 
            DocId = wh_json:get_value(<<"_id">>, JObj1),
            handle_couch_mgr_errors(Error, DocId, Context);
        {ok, _} ->
            lager:debug("deleted ~s from ~s", [wh_json:get_value(<<"_id">>, JObj), Db]),
            _ = send_document_change(deleted, Db, JObj1),
            handle_couch_mgr_success(wh_json:new(), Context)
    end.

delete(#cb_context{db_name=Db, doc=JObj}=Context, permanent) ->
    case couch_mgr:del_doc(Db, JObj) of
        {error, not_found} -> handle_couch_mgr_success(wh_json:new(), Context);
        {error, Error} -> 
            DocId = wh_json:get_value(<<"_id">>, JObj),
            handle_couch_mgr_errors(Error, DocId, Context);
        {ok, _} ->
            lager:debug("permanently deleted ~s from ~s", [wh_json:get_value(<<"_id">>, JObj), Db]),
            _ = send_document_change(deleted, Db, wh_json:set_value(<<"pvt_deleted">>, true, JObj)),
            handle_couch_mgr_success(wh_json:new(), Context)
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
-spec delete_attachment/3 :: (ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
delete_attachment(DocId, AName, #cb_context{db_name=Db}=Context) ->
    case couch_mgr:delete_attachment(Db, DocId, AName) of
        {error, not_found} -> handle_couch_mgr_success(wh_json:new(), Context);
        {error, Error} -> handle_couch_mgr_errors(Error, AName, Context);
        {ok, _} ->
            lager:debug("deleted attachment ~s from doc ~s from ~s", [AName, DocId, Db]),
            handle_couch_mgr_success(wh_json:new(), Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will attempt to convert a revision tag on the provided
%% document into a usable ETag for the response
%% @end
%%--------------------------------------------------------------------
-spec rev_to_etag/1 :: (wh_json:json_object() | wh_json:json_objects() | ne_binary()) -> 'undefined' | 'automatic' | string().
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
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_couch_mgr_success/2 :: (wh_json:json_object() | wh_json:json_objects(), cb_context:context()) -> cb_context:context().
handle_couch_mgr_success([_|_]=JObjs, #cb_context{req_verb = <<"put">>, resp_headers=Headers}=Context) ->
    Context#cb_context{doc=JObjs
                       ,resp_status=success
                       ,resp_data=[wh_json:public_fields(JObj) 
                                   || JObj <- JObjs
                                          ,wh_json:is_false(<<"pvt_deleted">>, JObj, true)
                                  ]
                       ,resp_etag=rev_to_etag(JObjs)
                       ,resp_headers=
                           [{<<"Location">>, wh_json:get_value(<<"_id">>, JObj)}
                            || JObj <- JObjs
                           ] ++ Headers
                      };
handle_couch_mgr_success([_|_]=JObjs, Context) ->
    Context#cb_context{doc=JObjs
                       ,resp_status=success
                       ,resp_data=[wh_json:public_fields(JObj) 
                                   || JObj <- JObjs
                                          ,wh_json:is_false(<<"pvt_deleted">>, JObj, true)
                                  ]
                       ,resp_etag=rev_to_etag(JObjs)
                      };
handle_couch_mgr_success(JObj, #cb_context{req_verb = <<"put">>, resp_headers=Headers}=Context) ->
    Context#cb_context{doc=JObj
                       ,resp_status=success
                       ,resp_data=wh_json:public_fields(JObj)
                       ,resp_etag=rev_to_etag(JObj)
                       ,resp_headers=[{<<"Location">>, wh_json:get_value(<<"_id">>, JObj)} | Headers]
                      };
handle_couch_mgr_success(JObj, Context) ->
    Context#cb_context{doc=JObj
                       ,resp_status=success
                       ,resp_data=wh_json:public_fields(JObj)
                       ,resp_etag=rev_to_etag(JObj)
                      }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_couch_mgr_errors/3 :: (couch_util:couchbeam_errors(), api_binary() | api_binaries(), cb_context:context()) -> cb_context:context().
handle_couch_mgr_errors(invalid_db_name, _, #cb_context{db_name=Db}=Context) ->
    lager:debug("datastore ~s not_found", [Db]),
    cb_context:add_system_error(datastore_missing, [{details, wh_util:to_binary(Db)}], Context);
handle_couch_mgr_errors(db_not_reachable, _DocId, #cb_context{db_name=Db}=Context) ->
    lager:debug("operation on doc ~s from ~s failed: db_not_reachable", [_DocId, Db]),
    cb_context:add_system_error(datastore_unreachable, Context);
handle_couch_mgr_errors(not_found, DocId, #cb_context{db_name=Db}=Context) ->
    lager:debug("operation on doc ~s from ~s failed: not_found", [DocId, Db]),
    cb_context:add_system_error(bad_identifier, [{details, DocId}],  Context);    
handle_couch_mgr_errors(conflict, DocId, #cb_context{db_name=Db}=Context) ->
    lager:debug("failed to update doc ~s in ~s: conflicts", [DocId, Db]),
    cb_context:add_system_error(datastore_conflict, Context);
handle_couch_mgr_errors(invalid_view_name, View, #cb_context{db_name=Db}=Context) ->
    lager:debug("loading view ~s from ~s failed: invalid view", [View, Db]),
    cb_context:add_system_error(datastore_missing_view, [{details, wh_util:to_binary(View)}], Context);
handle_couch_mgr_errors(Else, _, Context) ->
    lager:debug("operation failed: ~p", [Else]),
    try wh_util:to_binary(Else) of
        Reason ->
            cb_context:add_system_error(datastore_fault, [{details, Reason}], Context)
    catch
        _:_ -> 
            cb_context:add_system_error(datastore_fault, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is used to update the private timestamps, and db
%% parameters on all crossbar documents
%% @end
%%--------------------------------------------------------------------
-spec update_pvt_parameters/2 :: (wh_json:json_object() | wh_json:json_objects(), cb_context:context()) ->
                                         wh_json:json_object() | wh_json:json_objects().
update_pvt_parameters(JObjs, Context) when is_list(JObjs) ->
    [update_pvt_parameters(JObj, Context) || JObj <- JObjs];
update_pvt_parameters(JObj0, #cb_context{db_name=DbName}) ->
    lists:foldl(fun(Fun, JObj) -> Fun(JObj, DbName) end, JObj0, ?PVT_FUNS).

add_pvt_vsn(JObj, _) ->
    case wh_json:get_value(<<"pvt_vsn">>, JObj) of
        undefined ->
            wh_json:set_value(<<"pvt_vsn">>, ?CROSSBAR_DOC_VSN, JObj);
        _ ->
            JObj
    end.

add_pvt_account_db(JObj, DbName) ->
    case wh_json:get_value(<<"pvt_account_db">>, JObj) of
        undefined ->
            wh_json:set_value(<<"pvt_account_db">>, DbName, JObj);
        _Else -> JObj
    end.

add_pvt_account_id(JObj, DbName) ->
    case wh_json:get_value(<<"pvt_account_id">>, JObj) of
        undefined ->
            wh_json:set_value(<<"pvt_account_id">>, wh_util:format_account_id(DbName, raw), JObj);
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
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_included_docs/1 :: (wh_json:json_objects()) -> wh_json:json_objects().
extract_included_docs(JObjs) ->
    [wh_json:get_value(<<"doc">>, JObj) || JObj <- JObjs].

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec send_document_change/3 :: (wapi_conf:conf_action(), ne_binary(), wh_json:json_object() | wh_json:json_objects()) -> 'ok' | pid().
-spec send_document_change/4 :: (wapi_conf:conf_action(), ne_binary(), wh_json:json_object() | wh_json:json_objects(), wh_proplist()) -> 'ok' | pid().
-spec send_document_change/5 :: (wapi_conf:conf_action(), ne_binary(), wh_json:json_object() | wh_json:json_objects(), wh_proplist(), boolean()) -> 'ok' | pid().

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

%% ADD Unit Tests for private/public field filtering and merging
