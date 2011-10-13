%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 26 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar_doc).

-export([load/2, load_from_file/2, load_merge/3, load_view/3, load_view/4, load_attachment/3]).
-export([save/1, delete/1, delete/2, save_attachment/4, save_attachment/5, delete_attachment/3]).
-export([ensure_saved/1]).
-export([public_fields/1, private_fields/1, is_private_key/1]).
-export([rev_to_etag/1, current_doc_vsn/0]).

-include("../include/crossbar.hrl").

-define(CROSSBAR_DOC_VSN, <<"1">>).
-define(PVT_FUNS, [fun add_pvt_vsn/2, fun add_pvt_account_id/2, fun add_pvt_account_db/2
		   ,fun add_pvt_created/2, fun add_pvt_modified/2
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
-spec(current_doc_vsn/0 :: () -> <<_:8>>).
current_doc_vsn() ->
    ?CROSSBAR_DOC_VSN.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to load the context with account details,
%% including the account db name and the account doc.
%%
%% Failure here returns 410, 500, or 503
%% @end
%%--------------------------------------------------------------------
-spec(load/2 :: (DocId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
load(_DocId, #cb_context{db_name = <<>>}=Context) ->
    crossbar_util:response_db_missing(Context);
load(DocId, #cb_context{db_name=DB}=Context) ->
    case couch_mgr:open_doc(DB, DocId) of
        {error, db_not_reachable} ->
	    ?LOG("loading doc ~s from ~s failed: db not reachable", [DocId, DB]),
            crossbar_util:response_datastore_timeout(Context);
        {error, not_found} ->
	    ?LOG("loading doc ~s from ~s failed: doc not found", [DocId, DB]),
            crossbar_util:response_bad_identifier(DocId, Context);
	{ok, Doc} ->
	    ?LOG("loaded doc ~s from ~s", [DocId, DB]),
            case wh_util:is_true(wh_json:get_value(<<"pvt_deleted">>, Doc)) of
                true ->
                    crossbar_util:response_bad_identifier(DocId, Context);
                false ->
                    Context#cb_context{doc=Doc
                                       ,resp_status=success
                                       ,resp_data=public_fields(Doc)
                                       ,resp_etag=rev_to_etag(Doc)
                                      }
            end;
        _Else ->
	    ?LOG("Unexpected return from datastore: ~p", [_Else]),
            Context#cb_context{doc=?EMPTY_JSON_OBJECT}
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
-spec(load_from_file/2 :: (Db :: binary(), File :: binary()) -> no_return()).
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
-spec(load_merge/3 :: (DocId :: binary(), Data :: json_object(), Context :: #cb_context{}) -> #cb_context{}).
load_merge(_DocId, _Data, #cb_context{db_name = <<>>}=Context) ->
    ?LOG("db missing from #cb_context for doc ~s", [_DocId]),
    crossbar_util:response_db_missing(Context);
load_merge(DocId, DataJObj, #cb_context{db_name=DBName}=Context) ->
    case load(DocId, Context) of
        #cb_context{resp_status=success, doc=Doc}=Context1 ->
	    ?LOG("loaded doc ~s from ~s, merging", [DocId, DBName]),
	    PrivJObj = private_fields(Doc),
            Doc1 = wh_json:merge_jobjs(PrivJObj, DataJObj),
            Context1#cb_context{
	      doc=Doc1
	      ,resp_status=success
	      ,resp_data=public_fields(Doc1)
	      ,resp_etag=rev_to_etag(Doc1)
	     };
        Else ->
	    ?LOG("loading doc ~s from ~s failed unexpectedly", [DocId, DBName]),
            Else
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to load the context with the results of a view
%% run against the accounts database.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec(load_view/3 :: (View :: binary(), Options :: proplist(), Context :: #cb_context{}) -> #cb_context{}).
load_view(_View, _Options, #cb_context{db_name = <<>>}=Context) ->
    ?LOG("db missing from #cb_context for view ~s", [view_name_to_binary(_View)]),
    crossbar_util:response_db_missing(Context);
load_view(View, Options, #cb_context{db_name=DB}=Context) ->
    case couch_mgr:get_results(DB, View, Options) of
	{error, invalid_view_name} ->
	    ?LOG("loading view ~s from ~s failed: invalid view", [view_name_to_binary(View), DB]),
            crossbar_util:response_missing_view(Context);
	{error, not_found} ->
	    ?LOG("loading view ~s from ~s failed: not found", [view_name_to_binary(View), DB]),
	    crossbar_util:response_missing_view(Context);
	{ok, Doc} ->
	    ?LOG("loaded view ~s from ~s", [view_name_to_binary(View), DB]),
            Context#cb_context{
	      doc=Doc
	      ,resp_status=success
	      ,resp_etag=rev_to_etag(Doc)
	     };
        _Else ->
	    ?LOG("loading view ~s from ~s failed: unexpected ~p", [view_name_to_binary(View), DB, _Else]),
            Context#cb_context{doc=?EMPTY_JSON_OBJECT}
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
-spec load_view/4 :: (View, Options, Context, Filter) -> #cb_context{} when
      View :: binary(),
      Options :: proplist(),
      Context :: #cb_context{},
      Filter :: fun((Item :: json_object(), Acc :: json_objects()) -> json_objects()).
load_view(View, Options, Context, Filter) when is_function(Filter, 2) ->
    case load_view(View, Options, Context) of
        #cb_context{resp_status=success, doc=Doc} = Context1 ->
            Context1#cb_context{resp_data=lists:foldr(Filter, [], Doc)};
        Else ->
            Else
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
-spec(load_attachment/3 :: (DocId :: binary(), AName :: binary(), Context :: #cb_context{}) -> #cb_context{}).
load_attachment(_DocId, _AName, #cb_context{db_name = <<>>}=Context) ->
    ?LOG("loading attachment ~s from doc ~s failed: no db", [_DocId, _AName]),
    crossbar_util:response_db_missing(Context);
load_attachment(DocId, AName, #cb_context{db_name=DB}=Context) ->
    case couch_mgr:fetch_attachment(DB, DocId, AName) of
        {error, db_not_reachable} ->
	    ?LOG("loading attachment ~s from doc ~s from db ~s failed: db not reachable", [AName, DocId, DB]),
            crossbar_util:response_datastore_timeout(Context);
	{error, not_found} ->
	    ?LOG("loading attachment ~s from doc ~s from db ~s failed: attachment not found", [AName, DocId, DB]),
	    crossbar_util:response_bad_identifier(DocId, Context);
	{ok, AttachBin} ->
	    ?LOG("loaded attachment ~s from doc ~s from db ~s", [AName, DocId, DB]),
	    #cb_context{resp_status=success, doc=Doc} = Context1 = load(DocId, Context),

            Context1#cb_context{
	      resp_status=success
	      ,doc=Doc
	      ,resp_data=AttachBin
	      ,resp_etag=rev_to_etag(Doc)
	     };
        _Else ->
	    ?LOG("loading attachment ~s from doc ~s from db ~s failed: unexpected ~p", [AName, DocId, DB, _Else]),
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
-spec save/1 :: (Context) -> #cb_context{} when
      Context :: #cb_context{}.
save(#cb_context{db_name = <<>>}=Context) ->
    ?LOG("DB undefined, cannot save"),
    crossbar_util:response_db_missing(Context);
save(#cb_context{db_name=DB, doc=JObj, req_verb=Verb, resp_headers=RespHs}=Context) ->
    JObj0 = update_pvt_parameters(JObj, Context),
    case couch_mgr:save_doc(DB, JObj0) of
        {error, db_not_reachable} ->
	    ?LOG("Failed to save json: db not reachable"),
            crossbar_util:response_datastore_timeout(Context);
	{error, conflict} ->
	    ?LOG("Failed to save json: conflicts with existing doc"),
	    crossbar_util:response_conflicting_docs(Context);
	{ok, JObj1} when Verb =:= <<"put">> ->
	    ?LOG("Saved a put request, setting location headers"),
	    send_document_change(<<"created">>, DB, JObj1),
            Context#cb_context{
                 doc=JObj1
                ,resp_status=success
                ,resp_headers=[{"Location", wh_json:get_value(<<"_id">>, JObj1)} | RespHs]
                ,resp_data=public_fields(JObj1)
                ,resp_etag=rev_to_etag(JObj1)
            };
	{ok, JObj2} ->
	    ?LOG("Saved json doc"),
	    send_document_change(<<"edited">>, DB, JObj2),
            Context#cb_context{
                 doc=JObj2
                ,resp_status=success
                ,resp_data=public_fields(JObj2)
                ,resp_etag=rev_to_etag(JObj2)
            };
        _Else ->
            ?LOG("Save failed: unexpected return from datastore: ~p", [_Else]),
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
-spec ensure_saved/1 :: (Context) -> #cb_context{} when
      Context :: #cb_context{}.
ensure_saved(#cb_context{db_name = <<>>}=Context) ->
    ?LOG("DB undefined, cannot ensure save"),
    crossbar_util:response_db_missing(Context);
ensure_saved(#cb_context{db_name=DB, doc=JObj, req_verb=Verb, resp_headers=RespHs}=Context) ->
    JObj0 = update_pvt_parameters(JObj, Context),
    case couch_mgr:ensure_saved(DB, JObj0) of
        {error, db_not_reachable} ->
	    ?LOG("Failed to save json: db not reachable"),
            crossbar_util:response_datastore_timeout(Context);
	{ok, JObj1} when Verb =:= <<"put">> ->
	    ?LOG("Saved a put request, setting location headers"),
	    send_document_change(<<"created">>, DB, JObj1),
            Context#cb_context{
                 doc=JObj1
                ,resp_status=success
                ,resp_headers=[{"Location", wh_json:get_value(<<"_id">>, JObj1)} | RespHs]
                ,resp_data=public_fields(JObj1)
                ,resp_etag=rev_to_etag(JObj1)
            };
	{ok, JObj2} ->
	    ?LOG("Saved json doc"),
	    send_document_change(<<"edited">>, DB, JObj2),
            Context#cb_context{
                 doc=JObj2
                ,resp_status=success
                ,resp_data=public_fields(JObj2)
                ,resp_etag=rev_to_etag(JObj2)
            };
        _Else ->
            ?LOG("Save failed: unexpected return from datastore: ~p", [_Else]),
            Context
    end.

-spec send_document_change/3 :: (Action, Db, Doc) -> pid() when
      Action :: ne_binary(), %% <<"created">> | <<"edited">> | <<"deleted">>
      Db :: ne_binary(),
      Doc :: json_object().
send_document_change(Action, Db, Doc) ->
    spawn(fun() ->
                  Id = wh_json:get_value(<<"_id">>, Doc),
                  Type = wh_json:get_binary_value(<<"pvt_type">>, Doc, <<"undefined">>),
		  Change = wh_json:from_list([
					      {<<"ID">>, Id}
					      ,{<<"Rev">>, wh_json:get_value(<<"_rev">>, Doc)}
					      ,{<<"Doc">>, public_fields(Doc)}
					      ,{<<"Type">>, Type}
					      ,{<<"Account-DB">>, wh_json:get_value(<<"pvt_account_db">>, Doc)}
					      ,{<<"Account-ID">>, wh_json:get_value(<<"pvt_account_id">>, Doc)}
					      ,{<<"Date-Modified">>, wh_json:get_binary_value(<<"pvt_created">>, Doc)}
					      ,{<<"Date-Created">>, wh_json:get_binary_value(<<"pvt_modified">>, Doc)}
					      ,{<<"Version">>, wh_json:get_binary_value(<<"pvt_vsn">>, Doc)}
					      | wh_api:default_headers(<<>>, <<"configuration">>, <<"doc_", Action/binary>>, ?APP_NAME, ?APP_VSN)
					     ]),
		  ?LOG("publishing configuration document_change event for ~s, type: ~s", [Id, Type]),
		  {ok, Payload} = wh_api:document_change(Change),
		  amqp_util:document_change_publish(Action, Db, Type, Id, Payload)
	  end).
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save the Contents as an attachment on the document.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec(save_attachment/4 :: (DocId :: binary(), AName :: binary(), Contents :: binary(), Context :: #cb_context{}) -> #cb_context{}).
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
-spec(save_attachment/5 :: (DocId :: binary(), AName :: binary(), Contents :: binary(), Context :: #cb_context{}, Options :: proplist()) -> #cb_context{}).
save_attachment(_DocId, _AName, _, #cb_context{db_name = <<>>}=Context, _) ->
    ?LOG("Saving attachment ~s to doc ~s failed: no db specified", [_AName, _DocId]),
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
	    ?LOG("Saving attachment ~s to doc ~s to db ~s failed: db not reachable", [AName, DocId, DB]),
            crossbar_util:response_datastore_timeout(Context);
	{error, conflict} ->
	    ?LOG("Saving attachment ~s to doc ~s to db ~s failed: conflict", [AName, DocId, DB]),
	    crossbar_util:response_conflicting_docs(Context);
	{ok, _Res} ->
	    ?LOG("Saved attachment ~s to doc ~s to db ~s", [AName, DocId, DB]),
	    {ok, Rev1} = couch_mgr:lookup_doc_rev(DB, DocId),
            Context#cb_context{
	      resp_status=success
	      ,resp_data=[]
	      ,resp_etag=rev_to_etag(Rev1)
            };
        _Else ->
	    ?LOG("Saving attachment ~s to doc ~s to db ~s failed: unexpected: ~p", [AName, DocId, DB, _Else]),
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
-spec delete/1 :: (Context) -> #cb_context{} when
      Context :: #cb_context{}.
-spec delete/2 :: (Context, Switch) -> #cb_context{} when
      Context :: #cb_context{},
      Switch :: permanent.

delete(#cb_context{db_name = <<>>, doc=JObj}=Context) ->
    ?LOG("deleting ~s failed, no db", [wh_json:get_value(<<"_id">>, JObj)]),
    crossbar_util:response_db_missing(Context);
delete(#cb_context{db_name=DB, doc=JObj}=Context) ->
    JObj0 = update_pvt_parameters(JObj, Context),
    JObj1 = wh_json:set_value(<<"pvt_deleted">>, true, JObj0),
    case couch_mgr:save_doc(DB, JObj1) of
        {error, db_not_reachable} ->
	    ?LOG("deleting ~s from ~s failed, db not reachable", [wh_json:get_value(<<"_id">>, JObj), DB]),
            crossbar_util:response_datastore_timeout(Context);
	{ok, _Doc} ->
	    ?LOG("deleted ~s from ~s", [wh_json:get_value(<<"_id">>, JObj), DB]),
	    send_document_change(<<"deleted">>, DB, JObj1),
            Context#cb_context{
	       doc = ?EMPTY_JSON_OBJECT
	      ,resp_status=success
	      ,resp_data=[]
	     };
        _Else ->
	    ?LOG("deleting ~s from ~s failed: unexpected ~p", [wh_json:get_value(<<"_id">>, JObj), DB, _Else]),
            Context
    end.

delete(#cb_context{db_name = <<>>, doc=JObj}=Context, permanent) ->
    ?LOG("permanent deleting ~s failed, no db", [wh_json:get_value(<<"_id">>, JObj)]),
    crossbar_util:response_db_missing(Context);
delete(#cb_context{db_name=DB, doc=JObj}=Context, permanent) ->
    case couch_mgr:del_doc(DB, JObj) of
        {error, db_not_reachable} ->
	    ?LOG("permanent deleting ~s from ~s failed, db not reachable", [wh_json:get_value(<<"_id">>, JObj), DB]),
            crossbar_util:response_datastore_timeout(Context);
	{ok, _Doc} ->
	    ?LOG("permanently deleted ~s from ~s", [wh_json:get_value(<<"_id">>, JObj), DB]),
            Context#cb_context{
	       doc = ?EMPTY_JSON_OBJECT
	      ,resp_status=success
	      ,resp_data=[]
	     };
        _Else ->
	    ?LOG("permanently deleting ~s from ~s failed: unexpected ~p", [wh_json:get_value(<<"_id">>, JObj), DB, _Else]),
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
-spec(delete_attachment/3 :: (DocId :: binary(), AName :: binary(), Context :: #cb_context{}) -> #cb_context{}).
delete_attachment(_DocId, _AName, #cb_context{db_name = <<>>}=Context) ->
    ?LOG("deleting attachment ~s from doc ~s failed: no db", [_AName, _DocId]),
    crossbar_util:response_db_missing(Context);
delete_attachment(DocId, AName, #cb_context{db_name=DB}=Context) ->
    case couch_mgr:delete_attachment(DB, DocId, AName) of
        {error, db_not_reachable} ->
	    ?LOG("deleting attachment ~s from doc ~s from ~s failed: db not reachable", [AName, DocId, DB]),
            crossbar_util:response_datastore_timeout(Context);
	{error, not_found} ->
	    ?LOG("deleting attachment ~s from doc ~s from ~s failed: not found", [AName, DocId, DB]),
	    crossbar_util:response_bad_identifier(DocId, Context);
	{ok, _Res} ->
	    ?LOG("deleted attachment ~s from doc ~s from ~s", [AName, DocId, DB]),
	    {ok, Rev} = couch_mgr:lookup_doc_rev(DB, DocId),
            Context#cb_context{
	      resp_status=success
	      ,resp_data=[]
	      ,resp_etag=rev_to_etag(Rev)
            };
        _Else ->
	    ?LOG("deleting attachment ~s from doc ~s from ~s failed: unexpected ~p", [AName, DocId, DB, _Else]),
            Context
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will filter any private fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec public_fields/1 :: (json_object() | json_objects()) -> json_object() | json_objects().
public_fields([_|_]=JObjs)->
    lists:map(fun public_fields/1, JObjs);
public_fields(JObj) ->
    PubJObj = wh_json:from_list([ KV || {K, _}=KV <- wh_json:to_proplist(JObj), not is_private_key(K)]),
    case wh_json:get_value(<<"_id">>, JObj) of
        undefined -> PubJObj;
        Id -> wh_json:set_value(<<"id">>, Id, PubJObj)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will filter any public fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec private_fields/1 :: (json_object() | json_objects()) -> json_object() | json_objects().
private_fields([_|_]=JObjs)->
    lists:map(fun public_fields/1, JObjs);
private_fields(JObj) ->
    wh_json:from_list([ KV || {K,_}=KV <- wh_json:to_proplist(JObj), is_private_key(K)]).

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
-spec rev_to_etag/1 :: (json_object() | json_objects()) -> 'undefined' | 'automatic' | string().
rev_to_etag([_|_])-> automatic;
rev_to_etag([]) -> undefined;
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
-spec update_pvt_parameters/2 :: (json_object(), #cb_context{}) -> json_object().
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
    wh_json:set_value(<<"pvt_account_db">>, DBName, JObj).

add_pvt_account_id(JObj, DBName) ->
    wh_json:set_value(<<"pvt_account_id">>, whapps_util:get_db_name(DBName, raw), JObj).

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
%% Standardize the view data structure as a binary for use in the
%% logs
%% @end
%%--------------------------------------------------------------------
-spec view_name_to_binary/1 :: ({binary() | string(), binary() | string()} | binary() | string()) -> binary().
view_name_to_binary(View) when is_binary(View) -> View;
view_name_to_binary({Cat, View}) -> list_to_binary([Cat, "/", View]);
view_name_to_binary(View) -> wh_util:to_binary(View).

%% ADD Unit Tests for private/public field filtering and merging
