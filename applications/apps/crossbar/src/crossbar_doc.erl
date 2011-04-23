%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 26 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar_doc).

-export([load/2, load_from_file/2, load_merge/3, load_view/3, load_view/4, load_attachment/3]).
-export([save/1, delete/1, save_attachment/4, save_attachment/5, delete_attachment/3]).
-export([public_fields/1, private_fields/1, is_private_key/1]).
-export([rev_to_etag/1]).

-include("../include/crossbar.hrl").

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
load(_DocId, #cb_context{db_name=undefined}=Context) ->
    crossbar_util:response_db_missing(Context);
load(DocId, #cb_context{db_name=DB}=Context) ->
    case couch_mgr:open_doc(DB, DocId) of
        {error, db_not_reachable} ->
            crossbar_util:response_datastore_timeout(Context);
        {error, not_found} ->
            crossbar_util:response_bad_identifier(DocId, Context);
	{ok, Doc} ->
            Context#cb_context{
	      doc=Doc
	      ,resp_status=success
	      ,resp_data=public_fields(Doc)
	      ,resp_etag=rev_to_etag(Doc)
	     };
        _Else ->
            logger:format_log(error, "CB_DOC.load: Unexpected return from datastore: ~p~n", [_Else]),
            Context#cb_context{doc=[]}
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
load_merge(_DocId, _Data, #cb_context{db_name=undefined}=Context) ->
    crossbar_util:response_db_missing(Context);
load_merge(DocId, {struct, Data}, Context) ->
    case load(DocId, Context) of
        #cb_context{resp_status=success, doc=Doc}=Context1 ->
            Doc1 = {struct, private_fields(Doc) ++ Data},
            Context1#cb_context{
                 doc=Doc1
                ,resp_status=success
                ,resp_data=public_fields(Doc1)
                ,resp_etag=rev_to_etag(Doc1)
            };
        Else ->
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
load_view(_View, _Options, #cb_context{db_name=undefined}=Context) ->
    crossbar_util:response_db_missing(Context);
load_view(View, Options, #cb_context{db_name=DB}=Context) ->
    case couch_mgr:get_results(DB, View, Options) of
	{error, invalid_view_name} ->
            crossbar_util:response_missing_view(Context);
	{error, not_found} ->
	    crossbar_util:response_missing_view(Context);
	{ok, Doc} ->
            Context#cb_context{
	      doc=Doc
	      ,resp_status=success
	      ,resp_etag=rev_to_etag(Doc)
	     };
        _Else ->
            logger:format_log(error, "CB_DOC.load_view: Unexpected return from datastore: ~p~n", [_Else]),
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
-spec(load_view/4 :: (View :: binary(), Options :: proplist(), Context :: #cb_context{}, Filter :: function()) -> #cb_context{}).
load_view(View, Options, Context, Filter) ->
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
load_attachment(_, _, #cb_context{db_name=undefined}=Context) ->
    crossbar_util:response_db_missing(Context);
load_attachment(DocId, AName, #cb_context{db_name=DB}=Context) ->
    io:format("CB_DOC: load_attach: ~p ~p ~p~n", [DB, DocId, AName]),
    case couch_mgr:fetch_attachment(DB, DocId, AName) of
        {error, db_not_reachable} ->
            crossbar_util:response_datastore_timeout(Context);
	{error, not_found} ->
	    crossbar_util:response_bad_identifier(DocId, Context);
	{ok, AttachBin} ->
	    #cb_context{doc=Doc} = Context1 = load(DocId, Context),
	    logger:format_log(info, "CB_DOC.load_attach: Res: ~p~n", [AttachBin]),
            Context1#cb_context{
	      resp_status=success
	      ,doc=Doc
	      ,resp_data=AttachBin
	      ,resp_etag=rev_to_etag(Doc)
	     };
        _Else ->
            logger:format_log(error, "CB_DOC.load_attach: Unexpected return from datastore: ~p~n", [_Else]),
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
-spec(save/1 :: (Context :: #cb_context{}) -> #cb_context{}).
save(#cb_context{db_name=undefined}=Context) ->
    crossbar_util:response_db_missing(Context);
save(#cb_context{db_name=DB, doc=JObj}=Context) ->
    JObj0 = update_pvt_parameters(JObj, Context),
    case couch_mgr:save_doc(DB, JObj0) of
        {error, db_not_reachable} ->
            crossbar_util:response_datastore_timeout(Context);
	{error, conflict} ->
	    crossbar_util:response_conflicting_docs(Context);
	{ok, JObj1} when Context#cb_context.req_verb =:= <<"put">> ->
            Context#cb_context{
                 doc=JObj1
                ,resp_status=success
                ,resp_headers=[{"Location", whapps_json:get_value(<<"_id">>, JObj1)} | Context#cb_context.resp_headers]
                ,resp_data=public_fields(JObj1)
                ,resp_etag=rev_to_etag(JObj1)
            };
	{ok, JObj2} ->
            Context#cb_context{
                 doc=JObj2
                ,resp_status=success
                ,resp_data=public_fields(JObj2)
                ,resp_etag=rev_to_etag(JObj2)
            };
        _Else ->
            logger:format_log(error, "CB_DOC.save: Unexpected return from datastore: ~p~n", [_Else]),
            Context
    end.

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
save_attachment(_, _, _, #cb_context{db_name=undefined}=Context, _) ->
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
            crossbar_util:response_datastore_timeout(Context);
	{error, conflict} ->
	    crossbar_util:response_conflicting_docs(Context);
	{ok, _Res} ->
	    logger:format_log(info, "CB_DOC.save_attach Res: ~p~n", [_Res]),
	    {ok, Rev1} = couch_mgr:lookup_doc_rev(DB, DocId),
            Context#cb_context{
	      resp_status=success
	      ,resp_data=[]
	      ,resp_etag=rev_to_etag(Rev1)
            };
        _Else ->
            logger:format_log(error, "CB_DOC.save_attach: Unexpected return from datastore: ~p~n", [_Else]),
            Context
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
-spec(delete/1 :: (Context :: #cb_context{}) -> #cb_context{}).
delete(#cb_context{db_name=undefined}=Context) ->
    crossbar_util:response_db_missing(Context);
delete(#cb_context{db_name=DB, doc=JObj}=Context) ->
    JObj0 = update_pvt_parameters(JObj, Context),
    JObj1 = whapps_json:set_value(<<"pvt_deleted">>, true, JObj0),
    case couch_mgr:save_doc(DB, JObj1) of
        {error, db_not_reachable} ->
            crossbar_util:response_datastore_timeout(Context);
	{ok, _Doc} ->
	    logger:format_log(info, "CB_DOC.delete: result: ~p~n", [_Doc]),
            Context#cb_context{
	       doc=undefined
	      ,resp_status=success
	      ,resp_data=[]
	     };
        _Else ->
            logger:format_log(error, "CB_DOC.delete: Unexpected return from datastore: ~p~n", [_Else]),
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
delete_attachment(_, _, #cb_context{db_name=undefined}=Context) ->
    crossbar_util:response_db_missing(Context);
delete_attachment(DocId, AName, #cb_context{db_name=DB}=Context) ->
    case couch_mgr:delete_attachment(DB, DocId, AName) of
        {error, db_not_reachable} ->
            crossbar_util:response_datastore_timeout(Context);
	{error, not_found} ->
	    crossbar_util:response_bad_identifier(DocId, Context);
	{ok, _Res} ->
	    logger:format_log(info, "CB_DOC.del_attach Res: ~p~n", [_Res]),
	    {ok, Rev} = couch_mgr:lookup_doc_rev(DB, DocId),
            Context#cb_context{
	      resp_status=success
	      ,resp_data=[]
	      ,resp_etag=rev_to_etag(Rev)
            };
        _Else ->
            logger:format_log(error, "CB_DOC.del_attach: Unexpected return from datastore: ~p~n", [_Else]),
            Context
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will filter any private fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec(public_fields/1 :: (Json :: json_object()|json_objects()) -> json_object()|json_objects()).
public_fields([{struct, _}|_]=Json)->
    lists:map(fun public_fields/1, Json);
public_fields({struct, Json}) ->
    PubDoc =
        lists:filter(fun({K, _}) ->
                            not is_private_key(K)
                     end, Json),
    case props:get_value(<<"_id">>, Json) of
        undefined ->
            {struct, PubDoc};
        Id ->
            {struct, PubDoc ++ [{<<"id">>, Id}]}
    end;
public_fields(Json) ->
    logger:format_log(error, "Unhandled Json format in public_fields:~n~p~n", [Json]),
    Json.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will filter any public fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec(private_fields/1 :: (Json :: json_object()|json_objects()) -> json_object()|json_objects()).
private_fields([{struct, _}|_]=Json)->
    lists:map(fun public_fields/1, Json);
private_fields({struct, Json}) ->
    lists:filter(fun({K, _}) ->
                        is_private_key(K)
                 end, Json);
private_fields(Json) ->
    logger:format_log(error, "Unhandled Json format in private fields:~n~p~n", [Json]),
    Json.
    
%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will return a boolean, true if the provided key is
%% considered private; otherwise false
%% @end
%%--------------------------------------------------------------------
-spec(is_private_key/1 :: (Key :: binary()) -> boolean()).
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
-spec(rev_to_etag/1 :: (Json :: json_object()|json_objects()) -> undefined | automatic | string()).
rev_to_etag([{struct, _}|_])->
    automatic;
rev_to_etag({struct, Props}) ->
    case props:get_value([<<"_rev">>], Props) of
        Rev when is_list(Rev) ->
	    rev_to_etag(Rev);
        _Else ->
            undefined
    end;
rev_to_etag([]) -> undefined;
rev_to_etag(Rev) when is_binary(Rev) ->
    rev_to_etag(whistle_util:to_list(Rev));
rev_to_etag(ETag) when is_list(ETag) ->
    logger:format_log(error, "Etag in rev to etag: ~p~n", [ETag]),
    string:sub_string(ETag, 1, 2) ++ string:sub_string(ETag, 4);
rev_to_etag(_Json) ->
    logger:format_log(error, "Unhandled Json format in rev to etag:~n~p~n", [_Json]),
    undefined.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is used to update the private timestamps, and db
%% parameters on all crossbar documents
%% @end
%%--------------------------------------------------------------------
-spec(update_pvt_parameters/2 :: (JObj0 :: json_object(), Context :: #cb_context{}) -> json_object()).
update_pvt_parameters(JObj0, Context) ->
    Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    JObj1 = whapps_json:set_value(<<"pvt_account_db">>, Context#cb_context.db_name, JObj0),
    case whapps_json:get_value(<<"pvt_created">>, JObj1) of
        undefined ->                                    
            JObj2 = whapps_json:set_value(<<"pvt_created">>, Timestamp, JObj1),
            whapps_json:set_value(<<"pvt_modified">>, Timestamp, JObj2);
        _ -> 
            whapps_json:set_value(<<"pvt_modified">>, Timestamp, JObj0)
    end.
