%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 26 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar_doc).

-export([load/2, load_from_file/2, load_merge/3, load_view/3, load_view/4]).
-export([save/1, delete/1]).
-export([public_fields/1, private_fields/1, is_private_key/1]).
-export([rev_to_etag/1]).

-include("crossbar.hrl").

-import(logger, [format_log/3]).

%%--------------------------------------------------------------------
%% @private
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
            format_log(error, "CB_DOC.load: Unexpected return from datastore: ~p~n", [_Else]),
            Context#cb_context{doc=[]}
    end.

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
-spec(load_view/3 :: (View :: tuple(string(), string()), Options :: proplist(), Context :: #cb_context{}) -> #cb_context{}).
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
            format_log(error, "CB_DOC.load_view: Unexpected return from datastore: ~p~n", [_Else]),
            Context#cb_context{doc=[]}
    end.

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
%% This function attempts to save the provided document to the accounts
%% database. The result is loaded into the context record.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec(save/1 :: (Context :: #cb_context{}) -> #cb_context{}).
save(#cb_context{db_name=undefined}=Context) ->
    crossbar_util:response_db_missing(Context);
save(#cb_context{db_name=DB, doc=Doc}=Context) ->
    case couch_mgr:save_doc(DB, Doc) of
        {error, db_not_reachable} ->
            crossbar_util:response_datastore_timeout(Context);
	{ok, Doc1} ->
            Context#cb_context{
                 doc=Doc1
                ,resp_status=success
                ,resp_data=public_fields(Doc1)
                ,resp_etag=rev_to_etag(Doc1)
            };
        _Else ->
            format_log(error, "CB_DOC.save: Unexpected return from datastore: ~p~n", [_Else]),
            Context
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will attempt to remove an account document from the
%% account database
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec(delete/1 :: (Context :: #cb_context{}) -> #cb_context{}).
delete(#cb_context{db_name=undefined}=Context) ->
    crossbar_util:response_db_missing(Context);
delete(#cb_context{db_name=DB, doc=Doc}=Context) ->
    case couch_mgr:del_doc(DB, Doc) of
        {error, db_not_reachable} ->
            crossbar_util:response_datastore_timeout(Context);
	{ok, _Doc} ->
	    format_log(info, "CB_DOC.delete: result: ~p~n", [_Doc]),
            Context#cb_context{
	      doc=undefined
	      ,resp_status=success
	      ,resp_data=[]
	     };
        _Else ->
            format_log(error, "CB_DOC.delete: Unexpected return from datastore: ~p~n", [_Else]),
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
    format_log(error, "Unhandled Json format in public_fields:~n~p~n", [Json]),
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
    format_log(error, "Unhandled Json format in private fields:~n~p~n", [Json]),
    Json.
    
%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will return a boolean, true if the provided key is
%% considered private; otherwise false
%% @end
%%--------------------------------------------------------------------
-spec(is_private_key/1 :: (Key :: binary()) -> boolean()).
is_private_key(Key) ->
    binary:first(Key) == 95 orelse byte_size(Key) < 4 orelse binary:bin_to_list(Key, 0, 4) == "pvt_".

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
            ETag = whistle_util:to_list(Rev),
            string:sub_string(ETag, 1, 2) ++ string:sub_string(ETag, 4);
        _Else ->
            undefined
    end;
rev_to_etag(_Json) ->
    format_log(error, "Unhandled Json format in rev to etag:~n~p~n", [_Json]),
    undefined.
