%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 26 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar_doc).

-export([load/2, load_merge/3, load_view/3]).
-export([save/1, delete/1]).
-export([couch_to_json/1]).
-export([public_fields/1, private_fields/1, is_private_field/1]).
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
-spec(load/2 :: (DocId :: term(), Context :: #cb_context{}) -> #cb_context{}).
load(DocId, Context) when not is_binary(DocId)->
    load(whistle_util:to_binary(DocId), Context);
load(DocId, #cb_context{db_name=DB}=Context) ->
    case couch_mgr:open_doc(DB, DocId) of
        {error, not_found} ->
            crossbar_util:response_bad_identifier(DocId, Context);
        {error, db_not_reachable} ->
            crossbar_util:response_datastore_timeout(Context);
	Doc when is_list(Doc) ->
            {struct, Json} = couch_to_json({Doc}),
            Context#cb_context{
                 doc=Json
                ,resp_status=success
                ,resp_data=[{struct, public_fields(Json)}]
                ,resp_etag=rev_to_etag(Doc)
            };
        _Else ->
            format_log(error, "Unexpected return from datastore: ~p~n", [_Else]),
            Context
    end.

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
-spec(load_merge/3 :: (DocId :: term(), Data :: proplist(), Context :: #cb_context{}) -> #cb_context{}).
load_merge(DocId, Data, Context) when not is_binary(DocId)->
    load_merge(whistle_util:to_binary(DocId), Data, Context);
load_merge(DocId, Data, Context) ->
    case load(DocId, Context) of
        #cb_context{resp_status=success, doc=Doc} = Context1 ->
            NewJson = private_fields(Doc) ++ Data,
            PubJson = public_fields(Doc),
            Context1#cb_context{
                 doc=NewJson
                ,resp_status=success
                ,resp_data=[{struct, PubJson}]
                ,resp_etag=rev_to_etag(Doc)
            };
        _Else ->
            format_log(error, "Unexpected return from datastore: ~p~n", [_Else]),
            Context
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
load_view(View, Options, #cb_context{db_name=DB}=Context) ->
    case couch_mgr:get_results(DB, View, Options) of
	false ->
            crossbar_util:response_db_missing(Context);
	{error,invalid_view_name} ->
            crossbar_util:response_missing_view(Context);
        {{error,not_found},fetch_failed}  ->
            crossbar_util:response_missing_view(Context);
	Doc when is_list(Doc) ->
            Json = couch_to_json(Doc),
            Context#cb_context{
                 doc=Json
                ,resp_status=success
                ,resp_data=Json
                ,resp_etag=automatic
            };
        _Else ->
            format_log(error, "Unexpected return from datastore: ~p~n", [_Else]),
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
save(#cb_context{db_name=DB, doc=Doc}=Context) ->
    case couch_mgr:save_doc(DB, Doc) of
        {error, db_not_reachable} ->
            crossbar_util:response_datastore_timeout(Context);
	{ok, Doc1} ->
            {struct, Json} = couch_to_json({Doc1}),
            PubJson = public_fields(Json),
            Context#cb_context{
                 doc=Json
                ,resp_status=success
                ,resp_data=[{struct, PubJson}]
                ,resp_etag=rev_to_etag(Doc1)
            };
        _Else ->
            format_log(error, "Unexpected return from datastore: ~p~n", [_Else]),
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
delete(#cb_context{db_name=DB, doc=Doc}=Context) ->
    case couch_mgr:del_doc(DB, Doc) of
        {error, db_not_reachable} ->
            crossbar_util:response_datastore_timeout(Context);
	{ok, _} ->
            Context#cb_context{
                 doc=undefined
                ,resp_status=success
                ,resp_data=[]
            };
        _Else ->
            format_log(error, "Unexpected return from datastore: ~p~n", [_Else]),
            Context
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will convert the couchbeam JSON encoding to mochijson2
%% @end
%%--------------------------------------------------------------------
-spec(couch_to_json/1 :: (Doc :: proplist()) -> proplist()).
couch_to_json(Doc) ->
    Str = couchbeam_util:json_encode(Doc),
    mochijson2:decode(Str).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will filter any private fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec(public_fields/1 :: (Json :: proplist()) -> proplist()).
public_fields(Json) ->
    PubDoc =
    lists:filter(fun({K, _}) ->
                        not is_private_field(K)
                 end, Json),
    [{<<"id">>, proplists:get_value(<<"_id">>, Json)}] ++ PubDoc.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will filter any public fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec(private_fields/1 :: (Json :: proplist()) -> proplist()).
private_fields(Json) ->
    lists:filter(fun({K, _}) ->
                        is_private_field(K)
                 end, Json).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will return a boolean, true if the provided key is
%% considered private; otherwise false
%% @end
%%--------------------------------------------------------------------
-spec(is_private_field/1 :: (Json :: proplist()) -> proplist()).
is_private_field(Key) ->
    binary:first(Key) == 95 orelse binary:bin_to_list(Key, 0, 4) == "pvt_".

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will attempt to convert a revision tag on the provided
%% document into a usable ETag for the response
%% @end
%%--------------------------------------------------------------------
-spec(rev_to_etag/1 :: (Doc :: proplist()) -> undefined | string()).
rev_to_etag(Doc) ->
    case proplists:get_value(<<"_rev">>, Doc) of
        undefined ->
            undefined;
        Rev ->
            ETag = whistle_util:to_list(Rev),
            string:sub_string(ETag, 1, 2) ++ string:sub_string(ETag, 4)
    end.