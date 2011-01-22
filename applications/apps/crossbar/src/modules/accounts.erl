%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%% Account module
%%%
%%% Handle client requests for account documents
%%%
%%% @end
%%% Created : 05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(accounts).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-include("../crossbar.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-define(SERVER, ?MODULE).

-define(ACCOUNTS_DB, "accounts").
-define(ACCOUNTS_LIST, {"accounts","listing"}).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init/1 :: (_) -> tuple(ok, #state{})).
init([]) ->
    bind_to_crossbar(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    io:format("Unhandled ~p", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} | 
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.accounts">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.accounts">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.accounts">>, [RD, Context | Params]}, State) ->
    %%spawn(fun() ->
                  Context1 = validate(wrq:method(RD), Params, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]},
	%%  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.accounts">>, [RD, #cb_context{doc=Doc, resp_status=success}=Context | Params]}, State) ->
    spawn(fun() ->
                  Context1 = save_doc(Doc, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.accounts">>, [RD, #cb_context{doc=Doc, resp_status=success}=Context | Params]}, State) ->
    spawn(fun() ->
                  Context1 = save_doc(Doc, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.accounts">>, [RD, #cb_context{doc=Doc, resp_status=success}=Context | Params]}, State) ->
    spawn(fun() ->
                  Context1 = delete_doc(Doc, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, Route, Payload}, State) ->
    format_log(info, "ACCOUNT(~p): unhandled binding: ~p~n", [self(), Route]),
    Pid ! {binding_result, true, Payload},
    {noreply, State};

handle_info(_Info, State) ->
    format_log(info, "ACCOUNT(~p): unhandled info ~p~n", [self(), _Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function binds this server to the crossbar bindings server,
%% for the keys we need to consume.
%% @end
%%--------------------------------------------------------------------
-spec(bind_to_crossbar/0 :: () -> no_return()).
bind_to_crossbar() ->
    crossbar_bindings:bind(<<"v1_resource.allowed_methods.accounts">>),
    crossbar_bindings:bind(<<"v1_resource.resource_exists.accounts">>),
    crossbar_bindings:bind(<<"v1_resource.validate.accounts">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.accounts">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec(validate/3 :: (Verb :: atom(), Params :: list(), Context :: #cb_context{}) -> #cb_context{}).
validate('GET', [], Context) ->
    load_view(?ACCOUNTS_LIST, [], Context);
validate('PUT', [], #cb_context{req_data={struct,Data}}=Context) ->
    case is_valid_doc(Data) of
        {true, []} ->
            create_doc(Data, Context);
        {false, Fields} ->
            Context#cb_context {
                 resp_status = error
                ,resp_error_msg = "Invalid data"
                ,resp_error_code = 400
                ,resp_data = Fields
            }
    end;
validate('GET', [DocId], Context) ->
    load_doc(DocId, Context);
validate('POST', [DocId], #cb_context{req_data={struct,Data}}=Context) ->
    case is_valid_doc(Data) of
        {true, []} ->
            load_merge_doc(DocId, Data, Context);
        {false, Fields} ->
            Context#cb_context {
                 resp_status = error
                ,resp_error_msg = "Invalid data"
                ,resp_error_code = 400
                ,resp_data = Fields
            }
    end;
validate('DELETE', [DocId], Context) ->
    load_doc(DocId, Context);
validate('DELETE', [DocId, <<"parents">>], Context) ->
    load_doc(DocId, Context);
validate('GET', [DocId, _], Context) ->
    load_doc(DocId, Context);
validate(_, _, Context) ->
    Context#cb_context {
         resp_status = error
	,resp_error_msg = "Faulty request"
	,resp_error_code = 400
    }.

is_valid_doc(Data) ->
    Schema = [
	   { [<<"base">>, <<"name">>]
	    ,[ {not_empty, []}
              ,{is_format, [phrase]}
	     ]}
	   ,{ [<<"base">>, <<"status">>]
	      ,[ {not_empty, []}
		%,{in_list, [{<<"enabled">>, <<"disabled">>}]}
	       ]}
	  ],
    Failed = crossbar_validator:validate(Schema, Data),
    {Failed =:= [], Failed}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec(allowed_methods/1 :: (Paths :: list()) -> tuple(boolean(), [])).
allowed_methods([]) ->
    {true, ['GET', 'PUT']};
allowed_methods([_]) ->
    {true, ['GET', 'POST', 'DELETE']};
allowed_methods([_, <<"parents">>]) ->
    {true, ['GET', 'DELETE']};
allowed_methods([_, Path]) ->
    Valid = lists:member(Path, [<<"ancestors">>, <<"children">>, <<"descendants">>]),
    {Valid, ['GET']};
allowed_methods([_, <<"parents">>, _]) ->
    {true, ['PUT']};
allowed_methods(_) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec(resource_exists/1 :: (Paths :: list()) -> tuple(boolean(), [])).
resource_exists([]) ->
    {true, []};
resource_exists([_]) ->
    {true, []};
resource_exists([_, Path]) ->
    Valid = lists:member(Path, [<<"parents">>, <<"ancestors">>, <<"children">>, <<"descendants">>]),
    {Valid, []};
resource_exists([_, <<"parents">>, _]) ->
    {true, []};
resource_exists(_) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function attempts to load the context with account details,
%% including the account db name and the account doc.
%%
%% Failure here returns 404, 500, or 503
%% @end
%%--------------------------------------------------------------------
-spec(load_doc/2 :: (DocId :: term(), Context :: #cb_context{}) -> #cb_context{}).
load_doc(DocId, Context) when not is_binary(DocId)->
    load_doc(whistle_util:to_binary(DocId), Context);
load_doc(DocId, Context) ->
    case couch_mgr:open_doc(?ACCOUNTS_DB, DocId) of
        {error, not_found} ->
            Context#cb_context {
                 resp_status = error
                ,resp_error_msg = "account not found"
                ,resp_error_code = 404
            };
        {error, db_not_reachable} ->
            Context#cb_context {
                 resp_status = error
                ,resp_error_msg = "datastore timeout"
                ,resp_error_code = 503
            };
	Doc when is_list(Doc) ->
            {struct, Json} = couch_to_json({Doc}),
            PubJson = json_public_fields(Json),
            Context#cb_context{
                 doc=Json
                ,db_name=get_db_name(Doc)
                ,resp_status=success
                ,resp_data=[{struct, PubJson}]
                ,resp_etag=rev_to_etag(Doc)
            };
	_Else ->
            Context#cb_context {
                 resp_status = error
                ,resp_error_msg = "unexpeced datastore response"
                ,resp_error_code = 500
                ,resp_data = [_Else]
            }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function attempts to merge the submitted data with the private
%% fields of an existing account document, if successful it will
%% load the context with the account details
%%
%% Failure here returns 404, 500, or 503
%% @end
%%--------------------------------------------------------------------
-spec(load_merge_doc/3 :: (DocId :: term(), Data :: proplist(), Context :: #cb_context{}) -> #cb_context{}).
load_merge_doc(DocId, Data, Context) when not is_binary(DocId)->
    load_merge_doc(whistle_util:to_binary(DocId), Data, Context);
load_merge_doc(DocId, Data, Context) ->
    case couch_mgr:open_doc(?ACCOUNTS_DB, DocId) of
        {error, not_found} ->
            Context#cb_context {
                 resp_status = error
                ,resp_error_msg = "account not found"
                ,resp_error_code = 404
            };
        {error, db_not_reachable} ->
            Context#cb_context {
                 resp_status = error
                ,resp_error_msg = "datastore timeout"
                ,resp_error_code = 503
            };
	Doc when is_list(Doc) ->
            {struct, Json} = couch_to_json({Doc}),
            NewJson = json_private_fields(Json) ++ Data,
            PubJson = json_public_fields(NewJson),
            Context#cb_context{
                 doc=NewJson
                ,db_name=get_db_name(Doc)
                ,resp_status=success
                ,resp_data=[{struct, PubJson}]
                ,resp_etag=rev_to_etag(Doc)
            };
	_Else ->
            Context#cb_context {
                 resp_status = error
                ,resp_error_msg = "unexpeced datastore response"
                ,resp_error_code = 500
                ,resp_data = [_Else]
            }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function attempts to load the context with the results of a view
%% run against the accounts database.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec(load_view/3 :: (View :: tuple(string(), string()), Options :: proplist(), Context :: #cb_context{}) -> #cb_context{}).
load_view(View, Options, Context) ->
    case couch_mgr:get_results(?ACCOUNTS_DB, View, Options) of
	false ->
            Context#cb_context {
                 resp_status = error
                ,resp_error_msg = "datastore unable to fulfill request"
                ,resp_error_code = 503
            };
	{error,invalid_view_name} ->
            Context#cb_context {
                 resp_status = error
                ,resp_error_msg = "datastore missing view"
                ,resp_error_code = 500
            };
        {{error,not_found},fetch_failed}  ->
            Context#cb_context {
                 resp_status = error
                ,resp_error_msg = "datastore missing view"
                ,resp_error_code = 500
            };
	Doc when is_list(Doc) ->
            Json = couch_to_json(Doc),
            Context#cb_context{
                 doc=Json
                ,resp_status=success
                ,resp_data=Json
                ,resp_etag=automatic
            };
	_Else ->
            Context#cb_context {
                 resp_status = error
                ,resp_error_msg = "unexpeced datastore response"
                ,resp_error_code = 500
                ,resp_data = [_Else]
            }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function creates a new account document with the given data
%% @end
%%--------------------------------------------------------------------
-spec(create_doc/2 :: (Data :: proplist(), Context :: #cb_context{}) -> #cb_context{}).
create_doc(Data, Context) ->
    Doc = create_private_fields() ++ Data,
    {struct, NewJson} = couch_to_json({Doc}),
    PubJson = json_public_fields(NewJson),
    Context#cb_context{
         doc=NewJson
        ,db_name=get_db_name(NewJson)
        ,resp_status=success
        ,resp_data=[{struct, PubJson}]
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function attempts to save the provided document to the accounts
%% database. The result is loaded into the context record.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec(save_doc/2 :: (Doc :: proplist(), Context :: #cb_context{}) -> #cb_context{}).
save_doc(Doc, Context) ->
    case couch_mgr:save_doc(?ACCOUNTS_DB, Doc) of
        {error, db_not_reachable} ->
            Context#cb_context {
                 resp_status = error
                ,resp_error_msg = "datastore timeout"
                ,resp_error_code = 503
            };
	{ok, Doc1} ->
            {struct, Json} = couch_to_json({Doc1}),
            PubJson = json_public_fields(Json),
            Context#cb_context{
                 doc=Json
                ,resp_status=success
                ,resp_data=[{struct, PubJson}]
                ,resp_etag=rev_to_etag(Doc1)
            };
	_Else ->
            Context#cb_context {
                 resp_status = error
                ,resp_error_msg = "unexpeced datastore response!!"
                ,resp_error_code = 500
                ,resp_data = [_Else]
            }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will attempt to remove an account document from the
%% account database
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec(delete_doc/2 :: (Doc :: proplist(), Context :: #cb_context{}) -> #cb_context{}).
delete_doc(Doc, Context) ->
    case couch_mgr:del_doc(?ACCOUNTS_DB, Doc) of
        {error, db_not_reachable} ->
            Context#cb_context {
                 resp_status = error
                ,resp_error_msg = "datastore timeout"
                ,resp_error_code = 503
            };
	{ok, _} ->
            Context#cb_context{
                 doc=undefined
                ,resp_status=success
                ,resp_data=[]
            };
	_Else ->
            Context#cb_context {
                 resp_status = error
                ,resp_error_msg = "unexpeced datastore response!!"
                ,resp_error_code = 500
                ,resp_data = [_Else]
            }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function returns the private fields to be added to a new account
%% document
%% @end
%%--------------------------------------------------------------------
-spec(create_private_fields/0 :: () -> proplist()).
create_private_fields() ->
    [
        {<<"pvt_identifier">>,{struct,[{<<"type">>,<<"account">>},{<<"tree">>,[]}]}}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will filter any private fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec(json_public_fields/1 :: (Json :: proplist()) -> proplist()).
json_public_fields(Json) ->
    PubDoc =
    lists:filter(fun({K, _}) ->
                        not is_private_field(K)
                 end, Json),
    [{<<"id">>, proplists:get_value(<<"_id">>, Json)}] ++ PubDoc.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will filter any public fields out of the provided
%% json proplist
%% @end
%%--------------------------------------------------------------------
-spec(json_private_fields/1 :: (Json :: proplist()) -> proplist()).
json_private_fields(Json) ->
    lists:filter(fun({K, _}) ->
                        is_private_field(K)
                 end, Json).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will return a boolean, true if the provided key is
%% considered private; otherwise false
%% @end
%%--------------------------------------------------------------------
-spec(is_private_field/1 :: (Json :: proplist()) -> proplist()).
is_private_field(Key) ->
    binary:first(Key) == 95 orelse binary:bin_to_list(Key, 0, 4) == "pvt_".

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will convert the couchbeam JSON encoding to mochijson2
%% @end
%%--------------------------------------------------------------------
-spec(couch_to_json/1 :: (Doc :: proplist()) -> proplist()).
couch_to_json(Doc) ->
    Str = couchbeam_util:json_encode(Doc),
    mochijson2:decode(Str).

%%--------------------------------------------------------------------
%% @private
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will attempt to convert the account document ID into
%% the account database name
%% @end
%%--------------------------------------------------------------------
-spec(get_db_name/1 :: (Doc :: proplist()) -> undefined | binary()).
get_db_name(Doc) ->
    case proplists:get_value(<<"_id">>, Doc) of
        undefined ->
            undefined;
       _Id ->
            Id = whistle_util:to_list(_Id),
            Db = [string:sub_string(Id, 1, 2), $/, string:sub_string(Id, 3, 4), $/, string:sub_string(Id, 5)],
            whistle_util:to_binary(Db)
    end.