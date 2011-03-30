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
-export([start_link/0, update_all_accounts/1, replicate_from_accounts/2, replicate_from_account/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).

-define(ACCOUNTS_DB, "crossbar%2Faccounts").

-define(VIEW_FILE, <<"views/accounts.json">>).

-define(VIEW_LIST, {"accounts", "listing"}).
-define(VIEW_PARENT, {"accounts", "parent"}).
-define(VIEW_CHILDREN, {"accounts", "children"}).
-define(VIEW_DESCENDANTS, {"accounts", "descendants"}).

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

%%--------------------------------------------------------------------
%% @doc
%% Update a document in each crossbar account database with the
%% file contents.  This is intended for _design docs....
%%
%% @spec update_all_accounts() -> ok | error
%% @end
%%--------------------------------------------------------------------
-spec(update_all_accounts/1 :: (File :: list() | binary()) -> ok | error).
update_all_accounts(File) ->
    case crossbar_doc:load_view(?VIEW_LIST, [], #cb_context{db_name=?ACCOUNTS_DB}) of
        #cb_context{resp_status=success, doc=Doc} ->
            lists:foreach(fun(Account) ->                                  
                                  DbName = get_db_name(whapps_json:get_value(["id"], Account)),
                                  case couch_mgr:update_doc_from_file(DbName, crossbar, File) of
                                      {error, not_found} ->
                                          couch_mgr:load_doc_from_file(DbName, crossbar, File);
                                      Else ->
                                          Else
                                  end
                          end, Doc),
                ok;
        _Else ->
            error
    end.

-spec(replicate_from_accounts/2 :: (TargetDB :: binary(), FilterDoc :: binary()) -> ok | error).
replicate_from_accounts(TargetDB, FilterDoc) when is_binary(FilterDoc) ->
    case crossbar_doc:load_view(?VIEW_LIST, [], #cb_context{db_name=?ACCOUNTS_DB}) of
        #cb_context{resp_status=success, doc=Doc} ->
	    BaseReplicate = [{<<"target">>, TargetDB}
			     ,{<<"filter">>, FilterDoc}
			     ,{<<"create_target">>, true}
			    ],

            lists:foreach(fun(Account) ->                                  
                                  DbName = get_db_name(whapps_json:get_value(["id"], Account), unencoded),
				  couch_mgr:db_replicate([{<<"source">>, DbName} | BaseReplicate])
                          end, Doc),
	    ok;
        _Else ->
            error
    end.

-spec(replicate_from_account/3 :: (SourceDB :: binary(), TargetDB :: binary(), FilterDoc :: binary()) -> ok | error).
replicate_from_account(SourceDB, TargetDB, FilterDoc) when is_binary(FilterDoc) ->
    BaseReplicate = [{<<"source">>, get_db_name(SourceDB, unencoded)}
		     ,{<<"target">>, TargetDB}
		     ,{<<"filter">>, FilterDoc}
		     ,{<<"create_target">>, true}
		    ],
    couch_mgr:db_replicate(BaseReplicate).

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
    crossbar_doc:load_from_file(?ACCOUNTS_DB, ?VIEW_FILE),
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
handle_info({binding_fired, Pid, <<"v1_resource.validate.accounts">>, [RD, #cb_context{req_nouns=[{<<"accounts">>, _}]}=Context | Params]}, State) ->
    spawn(fun() ->
		  crossbar_util:binding_heartbeat(Pid),
		  Context1 = validate(Params, Context#cb_context{db_name=?ACCOUNTS_DB}),
		  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};
handle_info({binding_fired, Pid, <<"v1_resource.validate.accounts">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                Context1 = load_account_db(Params, Context),
                Pid ! {binding_result, true, [RD, Context1, Params]}
	 end),
    {noreply, State};
handle_info({binding_fired, Pid, <<"v1_resource.execute.post.accounts">>, [RD, Context | [_, <<"parent">>]=Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:binding_heartbeat(Pid),
                  case crossbar_doc:save(Context) of
                      #cb_context{resp_status=success}=Context1 ->
                          Pid ! {binding_result, true, [RD, Context1#cb_context{resp_data={struct, []}}, Params]};
                      Else ->
                          Pid ! {binding_result, true, [RD, Else, Params]}
                  end
	  end),
    {noreply, State};
handle_info({binding_fired, Pid, <<"v1_resource.execute.post.accounts">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  Context1 = crossbar_doc:save(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};
handle_info({binding_fired, Pid, <<"v1_resource.execute.put.accounts">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  case crossbar_doc:save(Context) of
                      #cb_context{resp_status=success, doc=Doc}=Context1 ->
                          DbName = get_db_name(Doc),
                          case couch_mgr:db_create(DbName) of
                              false ->
                                  format_log(error, "ACCOUNTS(~p): Failed to create database: ~p~n", [self(), get_db_name(Doc)]),
                                  crossbar_doc:delete(Context1),
                                  Pid ! {binding_result, true, [RD, crossbar_util:response_db_fatal(Context), Params]};
                              true ->
                                  Pid ! {binding_result, true, [RD, Context1, Params]},
                                  Responses = crossbar_bindings:map(<<"account.created">>, Context1),                                  
                                  lists:foreach(fun({true, File}) ->                                         
                                                        couch_mgr:load_doc_from_file(DbName, crossbar, File)
                                                end, crossbar_bindings:succeeded(Responses))
                          end;
                      Else ->
                          Pid ! {binding_result, true, [RD, Else, Params]}
                  end
	  end),
    {noreply, State};
%handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.accounts">>, [RD, #cb_context{doc=Doc}=Context | [_, <<"parent">>]=Params]}, State) ->
%    %%spawn(fun() ->
%                  Doc1 = crossbar_util:set_json_values(<<"pvt_tree">>, [], Doc),
%                  Context1 = crossbar_doc:save(Context#cb_context{db_name=?ACCOUNTS_DB, doc=Doc1}),
%                  Pid ! {binding_result, true, [RD, Context1, Params]},
%	%%  end),
%    {noreply, State};
handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.accounts">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  Context1 = crossbar_doc:delete(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};
handle_info({binding_fired, Pid, _Route, Payload}, State) ->
    Pid ! {binding_result, true, Payload},
    {noreply, State};
handle_info(_Info, State) ->
    format_log(info, "ACCOUNTS(~p): unhandled info ~p~n", [self(), _Info]),
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
-spec(bind_to_crossbar/0 :: () ->  ok | tuple(error, exists)).
bind_to_crossbar() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.accounts">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.accounts">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.accounts">>),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.#.accounts">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec(allowed_methods/1 :: (Paths :: list()) -> tuple(boolean(), http_methods())).
allowed_methods([]) ->
    {true, ['GET', 'PUT']};
allowed_methods([_]) ->
    {true, ['GET', 'POST', 'DELETE']};
allowed_methods([_, <<"parent">>]) ->
    {true, ['GET', 'POST', 'DELETE']};
allowed_methods([_, Path]) ->
    Valid = lists:member(Path, [<<"ancestors">>, <<"children">>, <<"descendants">>, <<"siblings">>]),
    {Valid, ['GET']};
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
    Valid = lists:member(Path, [<<"parent">>, <<"ancestors">>, <<"children">>, <<"descendants">>, <<"siblings">>]),
    {Valid, []};
resource_exists(_) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec(validate/2 :: (Params :: list(), Context :: #cb_context{}) -> #cb_context{}).
validate([], #cb_context{req_verb = <<"get">>}=Context) ->
    load_account_summary([], Context);
validate([], #cb_context{req_verb = <<"put">>}=Context) ->
    create_account(Context);
validate([DocId], #cb_context{req_verb = <<"get">>}=Context) ->
    load_account(DocId, Context);
validate([DocId], #cb_context{req_verb = <<"post">>}=Context) ->
    update_account(DocId, Context);
validate([DocId], #cb_context{req_verb = <<"delete">>}=Context) ->
    load_account(DocId, Context);
validate([DocId, <<"parent">>], #cb_context{req_verb = <<"get">>}=Context) ->
    load_parent(DocId, Context);
validate([DocId, <<"parent">>], #cb_context{req_verb = <<"post">>}=Context) ->
    update_parent(DocId, Context);
validate([DocId, <<"parent">>], #cb_context{req_verb = <<"delete">>}=Context) ->
    load_account(DocId, Context);
validate([DocId, <<"children">>], #cb_context{req_verb = <<"get">>}=Context) ->
    load_children(DocId, Context);
validate([DocId, <<"descendants">>], #cb_context{req_verb = <<"get">>}=Context) ->
    load_descendants(DocId, Context);
validate([DocId, <<"siblings">>], #cb_context{req_verb = <<"get">>}=Context) ->
    load_siblings(DocId, Context);
validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec(load_account_summary/2 :: (DocId :: binary() | [], Context :: #cb_context{}) -> #cb_context{}).
load_account_summary([], Context) ->
    crossbar_doc:load_view(?VIEW_LIST, [], Context, fun normalize_view_results/2);
load_account_summary(DocId, Context) ->
    crossbar_doc:load_view(?VIEW_LIST, [
         {<<"startkey">>, [DocId]}
        ,{<<"endkey">>, [DocId, {struct, []}]}
    ], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new account document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec(create_account/1 :: (Context :: #cb_context{}) -> #cb_context{}).
create_account(#cb_context{req_data=Data}=Context) ->
    case is_valid_doc(Data) of
        {false, Fields} ->
            crossbar_util:response_invalid_data(Fields, Context);
        {true, []} ->
            Context#cb_context{
	      doc=set_private_fields(Data)
	      ,resp_status=success
	     }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an account document from the database
%% @end
%%--------------------------------------------------------------------
-spec(load_account/2 :: (DocId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
load_account(DocId, Context) ->
    crossbar_doc:load(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing account document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec(update_account/2 :: (DocId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
update_account(DocId, #cb_context{req_data=Data}=Context) ->
    case is_valid_doc(Data) of
        {false, Fields} ->
            crossbar_util:response_invalid_data(Fields, Context);
        {true, []} ->
            crossbar_doc:load_merge(DocId, Data, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summary of the parent of the account
%% @end
%%--------------------------------------------------------------------
-spec(load_parent/2 :: (DocId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
load_parent(DocId, Context) ->
    View =
        crossbar_doc:load_view(?VIEW_PARENT, [
             {<<"startkey">>, DocId}
            ,{<<"endkey">>, DocId}
        ], Context),
    case View#cb_context.doc of
        [JObj|_] ->
            Parent = whapps_json:get_value([<<"value">>, <<"id">>], JObj),
            load_account_summary(Parent, Context);
        _Else ->
            crossbar_util:response_bad_identifier(DocId, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update the tree with a new parent, cascading when necessary, if the
%% new parent is valid
%% @end
%%--------------------------------------------------------------------
-spec(update_parent/2 :: (DocId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
update_parent(DocId, #cb_context{req_data=Data}=Context) ->
    case is_valid_parent(Data) of
        {false, Fields} ->
            crossbar_util:response_invalid_data(Fields, Context);
        {true, []} ->
            %% OMGBBQ! NO CHECKS FOR CYCLIC REFERENCES WATCH OUT!
            ParentId = props:get_value(<<"parent">>, element(2, Data)),
            update_tree(DocId, ParentId, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a summary of the children of this account
%% @end
%%--------------------------------------------------------------------
-spec(load_children/2 :: (DocId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
load_children(DocId, Context) ->
    crossbar_doc:load_view(?VIEW_CHILDREN, [
         {<<"startkey">>, [DocId]}
        ,{<<"endkey">>, [DocId, {struct, []}]}
    ], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a summary of the descendants of this account
%% @end
%%--------------------------------------------------------------------
-spec(load_descendants/2 :: (DocId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
load_descendants(DocId, Context) ->
    crossbar_doc:load_view(?VIEW_DESCENDANTS, [
         {<<"startkey">>, [DocId]}
        ,{<<"endkey">>, [DocId, {struct, []}]}
    ], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a summary of the siblngs of this account
%% @end
%%--------------------------------------------------------------------
-spec(load_siblings/2 :: (DocId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
load_siblings(DocId, Context) ->
    View =
        crossbar_doc:load_view(?VIEW_PARENT, [
             {<<"startkey">>, DocId}
            ,{<<"endkey">>, DocId}
        ], Context),
    case View#cb_context.doc of
        [JObj|_] ->
            Parent = whapps_json:get_value([<<"value">>, <<"id">>], JObj),
            load_children(Parent, Context);
        _Else ->
            crossbar_util:response_bad_identifier(DocId, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec(normalize_view_results/2 :: (JObj :: json_object(), Acc :: json_objects()) -> json_objects()).
normalize_view_results(JObj, Acc) ->
    [whapps_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(is_valid_parent/1 :: (JObj :: json_object()) -> tuple(true, [])). %tuple(boolean(), list())).
is_valid_parent({struct, [_]}) ->
    {true, []};
is_valid_parent(_JObj) ->
    {true, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(is_valid_doc/1 :: (Data :: json_object()) -> tuple(boolean(), json_objects())).
is_valid_doc(undefined) ->
    {false, []};
is_valid_doc({struct, Data}) ->
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
%%
%% @end
%%--------------------------------------------------------------------
-spec(update_tree/3 :: (DocId :: binary(), ParentId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
update_tree(DocId, ParentId, Context) ->
    case crossbar_doc:load(ParentId, Context) of
        #cb_context{resp_status=success, doc=Parent} ->
            Descendants =
                crossbar_doc:load_view(?VIEW_DESCENDANTS, [
                     {<<"startkey">>, [DocId]}
                    ,{<<"endkey">>, [DocId, {struct, []}]}
                ], Context),
            case Descendants of
                #cb_context{resp_status=success, doc=[]} ->
                    crossbar_util:response_bad_identifier(DocId, Context);
                #cb_context{resp_status=success, doc=Doc}=Context1 ->
                    Tree = whapps_json:get_value(<<"pvt_tree">>, Parent) ++ [ParentId, DocId],
                    Updater = fun(Update, Acc) -> update_doc_tree(Tree, Update, Acc) end,
                    Updates = lists:foldr(Updater, [], Doc),
                    Context1#cb_context{doc=Updates}
            end;
        Else ->
            Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(update_doc_tree/3 :: (ParentTree :: list(), Update :: json_object(), Acc :: json_objects()) -> json_objects()).
update_doc_tree(ParentTree, {struct, Prop}, Acc) ->
    DocId = props:get_value(<<"id">>, Prop),
    ParentId = lists:last(ParentTree),
    case crossbar_doc:load(DocId, #cb_context{db_name=?ACCOUNTS_DB}) of
        #cb_context{resp_status=success, doc=Doc} ->
            Tree = whapps_json:get_value(<<"pvt_tree">>, Doc),
            SubTree =
                case lists:dropwhile(fun(E)-> E =/= ParentId end, Tree) of
                    [] -> [];
                    List -> lists:nthtail(1,List)
                end,
            NewTree = lists:filter(fun(E) -> E =/= DocId end, ParentTree ++ SubTree),
            [whapps_json:set_value(<<"pvt_tree">>, NewTree, Doc) | Acc];
        _Else ->
            Acc
    end;
update_doc_tree(_ParentTree, _Object, Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function returns the private fields to be added to a new account
%% document
%% @end
%%--------------------------------------------------------------------
-spec(set_private_fields/1 :: (JObj :: json_object()) -> json_object()).
set_private_fields(JObj) ->
    JObj1 = whapps_json:set_value(<<"pvt_type">>, <<"account">>, JObj),
    whapps_json:set_value(<<"pvt_tree">>, [], JObj1).
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will verify an account id is valid, and if so return
%% the name of the account database
%% @end
%%--------------------------------------------------------------------
-spec(get_db_name/1 :: (DocId :: list(binary()) | json_object()) -> undefined | binary()).
get_db_name(Doc) -> get_db_name(Doc, encoded).

-spec(get_db_name/2 :: (DocId :: list(binary()) | json_object(), Encoded :: unencoded | encoded) -> undefined | binary()).
get_db_name({struct, _}=Doc, Encoded) ->
    get_db_name([whapps_json:get_value(["_id"], Doc)], Encoded);
get_db_name([DocId], Encoded) when is_binary(DocId) ->
    get_db_name(DocId, Encoded);
get_db_name(DocId, encoded) when is_binary(DocId) ->
    Id = whistle_util:to_list(DocId),
    Db = ["crossbar%2Fclients%2F", string:sub_string(Id, 1, 2), "%2F", string:sub_string(Id, 3, 4), "%2F", string:sub_string(Id, 5)],
    whistle_util:to_binary(Db);
get_db_name(DocId, unencoded) when is_binary(DocId) ->
    case binary:longest_common_prefix([<<"crossbar%2Fclients%2F">>, DocId]) of
	0 ->
	    format_log(info, "DocID unenc: ~p~n", [DocId]),
	    Id = whistle_util:to_list(DocId),
	    Db = ["crossbar/clients/", string:sub_string(Id, 1, 2), "/", string:sub_string(Id, 3, 4), "/", string:sub_string(Id, 5)],
	    whistle_util:to_binary(Db);
	_ ->
	    %% already encoded, convert %2F to /
	    whistle_util:to_binary(mochiweb_util:unquote(DocId))
    end;
get_db_name(_, _) ->
    undefined.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will attempt to load the context with the db name of
%% for this account
%% @end
%%--------------------------------------------------------------------
-spec(load_account_db/2 :: (DocId :: list(binary()) | json_object(), #cb_context{}) -> #cb_context{}).
load_account_db(DocId, Context)->
    DbName = get_db_name(DocId),
    case couch_mgr:db_exists(DbName) of
        false ->
            Context#cb_context{
                 db_name = undefined
                ,account_id = undefined
            };
        true ->
            Context#cb_context{
                db_name = DbName
               ,account_id = DocId
            }
    end.
