%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Account module
%%%
%%% Handle client requests for account documents
%%%
%%% @end
%%% Created : 05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_accounts).

-behaviour(gen_server).

%% API
-export([start_link/0, create_account/1, get_realm_from_db/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).

-define(AGG_DB, <<"accounts">>).
-define(AGG_VIEW_FILE, <<"views/accounts.json">>).
-define(AGG_VIEW_SUMMARY, <<"accounts/listing_by_id">>).
-define(AGG_VIEW_PARENT, <<"accounts/listing_by_parent">>).
-define(AGG_VIEW_CHILDREN, <<"accounts/listing_by_children">>).
-define(AGG_VIEW_DESCENDANTS, <<"accounts/listing_by_descendants">>).
-define(AGG_GROUP_BY_REALM, <<"accounts/group_by_realm">>).
-define(AGG_FILTER, <<"account/export">>).

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

-spec(get_realm_from_db/1 :: (DBName :: binary()) -> tuple(ok, binary()) | tuple(error, atom())).
get_realm_from_db(DBName) ->
    Doc = whapps_util:get_db_name(DBName, raw),
    case couch_mgr:open_doc(DBName, Doc) of
	{ok, JObj} -> {ok, wh_json:get_value(<<"realm">>, JObj)};
	{error, _}=E -> E
    end.

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
init(_) ->
    {ok, ok, 0}.

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
    {reply, ok, State}.

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
                  crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  %% Do all of our prep-work out of the agg db
                  %% later we will switch to save to the client db
                  Context1 = validate(Params, Context#cb_context{db_name=?AGG_DB}),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.accounts">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  Context1 = load_account_db(Params, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.accounts">>, [RD, Context | [AccountId, <<"parent">>]=Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  case crossbar_doc:save(Context#cb_context{db_name=whapps_util:get_db_name(AccountId, encoded)}) of
                      #cb_context{resp_status=success}=Context1 ->
                          Pid ! {binding_result, true, [RD, Context1#cb_context{resp_data=?EMPTY_JSON_OBJECT}, Params]};
                      Else ->
                          Pid ! {binding_result, true, [RD, Else, Params]}
                  end
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.accounts">>, [RD, Context | [AccountId]=Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = crossbar_doc:save(Context#cb_context{db_name=whapps_util:get_db_name(AccountId, encoded)}),
		  whapps_util:replicate_from_account(whapps_util:get_db_name(AccountId, unencoded), ?AGG_DB, ?AGG_FILTER),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.accounts">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = create_new_account_db(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

%handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.accounts">>, [RD, #cb_context{doc=Doc}=Context | [_, <<"parent">>]=Params]}, State) ->
%    %%spawn(fun() ->
%                  Doc1 = crossbar_util:set_json_values(<<"pvt_tree">>, [], Doc),
%                  Context1 = crossbar_doc:save(Context#cb_context{db_name=?AGG_DB, doc=Doc1}),
%                  Pid ! {binding_result, true, [RD, Context1, Params]},
%       %%  end),
%    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.accounts">>, [RD, Context | [AccountId]=Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  %% dont use the account id in cb_context as it may not represent the db_name...
                  DbName = whapps_util:get_db_name(AccountId, encoded),
                  try
                      %% Ensure the DB that we are about to delete is an account
                      {ok, JObj} = couch_mgr:open_doc(DbName, AccountId),

                      <<"account">> = wh_json:get_value(<<"pvt_type">>, JObj),
		      ?LOG_SYS("opened ~s in ~s", [DbName, AccountId]),

                      #cb_context{resp_status=success} = crossbar_doc:delete(Context),
		      ?LOG_SYS("deleted ~s in ~s", [DbName, AccountId]),

                      case couch_mgr:db_delete(DbName) of
                          true -> Pid ! {binding_result, true, [RD, Context, Params]};
                          false -> Pid ! {binding_result, true, [RD, crossbar_util:response_db_fatal(Context), Params]}
                      end
                  catch
                      _:_E ->
			  ?LOG_SYS("Exception while deleting account: ~p", [_E]),
                          Pid ! {binding_result, true, [RD, crossbar_util:response_bad_identifier(AccountId, Context), Params]}
                  end
          end),
    {noreply, State};

handle_info({binding_fired, Pid, _, Payload}, State) ->
    Pid ! {binding_result, false, Payload},
    {noreply, State};

handle_info(timeout, State) ->
    bind_to_crossbar(),
    {noreply, State};

handle_info(_Info, State) ->
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
-spec(bind_to_crossbar/0 :: () ->  no_return()).
bind_to_crossbar() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.accounts">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.accounts">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.accounts">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.accounts">>).

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
resource_exists(_T) ->
    io:format("~p~n", [_T]),
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
validate([AccountId], #cb_context{req_verb = <<"get">>}=Context) ->
    load_account(AccountId, Context);
validate([AccountId], #cb_context{req_verb = <<"post">>}=Context) ->
    update_account(AccountId, Context);
validate([AccountId], #cb_context{req_verb = <<"delete">>}=Context) ->
    load_account(AccountId, Context);
validate([AccountId, <<"parent">>], #cb_context{req_verb = <<"get">>}=Context) ->
    load_parent(AccountId, Context);
validate([AccountId, <<"parent">>], #cb_context{req_verb = <<"post">>}=Context) ->
    update_parent(AccountId, Context);
validate([AccountId, <<"parent">>], #cb_context{req_verb = <<"delete">>}=Context) ->
    load_account(AccountId, Context);
validate([AccountId, <<"children">>], #cb_context{req_verb = <<"get">>}=Context) ->
    load_children(AccountId, Context);
validate([AccountId, <<"descendants">>], #cb_context{req_verb = <<"get">>}=Context) ->
    load_descendants(AccountId, Context);
validate([AccountId, <<"siblings">>], #cb_context{req_verb = <<"get">>}=Context) ->
    load_siblings(AccountId, Context);
validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec(load_account_summary/2 :: (AccountId :: binary() | [], Context :: #cb_context{}) -> #cb_context{}).
load_account_summary([], Context) ->
    crossbar_doc:load_view(?AGG_VIEW_SUMMARY, [], Context, fun normalize_view_results/2);
load_account_summary(AccountId, Context) ->
    crossbar_doc:load_view(?AGG_VIEW_SUMMARY, [
         {<<"startkey">>, [AccountId]}
        ,{<<"endkey">>, [AccountId, ?EMPTY_JSON_OBJECT]}
    ], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new account document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec(create_account/1 :: (Context :: #cb_context{}) -> #cb_context{}).
create_account(#cb_context{req_data=JObj}=Context) ->
    case is_valid_doc(JObj) of
        {false, Fields} ->
	    ?LOG_SYS("Invalid JSON failed on fields ~p", [Fields]),
            crossbar_util:response_invalid_data(Fields, Context);
        {true, _} ->
	    ?LOG_SYS("JSON is valid"),
            case is_unique_realm(undefined, Context) of
                true ->
		    ?LOG_SYS("Realm ~s is valid and unique", [wh_json:get_value(<<"realm">>, JObj)]),
                    Context#cb_context{
                      doc=set_private_fields(JObj, Context)
                      ,resp_status=success
                     };
                false ->
		    ?LOG_SYS("Invalid or non-unique realm: ~s", [wh_json:get_value(<<"realm">>, JObj)]),
                    crossbar_util:response_invalid_data([<<"realm">>], Context)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an account document from the database
%% @end
%%--------------------------------------------------------------------
-spec(load_account/2 :: (AccountId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
load_account(AccountId, Context) ->
    crossbar_doc:load(AccountId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing account document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec(update_account/2 :: (AccountId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
update_account(AccountId, #cb_context{req_data=Data}=Context) ->
    case is_valid_doc(Data) of
        {false, Fields} ->
	    ?LOG_SYS("Failed to validate JSON"),
             crossbar_util:response_invalid_data(Fields, Context);
        {true, _} ->
            case is_unique_realm(AccountId, Context) of
                true ->
		    %% Update the aggregate accounts DB (/accounts/AB/CB)
		    ?LOG_SYS("Realm is valid for ~s", [AccountId]),
		    crossbar_doc:load_merge(AccountId, Data, Context);
                false ->
		    ?LOG_SYS("Realm isn't valid for ~s", [AccountId]),
                    crossbar_util:response_invalid_data([<<"realm">>], Context)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summary of the parent of the account
%% @end
%%--------------------------------------------------------------------
-spec(load_parent/2 :: (AccountId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
load_parent(AccountId, Context) ->
    View =
        crossbar_doc:load_view(?AGG_VIEW_PARENT, [
             {<<"startkey">>, AccountId}
            ,{<<"endkey">>, AccountId}
        ], Context),
    case View#cb_context.doc of
        [JObj|_] ->
            Parent = wh_json:get_value([<<"value">>, <<"id">>], JObj),
            load_account_summary(Parent, Context);
        _Else ->
            crossbar_util:response_bad_identifier(AccountId, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update the tree with a new parent, cascading when necessary, if the
%% new parent is valid
%% @end
%%--------------------------------------------------------------------
-spec(update_parent/2 :: (AccountId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
update_parent(AccountId, #cb_context{req_data=Data}=Context) ->
    case is_valid_parent(Data) of
        %% {false, Fields} ->
        %%     crossbar_util:response_invalid_data(Fields, Context);
        {true, []} ->
            %% OMGBBQ! NO CHECKS FOR CYCLIC REFERENCES WATCH OUT!
            ParentId = props:get_value(<<"parent">>, element(2, Data)),
            update_tree(AccountId, ParentId, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a summary of the children of this account
%% @end
%%--------------------------------------------------------------------
-spec(load_children/2 :: (AccountId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
load_children(AccountId, Context) ->
    crossbar_doc:load_view(?AGG_VIEW_CHILDREN, [
         {<<"startkey">>, [AccountId]}
        ,{<<"endkey">>, [AccountId, ?EMPTY_JSON_OBJECT]}
    ], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a summary of the descendants of this account
%% @end
%%--------------------------------------------------------------------
-spec(load_descendants/2 :: (AccountId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
load_descendants(AccountId, Context) ->
    crossbar_doc:load_view(?AGG_VIEW_DESCENDANTS, [
         {<<"startkey">>, [AccountId]}
        ,{<<"endkey">>, [AccountId, ?EMPTY_JSON_OBJECT]}
    ], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a summary of the siblngs of this account
%% @end
%%--------------------------------------------------------------------
-spec(load_siblings/2 :: (AccountId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
load_siblings(AccountId, Context) ->
    View =
        crossbar_doc:load_view(?AGG_VIEW_PARENT, [
             {<<"startkey">>, AccountId}
            ,{<<"endkey">>, AccountId}
        ], Context),
    case View#cb_context.doc of
        [JObj|_] ->
            Parent = wh_json:get_value([<<"value">>, <<"id">>], JObj),
            load_children(Parent, Context);
        _Else ->
            crossbar_util:response_bad_identifier(AccountId, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec(normalize_view_results/2 :: (JObj :: json_object(), Acc :: json_objects()) -> json_objects()).
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

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
-spec(is_valid_doc/1 :: (JObj :: json_object()) -> tuple(boolean(), list(binary()))).
is_valid_doc(JObj) ->
    {(wh_json:get_value(<<"realm">>, JObj) =/= undefined), [<<"realm">>]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(update_tree/3 :: (AccountId :: binary(), ParentId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
update_tree(AccountId, ParentId, Context) ->
    case crossbar_doc:load(ParentId, Context) of
        #cb_context{resp_status=success, doc=Parent} ->
            Descendants =
                crossbar_doc:load_view(?AGG_VIEW_DESCENDANTS, [
                     {<<"startkey">>, [AccountId]}
                    ,{<<"endkey">>, [AccountId, ?EMPTY_JSON_OBJECT]}
                ], Context),
            case Descendants of
                #cb_context{resp_status=success, doc=[]} ->
                    crossbar_util:response_bad_identifier(AccountId, Context);
                #cb_context{resp_status=success, doc=Doc}=Context1 ->
                    Tree = wh_json:get_value(<<"pvt_tree">>, Parent) ++ [ParentId, AccountId],
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
    AccountId = props:get_value(<<"id">>, Prop),
    ParentId = lists:last(ParentTree),
    case crossbar_doc:load(AccountId, #cb_context{db_name=?AGG_DB}) of
        #cb_context{resp_status=success, doc=Doc} ->
            Tree = wh_json:get_value(<<"pvt_tree">>, Doc),
            SubTree =
                case lists:dropwhile(fun(E)-> E =/= ParentId end, Tree) of
                    [] -> [];
                    List -> lists:nthtail(1,List)
                end,
            [wh_json:set_value(<<"pvt_tree">>, [E || E <- ParentTree ++ SubTree, E =/= AccountId], Doc) | Acc];
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
-spec(set_private_fields/2 :: (JObj :: json_object(), Context :: #cb_context{}) -> json_object()).
set_private_fields(JObj0, Context) ->
    lists:foldl(fun(Fun, JObj1) ->
                        Fun(JObj1, Context)
                end, JObj0, [fun add_pvt_type/2, fun add_pvt_api_key/2, fun add_pvt_tree/2]).

add_pvt_type(JObj, _) ->
    wh_json:set_value(<<"pvt_type">>, <<"account">>, JObj).

add_pvt_api_key(JObj, _) ->
    wh_json:set_value(<<"pvt_api_key">>, wh_util:to_binary(wh_util:to_hex(crypto:rand_bytes(32))), JObj).

add_pvt_tree(JObj, #cb_context{auth_doc=undefined}) ->
    wh_json:set_value(<<"pvt_tree">>, [], JObj);
add_pvt_tree(JObj, #cb_context{auth_doc=Token}) ->
    AuthAccId = wh_json:get_value(<<"account_id">>, Token),
    case is_binary(AuthAccId) andalso couch_mgr:open_doc(whapps_util:get_db_name(AuthAccId, encoded), AuthAccId) of
        {ok, AuthJObj} ->
            ParentTree = wh_json:get_value(<<"pvt_tree">>, AuthJObj, []),
            wh_json:set_value(<<"pvt_tree">>, ParentTree ++ [AuthAccId], JObj);
        false ->
            wh_json:set_value(<<"pvt_tree">>, [], JObj);
        _ ->
            wh_json:set_value(<<"pvt_tree">>, [AuthAccId], JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will attempt to load the context with the db name of
%% for this account
%% @end
%%--------------------------------------------------------------------
-spec load_account_db/2 :: (AccountId, Context) -> #cb_context{} when
      AccountId :: binary() | [binary(),...],
      Context :: #cb_context{}.
load_account_db([AccountId|_], Context) ->
    load_account_db(AccountId, Context);
load_account_db(AccountId, Context) when is_binary(AccountId) ->
    DbName = whapps_util:get_db_name(AccountId, encoded),
    ?LOG_SYS("Account determined that db name: ~s", [DbName]),
    case couch_mgr:db_exists(DbName) of
        false ->
            Context#cb_context{
                 db_name = <<>>
                ,account_id = <<>>
            };
        true ->
            Context#cb_context{
                db_name = DbName
               ,account_id = AccountId
            }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create a new account and corresponding database
%% then spawn a short initial function
%% @end
%%--------------------------------------------------------------------
-spec(create_new_account_db/1 :: (Context :: #cb_context{}) -> #cb_context{}).
create_new_account_db(#cb_context{doc=Doc}=Context) ->
    DbName = wh_json:get_value(<<"_id">>, Doc, couch_mgr:get_uuid()),
    Db = whapps_util:get_db_name(DbName, encoded),
    case couch_mgr:db_create(Db) of
        false ->
	    ?LOG_SYS("Failed to create database: ~s", [DbName]),
            crossbar_util:response_db_fatal(Context);
        true ->
	    ?LOG_SYS("Created DB for account id ~s", [DbName]),
            JObj = wh_json:set_value(<<"_id">>, DbName, Doc),
            case crossbar_doc:save(Context#cb_context{db_name=Db, doc=JObj}) of
                #cb_context{resp_status=success}=Context1 ->
                    crossbar_bindings:map(<<"account.created">>, Db),
                    couch_mgr:revise_docs_from_folder(Db, crossbar, "account"),
                    whapps_util:replicate_from_account(whapps_util:get_db_name(Db, unencoded), ?AGG_DB, ?AGG_FILTER),
                    Context1;
                Else ->
		    ?LOG_SYS("Other PUT resp: ~s: ~p~n", [Else#cb_context.resp_status, Else#cb_context.doc]),
                    Else
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will determine if the realm in the request is
%% unique or belongs to the request being made
%% @end
%%--------------------------------------------------------------------
-spec(is_unique_realm/2 :: (AccountId :: binary()|undefined, Context :: #cb_context{}) -> boolean()).
is_unique_realm(AccountId, #cb_context{req_data=JObj}=Context) ->
    is_unique_realm(AccountId, Context, wh_json:get_value(<<"realm">>, JObj)).

is_unique_realm(_, _, undefined) -> false;
is_unique_realm(undefined, Context, Realm) ->
    V = case crossbar_doc:load_view(?AGG_GROUP_BY_REALM, [{<<"key">>, Realm}
							  ,{<<"reduce">>, <<"true">>}
							 ]
				    ,Context#cb_context{db_name=?AGG_DB}) of
	    #cb_context{resp_status=success, doc=[J]} -> wh_json:get_value(<<"value">>, J, []);
	    #cb_context{resp_status=success, doc=[]} -> []
	end,
    ?LOG_SYS("Is unique realm: ~s AcctID: undefined V: ~p", [Realm, V]),
    is_unique_realm1(Realm, V);
is_unique_realm(AccountId, Context, Realm) ->
    V = case crossbar_doc:load_view(?AGG_GROUP_BY_REALM, [{<<"key">>, Realm}
							  ,{<<"reduce">>, <<"true">>}
							 ]
				    ,Context#cb_context{db_name=?AGG_DB}) of
	    #cb_context{resp_status=success, doc=[J]} -> wh_json:get_value(<<"value">>, J, []);
	    #cb_context{resp_status=success, doc=[]} -> []
	end,
    ?LOG_SYS("Is unique realm: ~s AcctID: ~s V: ~p", [Realm, AccountId, V]),
    is_unique_realm1(Realm, V).

is_unique_realm1(undefined, [_]) -> false;
is_unique_realm1(undefined, []) -> false;
is_unique_realm1(_, []) -> true;
is_unique_realm1(Realm, [Realm]) -> true;
is_unique_realm1(_, [_]) -> true;
is_unique_realm1(_, _) -> false.
