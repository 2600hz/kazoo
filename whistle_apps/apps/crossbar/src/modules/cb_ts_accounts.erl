%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%%
%%% Handle client requests for ts_account documents
%%%
%%% @end
%%% Created : 05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_ts_accounts).

-behaviour(gen_server).

%% API
-export([start_link/0, create_ts_account/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).

-define(TS_DB, <<"ts">>).
-define(VIEW_FILE, <<"views/ts_accounts.json">>).
-define(CB_LIST, <<"ts_accounts/crossbar_listing">>).

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
handle_info({binding_fired, Pid, <<"v1_resource.authorize">>
                 ,{RD, #cb_context{auth_doc=AuthDoc, req_nouns=Nouns, req_verb=Verb, req_id=ReqId}=Context}}, State) ->
    AccountId = wh_json:get_value(<<"account_id">>, AuthDoc, <<"0000000000">>),
    case props:get_value(<<"ts_accounts">>, Nouns) of
        [] when Verb =:= <<"put">> ->
            ?LOG(ReqId, "authorizing request to create a new trunkstore doc", []),
            Pid ! {binding_result, true, {RD, Context}};
        [AccountId] ->
            ?LOG(ReqId, "authorizing request to trunkstore doc ~s", [AccountId]),
            Pid ! {binding_result, true, {RD, Context}};
        _ ->
            Pid ! {binding_result, false, {RD, Context}}
    end,
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.ts_accounts">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.ts_accounts">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.ts_accounts">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
		  crossbar_util:binding_heartbeat(Pid),
		  Context1 = validate(Params, Context#cb_context{db_name=?TS_DB}),
		  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.ts_accounts">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  #cb_context{doc=Doc} = Context1 = crossbar_doc:save(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]},
                  timer:sleep(1000),
                  try stepswitch_maintenance:reconcile(wh_json:get_value(<<"_id">>, Doc), true) catch _:_ -> ok end
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.ts_accounts">>
                 ,[RD, #cb_context{auth_doc=AuthDoc, doc=Doc}=Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  AccountId = wh_json:get_value(<<"account_id">>, AuthDoc, <<"undefined">>),
                  Context1 = crossbar_doc:save(Context#cb_context{doc=wh_json:set_value(<<"_id">>, AccountId, Doc)}),
                  Pid ! {binding_result, true, [RD, Context1, Params]},
                  timer:sleep(1000),
                  try stepswitch_maintenance:reconcile(AccountId, true) catch _:_ -> ok end
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.ts_accounts">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  #cb_context{doc=Doc} = Context1 = crossbar_doc:delete(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]},
                  %% TODO: THIS IS VERY WRONG! Ties a local crossbar to a LOCAL stepswitch instance... quick and
                  %% dirty were the instructions for this module but someone PLEASE fix this later!
                  timer:sleep(1000),
                  try stepswitch_maintenance:reconcile(wh_json:get_value(<<"_id">>, Doc), true) catch _:_ -> ok end
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, _, Payload}, State) ->
    Pid ! {binding_result, false, Payload},
    {noreply, State};

handle_info(timeout, State) ->
    bind_to_crossbar(),
    couch_mgr:revise_doc_from_file(?TS_DB, crossbar, ?VIEW_FILE),
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
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.ts_accounts">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.ts_accounts">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.ts_accounts">>),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.#.ts_accounts">>).

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
    {true, ['GET', 'POST', 'DELETE', 'HEAD']};
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
    read_ts_account_summary(Context);
validate([], #cb_context{req_verb = <<"put">>}=Context) ->
    create_ts_account(Context);
validate([TSAccountId], #cb_context{req_verb = <<"get">>}=Context) ->
    read_ts_account(TSAccountId, Context);
validate([TSAccountId], #cb_context{req_verb = <<"post">>}=Context) ->
    update_ts_account(TSAccountId, Context);
validate([TSAccountId], #cb_context{req_verb = <<"delete">>}=Context) ->
    read_ts_account(TSAccountId, Context);
validate([TSAccountId], #cb_context{req_verb = <<"head">>}=Context) ->
    check_ts_account(TSAccountId, Context);
validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new ts_account document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec(create_ts_account/1 :: (Context :: #cb_context{}) -> #cb_context{}).
create_ts_account(#cb_context{req_data=JObj1}=Context) ->
    case is_valid_doc(JObj1) of
        {false, Fields} ->
            crossbar_util:response_invalid_data(Fields, Context);
        {true, []} ->
            JObj2 = wh_json:set_value(<<"type">>, <<"sys_info">>, JObj1),
            Context#cb_context{doc=wh_json:set_value(<<"pvt_type">>, <<"sip_service">>, JObj2)
                               ,resp_status=success
                              }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a ts_account document from the database
%% @end
%%--------------------------------------------------------------------
-spec(read_ts_account/2 :: (TSAccountId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
read_ts_account(TSAccountId, Context) ->
    crossbar_doc:load(TSAccountId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing ts_account document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec(update_ts_account/2 :: (TSAccountId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
update_ts_account(TSAccountId, #cb_context{req_data=JObj}=Context) ->
    case is_valid_doc(JObj) of
        {false, Fields} ->
            crossbar_util:response_invalid_data(Fields, Context);
        {true, []} ->
            crossbar_doc:load_merge(TSAccountId, JObj, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec(read_ts_account_summary/1 :: (Context :: #cb_context{}) -> #cb_context{}).
read_ts_account_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to determine if the account exists in as light weight as
%% possible, altho just getting here...
%% @end
%%--------------------------------------------------------------------
-spec check_ts_account/2 :: (TSAccountId, Context) -> #cb_context{} when
      TSAccountId :: binary(),
      Context :: #cb_context{}.
check_ts_account(TSAccountId, #cb_context{db_name=Db}=Context) ->
    case couch_mgr:lookup_doc_rev(Db, TSAccountId) of
        {ok, Rev} ->
            Context#cb_context{resp_status=success
                               ,resp_data=[]
                               ,resp_etag=wh_util:to_list(Rev)};
        {error, _} ->
            crossbar_util:response_bad_identifier(TSAccountId, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec(normalize_view_results/2 :: (Doc :: json_object(), Acc :: json_objects()) -> json_objects()).
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% NOTICE: This is very temporary, placeholder until the schema work is
%% complete!
%% @end
%%--------------------------------------------------------------------
-spec is_valid_doc/1 :: (JObj :: json_object()) -> {boolean(), [binary(),...] | []}.
is_valid_doc(JObj) ->
    case wh_json:get_value(<<"account">>, JObj) of
	undefined ->
	    {false, [<<"account">>]};
	_ ->
	    {true, []}
    end.
