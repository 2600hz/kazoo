%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%% Servers module
%%%
%%% Handle client requests for server documents
%%%
%%% @end
%%% Created : 05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(servers).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).

-define(VIEW_FILE, <<"views/servers.json">>).
-define(SERVERS_LIST, <<"servers/listing_by_id">>).
-define(DEPLOY_CMD, "knife bootstrap ~s '~s' -x root -P ~s -N ~s -d ~s").


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
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.servers">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.servers">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.servers">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                crossbar_util:binding_heartbeat(Pid),
                Context1 = validate(Params, Context),
                Pid ! {binding_result, true, [RD, Context1, Params]}
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.servers">>, [RD, #cb_context{doc=Doc0}=Context | [_, <<"deploy">>]=Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:binding_heartbeat(Pid),
                  Doc1 = whapps_json:set_value(<<"pvt_deploy_status">>, <<"running">>, Doc0),
                  case crossbar_doc:save(Context#cb_context{doc=Doc1}) of
		      #cb_context{resp_status=success}=Context1 ->
			  spawn(fun() -> execute_deploy_cmd(Context1, Params) end),
			  Pid ! {binding_result, true, [RD, Context1#cb_context{resp_data={struct, []}}, Params]};
		      Else ->
			  Pid ! {binding_result, true, [RD, Else, Params]}
		  end
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.servers">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  Context1 = crossbar_doc:save(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.servers">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  Context1 = crossbar_doc:save(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.servers">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  Context1 = crossbar_doc:delete(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"account.created">>, _Payload}, State) ->    
    Pid ! {binding_result, true, ?VIEW_FILE},
    {noreply, State};

handle_info({binding_fired, Pid, _, Payload}, State) ->
    Pid ! {binding_result, false, Payload},
    {noreply, State};

handle_info(timeout, State) ->
    bind_to_crossbar(),
    accounts:update_all_accounts(?VIEW_FILE),
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
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.servers">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.servers">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.servers">>),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.#.servers">>),
    crossbar_bindings:bind(<<"account.created">>).

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
allowed_methods([_, <<"deploy">>]) ->
    {true, ['POST']};
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
resource_exists([_, <<"deploy">>]) ->
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
    load_server_summary(Context);
validate([], #cb_context{req_verb = <<"put">>}=Context) ->
    create_server(Context);
validate([DocId], #cb_context{req_verb = <<"get">>}=Context) ->
    load_server(DocId, Context);
validate([DocId], #cb_context{req_verb = <<"post">>}=Context) ->
    update_server(DocId, Context);
validate([DocId, <<"deploy">>], #cb_context{req_verb = <<"post">>}=Context) ->   
    case load_server(DocId, Context) of
        #cb_context{resp_status=success, doc=JObj}=Context1 ->
            case whapps_json:get_value(<<"pvt_deploy_status">>, JObj) of
                <<"running">> ->
                    crossbar_util:response(error, <<"deployment already running">>, 409, Context1);
               _Else ->
                    Context1
            end;
        Else ->
            Else
    end;
validate([DocId], #cb_context{req_verb = <<"delete">>}=Context) ->
    load_server(DocId, Context);
validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec(load_server_summary/1 :: (Context :: #cb_context{}) -> #cb_context{}).
load_server_summary(Context) ->
    crossbar_doc:load_view(?SERVERS_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new server document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec(create_server/1 :: (Context :: #cb_context{}) -> #cb_context{}).
create_server(#cb_context{req_data=JObj}=Context) ->
    case is_valid_doc(JObj) of
        {false, Fields} ->
            crossbar_util:response_invalid_data(Fields, Context);
        {true, []} ->
            Doc1=whapps_json:set_value(<<"pvt_type">>, <<"server">>, JObj),
            Doc2=whapps_json:set_value(<<"pvt_deploy_status">>, <<"never_run">>, Doc1),            
            Context#cb_context{
                 doc=Doc2
                ,resp_status=success
            }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a server document from the database
%% @end
%%--------------------------------------------------------------------
-spec(load_server/2 :: (DocId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
load_server(DocId, Context) ->
    crossbar_doc:load(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing server document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec(update_server/2 :: (DocId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
update_server(DocId, #cb_context{req_data=JObj}=Context) ->
    case is_valid_doc(JObj) of
        {false, Fields} ->
            crossbar_util:response_invalid_data(Fields, Context);
        {true, []} ->
            crossbar_doc:load_merge(DocId, JObj, Context)
    end.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec(normalize_view_results/2 :: (Doc :: json_object(), Acc :: json_objects()) -> json_objects()).
normalize_view_results(JObj, Acc) ->
    [whapps_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% NOTICE: This is very temporary, placeholder until the schema work is
%% complete!
%% @end
%%--------------------------------------------------------------------
-spec(is_valid_doc/1 :: (JObj :: json_object()) -> tuple(boolean(), json_objects())).
is_valid_doc(_JObj) ->
    {true, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Execute the deploy command with the provided arguments, ensuring the 
%% doc is updated afterward
%% @end
%%--------------------------------------------------------------------
-spec(execute_deploy_cmd/2 :: (Context :: #cb_context{}, Params :: list(binary())) -> 
                                   tuple(ok, json_object()) | tuple(error, atom())).
execute_deploy_cmd(#cb_context{db_name=Db, req_data=Request}, [DocId, _]) ->        
    try
        Command = io_lib:format(?DEPLOY_CMD, [
                                               whapps_json:get_value(<<"ip">>, Request)
                                              ,lists:map(fun(E) -> <<E/binary, $ >> end, whapps_json:get_value(<<"roles">>, Request))
                                              ,whapps_json:get_value(<<"password">>, Request)
                                              ,whapps_json:get_value(<<"node_name">>, Request)
                                              ,whapps_json:get_value(<<"operating_system">>, Request)
                                             ]),
        logger:format_log(info, "Executing command ~s", [Command]),
        Results = os:cmd(Command),    
        couch_mgr:put_attachment(Db, DocId, <<"deployment.log">>, Results)
    catch 
        _:_ -> 
            ignore
    end,
    {ok, JObj0} = couch_mgr:open_doc(Db, DocId),
    {ok, JObj1} = safe_save(Db, whapps_json:set_value(<<"pvt_deploy_status">>, <<"idle">>, JObj0)),
    safe_save(Db, whapps_json:set_value(<<"pvt_deploy_request">>, Request, JObj1)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loop the save if it is in conflict until it works
%% @end
%%--------------------------------------------------------------------
-spec(safe_save/2 :: (Db :: binary(), JObj :: json_object()) ->
                          tuple(ok, json_object()) | tuple(error, atom())).
safe_save(Db, JObj) ->
    case couch_mgr:save_doc(Db, JObj) of
        {error, conflict} ->
            Rev = couch_mgr:lookup_doc_rev(Db, whapps_json:get_value(<<"_id">>, JObj)),
            safe_save(Db, whapps_json:set_value(<<"_rev">>, Rev, JObj));
        Else ->
            Else
    end.           
