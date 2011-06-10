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
handle_info({binding_fired, Pid, <<"v1_resource.content_types_provided.servers">>, {RD, Context, [_, <<"deployment">>]=Params}}, State) ->
    spawn(fun() ->
                  Pid ! {binding_result, true, {RD, content_types_provided(Context), Params}}
	  end),
    {noreply, State};

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

handle_info({binding_fired, Pid, <<"v1_resource.execute.get.servers">>, [RD, Context | [_, <<"deployment">>]=Params]}, State) ->
	    spawn(fun() ->
			  Context1 = Context#cb_context{resp_headers = [{<<"Content-Type">>, <<"text/plain">>}
									,{<<"Content-Length">>
									      ,whistle_util:to_binary(binary:referenced_byte_size(Context#cb_context.resp_data))}
									| Context#cb_context.resp_headers]},
			  Pid ! {binding_result, true, [RD, Context1, Params]}
		  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.servers">>, [RD, Context | [_, <<"deployment">>]=Params]}, State) ->
    spawn(fun() ->
                  %% The validation will stop us before we get here if the deploy is running, 
                  %% and we will not execute unless we are able to update the document (using the doc
                  %% as a mutex basicly).  However, if a server achieves the 'lock' and dies nothing
                  %% cleans it up at the moment.
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = execute_deploy_cmd(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
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
    whapps_util:update_all_accounts(?VIEW_FILE),
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
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_provided.servers">>),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.servers">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.servers">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.servers">>),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.#.servers">>),
    crossbar_bindings:bind(<<"account.created">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update the content type so users can pull down the deploy log as
%% plain text
%% @end
%%--------------------------------------------------------------------
-spec(content_types_provided/1 :: (Context :: #cb_context{}) -> #cb_context{}).
content_types_provided(#cb_context{req_verb = <<"get">>, content_types_provided=CTP}=Context) ->
    Context#cb_context{content_types_provided=[{to_binary, ["text/plain"] ++ props:get_value(to_binary, CTP, [])} | proplists:delete(to_binary,CTP)]};
content_types_provided(Context) -> Context.

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
allowed_methods([_, <<"deployment">>]) ->
    {true, ['PUT', 'GET']};
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
resource_exists([_, <<"deployment">>]) ->
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
validate([ServerId], #cb_context{req_verb = <<"get">>}=Context) ->
    load_server(ServerId, Context);
validate([ServerId], #cb_context{req_verb = <<"post">>}=Context) ->
    update_server(ServerId, Context);
validate([ServerId, <<"deployment">>], #cb_context{req_verb = <<"put">>}=Context) ->   
    case load_server(ServerId, Context) of
        #cb_context{resp_status=success, doc=JObj}=Context1 ->
            case wh_json:get_value(<<"pvt_deploy_status">>, JObj) of
                <<"running">> ->
                    crossbar_util:response(error, <<"deployment already running">>, 409, Context1);
               _Else ->
                    Context1
            end;
        Else ->
            Else
    end;
validate([ServerId, <<"deployment">>], #cb_context{req_verb = <<"get">>}=Context) ->
    crossbar_doc:load_attachment(ServerId, <<"deployment.log">>, Context);
validate([ServerId], #cb_context{req_verb = <<"delete">>}=Context) ->
    load_server(ServerId, Context);
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
        {true, _} ->
            JObj1=wh_json:set_value(<<"pvt_type">>, <<"server">>, JObj),
            JObj2=wh_json:set_value(<<"pvt_deploy_status">>, <<"never_run">>, JObj1),            
            JObj3=wh_json:set_value(<<"pvt_db_key">>, whistle_util:to_binary(whistle_util:to_hex(crypto:rand_bytes(5))), JObj2),
            JObj4=wh_json:set_value(<<"pvt_cookie">>, whistle_util:to_binary(whistle_util:to_hex(crypto:rand_bytes(24))), JObj3),
            Context#cb_context{
                 doc=JObj4
                ,resp_status=success
            }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a server document from the database
%% @end
%%--------------------------------------------------------------------
-spec(load_server/2 :: (ServerId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
load_server(ServerId, Context) ->
    crossbar_doc:load(ServerId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing server document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec(update_server/2 :: (ServerId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
update_server(ServerId, #cb_context{req_data=JObj}=Context) ->
    case is_valid_doc(JObj) of
        {false, Fields} ->
            crossbar_util:response_invalid_data(Fields, Context);
        {true, _} ->
            crossbar_doc:load_merge(ServerId, JObj, Context)
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
-spec(is_valid_doc/1 :: (JObj :: json_object()) -> tuple(boolean(), list(binary()))).
is_valid_doc(JObj) ->
    {(wh_json:get_value(<<"hostname">>, JObj) =/= undefined), [<<"hostname">>]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Execute the deploy command with the provided arguments, ensuring the 
%% doc is updated afterward.  Because the doc is are mutex for the 
%% deployment there are try..catches so any failures release the lock
%% @end
%%--------------------------------------------------------------------
-spec(execute_deploy_cmd/1 :: (Context :: #cb_context{}) -> #cb_context{}).
execute_deploy_cmd(#cb_context{db_name=Db, doc=JObj, req_data=Data}=Context) ->
    case couch_mgr:save_doc(Db, wh_json:set_value(<<"pvt_deploy_status">>, <<"running">>, JObj)) of
        {ok, _} ->
            ServerId = wh_json:get_value(<<"_id">>, JObj),                
            try 
                Ip = wh_json:get_value(<<"ip">>, JObj),
                Roles = fun([H|T], Sep) -> 
                                <<H/binary, (list_to_binary([<<(Sep)/binary, X/binary>> || X <- T]))/binary>> 
                        end(wh_json:get_value(<<"roles">>, JObj), <<",">>),
                Password = wh_json:get_value(<<"password">>, Data),
                Hostname = wh_json:get_value(<<"hostname">>, JObj),
                OS = wh_json:get_value(<<"operating_system">>, JObj),
                AccountId = whapps_util:get_db_name(Db, raw),
                Cmd = whistle_util:to_list(<<(whistle_util:to_binary(code:priv_dir(crossbar)))/binary
                                             ,"/deploy.sh"
                                             ,$ , $" ,Ip/binary, $"
                                             ,$ , $" ,Roles/binary, $"
                                             ,$ , $" ,Password/binary, $"
                                             ,$ , $" ,Hostname/binary, $"
                                             ,$ , $" ,OS/binary, $"
                                             ,$ , $" ,ServerId/binary, $"
                                             ,$ , $" ,AccountId/binary, $"
                                             ,$ , $" ,(wh_json:get_value(<<"pvt_cookie">>, JObj))/binary, $"
                                             ,$ , $" ,(wh_json:get_value(<<"pvt_db_key">>, JObj))/binary, $"
                                             ,$ , $" ,(Db)/binary, $"
                                           >>),                
               spawn(fun() -> 
                             try
                                 Port = open_port({spawn, Cmd}, [stderr_to_stdout, {line, 256}, exit_status, binary]),
                                 monitor_deployment(Port, Db, ServerId, <<>>)
                             catch
                                 _:_ ->
                                     ignore
                             end,
                             {ok, _} = mark_deploy_complete(Db, ServerId)
                     end),
                crossbar_util:response([], Context)
            catch 
                _:_ ->
                    _ = mark_deploy_complete(Db, ServerId),
                    crossbar_util:response(error, <<"failed to start deployment">>, Context)
            end;
        {error, _} ->
            crossbar_util:response(error, <<"could not lock deployment">>, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loop the save if it is in conflict until it works
%% @end
%%--------------------------------------------------------------------
-spec(mark_deploy_complete/2 :: (Db :: binary(), ServerId :: binary()) ->
                          tuple(ok, json_object()) | tuple(error, atom())).
mark_deploy_complete(Db, ServerId) ->
    {ok, JObj} = couch_mgr:open_doc(Db, ServerId),
    couch_mgr:ensure_saved(Db, wh_json:set_value(<<"pvt_deploy_status">>, <<"idle">>, JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This is the db trashing function, every new line update the attachment
%% TODO: implement batching
%% @end
%%--------------------------------------------------------------------                   
-spec(monitor_deployment/4 :: (Port :: port(), Db :: binary(), ServerId :: binary(), Acc :: binary()) -> no_return()).
monitor_deployment(Port, Db, ServerId, Acc) ->    
    receive
        {Port, {data, {eol, Bin}}} ->            
            Acc1 = <<Acc/binary, Bin/binary>>,
            couch_mgr:put_attachment(Db, ServerId, <<"deployment.log">>, Acc1),
            monitor_deployment(Port, Db, ServerId, Acc1);
        {Port, {data, {noeol, Bin}}} ->
            Acc1 = <<Acc/binary, Bin/binary>>,
            couch_mgr:put_attachment(Db, ServerId, <<"deployment.log">>, Acc1),
            monitor_deployment(Port, Db, ServerId, Acc1);
        {Port, {data, Bin}} ->
            Acc1 = <<Acc/binary, Bin/binary>>,
            couch_mgr:put_attachment(Db, ServerId, <<"deployment.log">>, Acc1),
            monitor_deployment(Port, Db, ServerId, Acc1);
        {Port, {exit_status, _}} ->
            couch_mgr:put_attachment(Db, ServerId, <<"deployment.log">>, Acc);
        {Port, eof} ->
            port_close(Port),
            couch_mgr:put_attachment(Db, ServerId, <<"deployment.log">>, Acc)   
    end.
