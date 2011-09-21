%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Servers module
%%%
%%% Handle client requests for server documents
%%%
%%% @end
%%% Created : 05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_servers).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([reload/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-define(SERVER, ?MODULE).

-define(SERVER_CONF, list_to_binary([code:lib_dir(crossbar, priv), "/servers/servers.conf"])).

-define(CB_LIST, <<"servers/crossbar_listing">>).
-define(VIEW_DEPLOY_ROLES, <<"servers/list_deployment_roles">>).

-record(state, {data_bag_tmpl=undefined
                ,role_tmpl=undefined
                ,prod_deploy_tmpl=undefined
                ,dev_deploy_tmpl=undefined
                ,dev_role = <<"all_in_one">>
                ,role_path_tmpl=undefined
                ,databag_path_tmpl=undefined
                ,databag_mapping=undefined
                ,delete_tmpl=undefined}).

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

reload() ->
    gen_server:cast(?SERVER, {reload}).

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
    {ok, #state{}, 0}.

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
handle_cast({reload}, State) ->
    {noreply, State, 0};

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
                 ,{RD, #cb_context{req_nouns=[{<<"servers">>, [_,<<"deployment">>]},
                                              {<<"accounts">>,[_]}]
                                   ,req_verb = <<"post">>
                                   ,req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authorizing request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authenticate">>
                 ,{RD, #cb_context{req_nouns=[{<<"servers">>, [_,<<"deployment">>]},
                                              {<<"accounts">>,[_]}]
                                   ,req_verb = <<"post">>
                                   ,req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authenticate request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.content_types_provided.servers">>, {RD, Context, [_, <<"log">>]=Params}}, State) ->
    spawn(fun() ->
                  Pid ! {binding_result, true, {RD, content_types_provided(Context), Params}}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.content_types_provided.servers">>, {RD, Context, Params}}, State) ->
    spawn(fun() ->
                  Pid ! {binding_result, true, {RD, Context, Params}}
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
                  crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = validate(Params, RD, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.servers">>, [RD, Context | [_, <<"deployment">>]=Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  Context2 = case crossbar_doc:save(Context) of
                                 #cb_context{resp_status=success, doc=JObj}=Context1 ->
                                     Context1#cb_context{resp_data={struct, [{<<"status">>, wh_json:get_value(<<"pvt_deploy_status">>, JObj)}
                                                                             ,{<<"log">>,wh_json:get_value(<<"pvt_deploy_log">>, JObj)}]}};
                                 Else -> Else
                             end,
                  Pid ! {binding_result, true, [RD, Context2, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.servers">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  Context1 = crossbar_doc:save(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.servers">>, [RD, Context | [_, <<"deployment">>]=Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  Context1 = execute_deploy_command(Context, State),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.servers">>, [RD, #cb_context{doc=Doc}=Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  Id = wh_util:to_binary(wh_util:to_hex(crypto:md5([wh_json:get_value(<<"ip">>, Doc), wh_json:get_value(<<"ssh_port">>, Doc)]))),
                  Context1 = crossbar_doc:save(Context#cb_context{doc=wh_json:set_value(<<"_id">>, Id, Doc)}),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.servers">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  case crossbar_doc:delete(Context, permanent) of
                      #cb_context{resp_status=success}=Context1 ->
                          execute_delete_command(Context, State),
                          Pid ! {binding_result, true, [RD, Context1, Params]};
                      Else ->
                          Pid ! {binding_result, true, [RD, Else, Params]}
                  end

	  end),
    {noreply, State};

handle_info({binding_fired, Pid, _, Payload}, State) ->
    Pid ! {binding_result, false, Payload},
    {noreply, State};

handle_info(timeout, _) ->
    {ok, State} = code_change(0, undefined, []),
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
code_change(_OldVsn, _State, _Extra) ->
    _ = bind_to_crossbar(),
    State = case file:consult(?SERVER_CONF) of
                {ok, Terms} ->
                    ?LOG_SYS("loaded config from ~s", [?SERVER_CONF]),
                    #state{data_bag_tmpl =
                               compile_template(props:get_value(data_bag_tmpl, Terms), cb_servers_data_bag)
                           ,databag_mapping =
                               props:get_value(databag_mapping, Terms, [])
                           ,databag_path_tmpl =
                               compile_template(props:get_value(databag_path_tmpl, Terms), cb_servers_databag_path_tmpl)
                           ,role_tmpl =
                               compile_template(props:get_value(role_tmpl, Terms), cb_servers_role_tmpl)
                           ,role_path_tmpl =
                               compile_template(props:get_value(role_path_tmpl, Terms), cb_servers_role_path_tmpl)
                           ,prod_deploy_tmpl =
                               compile_template(props:get_value(prod_deploy_tmpl, Terms), cb_servers_prod_deploy_tmpl)
                           ,dev_deploy_tmpl =
                               compile_template(props:get_value(dev_deploy_tmpl, Terms), cb_servers_dev_deploy_tmpl)
                           ,dev_role =
                               wh_util:to_binary(props:get_value(dev_role, Terms, <<"all_in_one">>))
                           ,delete_tmpl =
                               compile_template(props:get_value(delete_tmpl, Terms), cb_server_delete_tmpl)
                          };
                {error, _} ->
                    ?LOG_SYS("could not read config from ~s", [?SERVER_CONF]),
                    #state{}
            end,
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
-spec bind_to_crossbar/0 :: () ->  no_return().
bind_to_crossbar() ->
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_provided.servers">>),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.servers">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.servers">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.servers">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.servers">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update the content type so users can pull down the deploy log as
%% plain text
%% @end
%%--------------------------------------------------------------------
-spec(content_types_provided/1 :: (Context :: #cb_context{}) ->
                                        #cb_context{}).
content_types_provided(#cb_context{req_verb = <<"get">>, content_types_provided=CTP}=Context) ->
    Context#cb_context{content_types_provided=[{to_binary, ["text/plain"] ++ props:get_value(to_binary, CTP, [])}
                                               | proplists:delete(to_binary,CTP)]};
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
-spec allowed_methods/1 :: (Paths) -> {boolean(), http_methods()} when
      Paths :: list().
allowed_methods([]) ->
    {true, ['GET', 'PUT']};
allowed_methods([_]) ->
    {true, ['GET', 'POST', 'DELETE']};
allowed_methods([_, <<"deployment">>]) ->
    {true, ['GET', 'POST', 'PUT']};
allowed_methods([_, <<"log">>]) ->
    {true, ['GET']};
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
resource_exists([_, <<"log">>]) ->
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
-spec(validate/3 :: (Params :: list(), RD :: #wm_reqdata{}, Context :: #cb_context{}) -> #cb_context{}).
validate([], RD, #cb_context{req_verb = <<"get">>}=Context) ->
    load_server_summary(Context, wrq:req_qs(RD));
validate([], _, #cb_context{req_verb = <<"put">>}=Context) ->
    create_server(Context);
validate([ServerId], _, #cb_context{req_verb = <<"get">>}=Context) ->
    load_server(ServerId, Context);
validate([ServerId], _, #cb_context{req_verb = <<"post">>}=Context) ->
    update_server(ServerId, Context);
validate([ServerId], _, #cb_context{req_verb = <<"delete">>}=Context) ->
    load_server(ServerId, Context);
validate([ServerId, <<"deployment">>], _, #cb_context{req_verb = <<"get">>}=Context) ->
    case load_server(ServerId, Context) of
        #cb_context{resp_status=success, doc=JObj}=Context1 ->
            Context1#cb_context{resp_data={struct, [{<<"status">>, wh_json:get_value(<<"pvt_deploy_status">>, JObj)}
                                                    ,{<<"log">>,wh_json:get_value(<<"pvt_deploy_log">>, JObj)}]}};
        Else -> Else
    end;
validate([ServerId, <<"deployment">>], _, #cb_context{req_verb = <<"post">>}=Context) ->
    case load_server(ServerId, Context) of
        #cb_context{resp_status=success, doc=JObj, req_data=Data}=Context1 ->
            DeployLog = [Data | wh_json:get_value(<<"pvt_deploy_log">>, JObj, [])],
            Context1#cb_context{doc=wh_json:set_value(<<"pvt_deploy_log">>, DeployLog, JObj)};
        Else -> Else
    end;
validate([ServerId, <<"deployment">>], _, #cb_context{req_verb = <<"put">>}=Context) ->
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
validate([ServerId, <<"log">>], _, #cb_context{req_verb = <<"get">>}=Context) ->
    crossbar_doc:load_attachment(ServerId, <<"deployment.log">>, Context);
validate(_, _, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_server_summary/2 :: (Context, QueryParams) -> #cb_context{} when
      Context :: #cb_context{},
      QueryParams :: proplist().
load_server_summary(Context, []) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2);
load_server_summary(#cb_context{db_name=DbName}=Context, QueryParams) ->
    Result = crossbar_filter:filter_on_query_string(DbName, ?CB_LIST, QueryParams),
    Context#cb_context{resp_data=Result, resp_status=success}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new server document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_server/1 :: (Context) -> #cb_context{} when
      Context :: #cb_context{}.
create_server(#cb_context{req_data=JObj}=Context) ->
    case is_valid_doc(JObj) of
        {false, Fields} ->
            crossbar_util:response_invalid_data(Fields, Context);
        {true, _} ->
            Funs = [fun(Obj) -> wh_json:set_value(<<"pvt_deploy_status">>, <<"never_run">>, Obj) end,
                    fun(Obj) -> wh_json:set_value(<<"pvt_deploy_log">>, [], Obj) end,
                    fun(Obj) -> wh_json:set_value(<<"pvt_type">>, <<"server">>, Obj) end],
            Context#cb_context{resp_status=success, doc=lists:foldl(fun(Fun, Obj) -> Fun(Obj) end, JObj, Funs)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a server document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_server/2 :: (ServerId, Context) -> #cb_context{} when
      ServerId :: binary(),
      Context :: #cb_context{}.
load_server(ServerId, Context) ->
    crossbar_doc:load(ServerId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing server document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_server/2 :: (ServerId, Context) -> #cb_context{} when
      ServerId :: binary(),
      Context :: #cb_context{}.
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
-spec normalize_view_results/2 :: (Doc, Acc) -> json_objects() when
      Doc :: json_object(),
      Acc :: json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% NOTICE: This is very temporary, placeholder until the schema work is
%% complete!
%% @end
%%--------------------------------------------------------------------
-spec is_valid_doc/1 :: (JObj) -> {boolean(), [binary(),...]} when
      JObj :: json_object().
is_valid_doc(JObj) ->
    {(wh_json:get_value(<<"hostname">>, JObj) =/= undefined), [<<"hostname">>]}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Optional command template to execute on the deletion of a server
%% @end
%%--------------------------------------------------------------------
-spec execute_delete_command/2 :: (Context, State) -> ok when
      Context :: #cb_context{},
      State :: #state{}.
execute_delete_command(_, #state{delete_tmpl=undefined}) ->
    ?LOG("no delete template defined"),
    ok;
execute_delete_command(#cb_context{doc=JObj}, #state{delete_tmpl=DeleteTmpl}) ->
    Props = wh_json:to_proplist(JObj),
    {ok, C} = DeleteTmpl:render([{<<"server">>, Props}]),
    Cmd = binary_to_list(iolist_to_binary(C)),
    ?LOG("executing delete template: ~s", [Cmd]),
    Res = os:cmd(Cmd),
    ?LOG("Deleting template resulted in ~s", [Cmd, Res]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Execute the deploy command with the provided arguments, ensuring the
%% doc is updated afterward.  Because the doc is a "semaphore" for the
%% deployment
%% @end
%%--------------------------------------------------------------------
-spec execute_deploy_command/2 :: (Context, State) -> #cb_context{} when
      Context :: #cb_context{},
      State :: #state{}.
execute_deploy_command(Context, #state{dev_deploy_tmpl=undefined}) ->
    ?LOG("no development deploy template defined"),
    crossbar_util:response(error, <<"failed to start deployment">>, Context);
execute_deploy_command(Context, #state{prod_deploy_tmpl=undefined}) ->
    ?LOG("no production deploy template defined"),
    crossbar_util:response(error, <<"failed to start deployment">>, Context);
execute_deploy_command(#cb_context{db_name=Db, doc=JObj}=Context, State) ->
    ServerId = wh_json:get_value(<<"_id">>, JObj),
    Props = template_props(Context, State),
    CmdTmpl = get_command_tmpl(Context, State),
    {ok, C} = CmdTmpl:render(Props),
    Cmd = binary_to_list(iolist_to_binary(C)),
    case mark_deploy_running(Db, ServerId) of
        {ok, _} ->
            spawn(fun() ->
                          ?LOG("executing deploy command ~s", [Cmd]),
                          Res = os:cmd(Cmd),
                          ?LOG("deploy command execution completed", []),
                          {ok, _} = mark_deploy_complete(Db, ServerId),
                          ?LOG("attempting to upload log ~s", [Res]),
                          {ok, Log} = file:read_file(Res),
                          couch_mgr:put_attachment(Db, ServerId, <<"deployment.log">>, Log)
                  end),
            crossbar_util:response([], Context);
        {error, _} ->
            crossbar_util:response(error, <<"could not lock deployment">>, Context)
    end.


get_command_tmpl(#cb_context{doc=JObj}, #state{dev_role=DevRole}=State) ->
    Roles = wh_json:get_value(<<"roles">>, JObj, []),
    case lists:member(DevRole, Roles) of
        true ->
            ?LOG("use development template"),
            State#state.dev_deploy_tmpl;
        false ->
            ?LOG("use production template"),
            State#state.prod_deploy_tmpl
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create a proplist to provide to the templates during render
%% @end
%%--------------------------------------------------------------------
-spec template_props/2 :: (Context, State) -> proplist() when
      State :: #state{},
      Context :: #cb_context{}.
template_props(#cb_context{doc=JObj, req_data=Data, db_name=Db}=Context, State) ->
    Server = wh_json:to_proplist(JObj),
    Servers = case couch_mgr:get_results(Db, ?CB_LIST, [{<<"include_docs">>, true}]) of
                  {ok, Srvs} ->
                      [wh_json:to_proplist(wh_json:get_value(<<"doc">>, Srv))
                       || Srv <- Srvs];
                  {error, _} -> []
              end,
    Account = case couch_mgr:open_doc(Db, whapps_util:get_db_name(Db, raw)) of
                  {ok, A} -> wh_json:to_proplist(A);
                  {error, _} -> []
              end,
    {Role, RolePath} = case couch_mgr:get_results(Db, ?VIEW_DEPLOY_ROLES, [{<<"include_docs">>, true}]) of
                           {ok, []} ->
                               R = create_role(Account, Context, State),
                               {wh_json:to_proplist(R), write_role(Account, Server, R, State)};
                           {ok, [R|_]} ->
                               R2 = wh_json:get_value(<<"doc">>, R),
                               {wh_json:to_proplist(R2), write_role(Account, Server, R2, State)};
                           {error, _} ->
                               R = create_role(Account, Context, State),
                               {wh_json:to_proplist(R), write_role(Account, Server, R, State)}
                       end,
    DatabagBase = wh_json:set_value(<<"id">>, props:get_value(<<"_id">>, Account), ?EMPTY_JSON_OBJECT),
    Databag = create_databag(Servers, State, DatabagBase),
    DatabagPath = write_databag(Account, Server, Databag, State),
    %% create props to expose to the template
    [{<<"account">>, Account}
     ,{<<"role">>, Role}
     ,{<<"role_path">>, RolePath}
     ,{<<"databag">>, wh_json:to_proplist(Databag)}
     ,{<<"databag_path">>, DatabagPath}
     ,{<<"request">>, wh_json:to_proplist(Data)}
     ,{<<"servers">>, Servers}
     ,{<<"server">>, Server}
     ,{<<"host">>, wh_util:to_binary(net_adm:localhost())}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates the role document (when it doesnt exist)
%% @end
%%--------------------------------------------------------------------
-spec create_role/3 :: (Account, Context, State) -> json_object() when
      Account :: proplist(),
      Context :: #cb_context{},
      State :: #state{}.
create_role(_, _, #state{role_tmpl=undefined}) ->
    [];
create_role(Account, #cb_context{db_name=Db}, #state{role_tmpl=RoleTmpl}) ->
    try
        Props = [{<<"account">>, Account}
                 ,{<<"host">>, wh_util:to_binary(net_adm:localhost())}
                 %% The list index syntax of erlydtl doesnt seem to compile
                 ,{<<"rand_small_1">>, wh_util:to_hex(crypto:rand_bytes(8))}
                 ,{<<"rand_small_2">>, wh_util:to_hex(crypto:rand_bytes(8))}
                 ,{<<"rand_small_3">>, wh_util:to_hex(crypto:rand_bytes(8))}
                 ,{<<"rand_small_4">>, wh_util:to_hex(crypto:rand_bytes(8))}
                 ,{<<"rand_small_5">>, wh_util:to_hex(crypto:rand_bytes(8))}
                 ,{<<"rand_large_1">>, wh_util:to_hex(crypto:rand_bytes(24))}
                 ,{<<"rand_large_2">>, wh_util:to_hex(crypto:rand_bytes(24))}
                 ,{<<"rand_large_3">>, wh_util:to_hex(crypto:rand_bytes(24))}
                 ,{<<"rand_large_4">>, wh_util:to_hex(crypto:rand_bytes(24))}
                 ,{<<"rand_large_5">>, wh_util:to_hex(crypto:rand_bytes(24))}
                ],
        {ok, Role} = RoleTmpl:render(Props),
        JObj = mochijson2:decode(binary_to_list(iolist_to_binary(Role))),
        {ok, R} = couch_mgr:save_doc(Db, wh_json:set_value(<<"pvt_type">>, <<"deployment_role">>, JObj)),
        R
    catch
        _:_ ->
            ?EMPTY_JSON_OBJECT
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Writes the current databag json object to a file as defined by
%% databag_path_tmpl, then returns the path
%%
%% TODO: this cant be a template (the databag contents) yet...
%% @end
%%--------------------------------------------------------------------
-spec write_databag/4 :: (Account, Server, JObj, State) -> list() when
      Account :: proplist(),
      Server :: proplist(),
      JObj :: json_object(),
      State :: #state{}.
write_databag(_, _, _, #state{databag_path_tmpl=undefined}) ->
    [];
write_databag(Account, Server, JObj, #state{databag_path_tmpl=PathTmpl}) ->
    JSON = mochijson2:encode(crossbar_doc:public_fields(JObj)),
    Props = [{<<"account">>, Account}
             ,{<<"server">>, Server}],
    {ok, P} = PathTmpl:render(Props),
    Path = binary_to_list(iolist_to_binary(P)),
    ?LOG("writing databag to ~s", [Path]),
    ok = file:write_file(Path, JSON),
    Path.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates a databag for this deployment
%% @end
%%--------------------------------------------------------------------
-spec create_databag/3 :: (Servers, State, JObj) -> json_object() when
      Servers :: list(),
      State :: #state{},
      JObj :: json_object().
create_databag([], _, JObj) ->
    JObj;
create_databag([H|T], #state{databag_mapping=Mapping}=State, JObj) ->
    Roles = props:get_value(<<"roles">>, H, []),
    Hostname = wh_json:get_value(<<"hostname">>, H),
    IP = wh_json:get_value(<<"ip">>, H),
    NewJ = lists:foldr(fun(Role, J) ->
                               case proplists:get_value(Role, Mapping) of
                                   undefined ->
                                       J;
                                   Name ->
                                       wh_json:set_value([Name, <<"servers">>, Hostname], IP, J)
                               end
                       end, JObj, Roles),
    create_databag(T, State, NewJ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Writes the existing role json object to a file as defined by
%% role_path_tmpl, then returns the path
%% @end
%%--------------------------------------------------------------------
-spec write_role/4 :: (Account, Server, JObj, State) -> list() when
      Account :: proplist(),
      Server :: proplist(),
      JObj :: json_object(),
      State :: #state{}.
write_role(_, _, _, #state{role_path_tmpl=undefined}) ->
    [];
write_role(Account, Server, JObj, #state{role_path_tmpl=PathTmpl}) ->
    JSON = mochijson2:encode(crossbar_doc:public_fields(JObj)),
    Props = [{<<"account">>, Account}
             ,{<<"server">>, Server}],
    {ok, P} = PathTmpl:render(Props),
    Path = binary_to_list(iolist_to_binary(P)),
    ?LOG("writing role to ~s", [Path]),
    ok = file:write_file(Path, JSON),
    Path.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reset the deploy log and set the status to "running", if this
%% conflicts it will require another request
%% @end
%%--------------------------------------------------------------------
-spec mark_deploy_running/2 :: (Db, ServerId) -> tuple(ok, json_object())
                                                     | tuple(error, atom()) when
      Db :: binary(),
      ServerId :: binary().
mark_deploy_running(Db, ServerId) ->
    {ok, JObj} = couch_mgr:open_doc(Db, ServerId),
    case wh_json:get_value(<<"pvt_deploy_status">>, JObj) of
        <<"running">> ->
            {error, already_running};
        _ ->
            Funs = [fun(Obj) -> wh_json:set_value(<<"pvt_deploy_status">>, <<"running">>, Obj) end,
                    fun(Obj) -> wh_json:set_value(<<"pvt_deploy_log">>, [], Obj) end],
            couch_mgr:save_doc(Db, lists:foldl(fun(Fun, Obj) -> Fun(Obj) end, JObj, Funs))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loop the save if it is in conflict until it works
%% @end
%%--------------------------------------------------------------------
-spec mark_deploy_complete/2 :: (Db, ServerId) -> tuple(ok, json_object())
                                                      | tuple(error, atom()) when
      Db :: binary(),
      ServerId :: binary().
mark_deploy_complete(Db, ServerId) ->
    {ok, JObj} = couch_mgr:open_doc(Db, ServerId),
    couch_mgr:ensure_saved(Db, wh_json:set_value(<<"pvt_deploy_status">>, <<"idle">>, JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Compiles a template string or path, correcting relative paths
%% to the priv directory of this module
%% @end
%%--------------------------------------------------------------------
-spec compile_template/2 :: (Template, Name) -> undefined | atom() when
      Template :: undefined | string() | binary(),
      Name :: atom().
compile_template(undefined, _) ->
    undefined;
compile_template(Template, Name) when not is_binary(Template) ->
    Path = case string:substr(Template, 1, 1) of
               "/" ->
                   Template;
               _ ->
                   BasePath = code:lib_dir(crossbar, priv),
                   lists:concat([BasePath, "/servers/", Template])
           end,
    ?LOG("sourcing template from file at ~s", [Path]),
    do_compile_template(Path, Name);
compile_template(Template, Name) ->
    do_compile_template(Template, Name).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Compiles template string or path, normalizing the return
%% @end
%%--------------------------------------------------------------------
-spec do_compile_template/2 :: (Template, Name) -> undefined | atom() when
      Template :: string() | binary(),
      Name :: atom().
do_compile_template(Template, Name) ->
    case erlydtl:compile(Template, Name) of
        {ok, Name} ->
            ?LOG("compiled ~s template", [Name]),
            Name;
        ok ->
            ?LOG("compiled ~s template file", [Name]),
            Name;
        _E ->
            ?LOG("could not compile ~s template, ignoring", [Name]),
            undefined
    end.
