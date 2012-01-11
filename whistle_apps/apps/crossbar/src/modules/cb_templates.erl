%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%%
%%% Handle client requests for template documents
%%%
%%% @end
%%% Created : 05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_templates).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).

-define(DB_PREFIX, "template/").

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
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.templates">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.templates">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};



handle_info({binding_fired, Pid, <<"v1_resource.validate.templates">>
                 ,[RD, #cb_context{req_nouns=[{<<"templates">>, _}]}=Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = validate(Params, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.templates">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  Context1 = load_template_db(Params, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.templates">>, [RD, Context | [TemplateName]=Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = create_template_db(TemplateName, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.templates">>, [RD, Context | [TemplateName]=Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  DbName = format_template_name(TemplateName, encoded),
                  case couch_mgr:db_delete(DbName) of
                      true -> 
                          Pid ! {binding_result, true, [RD, Context, Params]};
                      false -> 
                          Pid ! {binding_result, true, [RD, crossbar_util:response_db_fatal(Context), Params]}
                  end
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"account.created">>, #cb_context{doc=JObj, account_id=AccountId, db_name=AccountDb}=Context}, State) -> 
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  import_template(wh_json:get_value(<<"role">>, JObj), AccountId, AccountDb)
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
-spec bind_to_crossbar/0 :: () ->  no_return().
bind_to_crossbar() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.templates">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.templates">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.templates">>),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.#.templates">>),
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
-spec allowed_methods/1 :: ([ne_binary(),...] | []) -> {boolean(), http_methods()}.
allowed_methods([]) ->
    {true, ['GET']};
allowed_methods([_]) ->
    {true, ['PUT', 'DELETE']};
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
-spec resource_exists/1 :: ([ne_binary(),...] | []) -> {boolean(), []}.
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
-spec validate/2 :: ([ne_binary(),...] | [], #cb_context{}) -> #cb_context{}.
validate([], #cb_context{req_verb = <<"get">>}=Context) ->
    summary(Context);
validate([TemplateName], #cb_context{req_verb = <<"put">>}=Context) ->
    case load_template_db(TemplateName, Context) of
        #cb_context{resp_status=success} ->
            crossbar_util:response_conflicting_docs(Context);
        _Else ->
            crossbar_util:response(wh_json:new(), Context)
    end;
validate([TemplateName], #cb_context{req_verb = <<"delete">>}=Context) ->
    load_template_db(TemplateName, Context);
validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary/1 :: (#cb_context{}) -> #cb_context{}.
summary(Context) ->
    case couch_mgr:db_info() of
        {ok, Dbs} ->
            Context#cb_context{resp_status=success
                               ,resp_data = [format_template_name(Db, raw) || Db <- Dbs,
                                            (fun(<<?DB_PREFIX, _/binary>>) -> true;
                                                (_) -> false end)(Db)]
                              };
        _ ->
            crossbar_util:response_missing_view(Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will attempt to load the context with the db name of
%% for this account
%% @end
%%--------------------------------------------------------------------
-spec load_template_db/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_template_db([TemplateName], Context) ->
    load_template_db(TemplateName, Context);
load_template_db(TemplateName, Context) ->
    DbName = format_template_name(TemplateName, encoded),
    case couch_mgr:db_exists(DbName) of
        false ->
            ?LOG("check failed for template db ~s", [DbName]),
            crossbar_util:response_db_missing(Context);
        true ->
            ?LOG("check succeeded for template db ~s", [DbName]),
            Context#cb_context{resp_status=success
                               ,db_name = DbName
                               ,account_id = TemplateName
                              }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Format the template/db name into a raw, unencoded or encoded form.
%% @end
%%--------------------------------------------------------------------
-spec format_template_name/2 :: (ne_binary(), unencoded | encoded | raw) -> ne_binary().
format_template_name(<<"template/", _/binary>> = TemplateName, unencoded) ->
    TemplateName;
format_template_name(<<"template%2F", TemplateName/binary>>, unencoded) ->
    <<"template/", TemplateName/binary>>;
format_template_name(TemplateName, unencoded) ->
    <<"template/", TemplateName/binary>>;
format_template_name(<<"template%2F", _/binary>> = TemplateName, encoded) ->
    TemplateName;
format_template_name(<<"template/", TemplateName/binary>>, encoded) ->
    <<"template%2F", TemplateName/binary>>;
format_template_name(TemplateName, encoded) ->
    <<"template%2F", TemplateName/binary>>;
format_template_name(<<"template%2F", TemplateName/binary>>, raw) ->
    TemplateName;
format_template_name(<<"template/", TemplateName/binary>>, raw) ->
    TemplateName; 
format_template_name(TemplateName, raw) ->
    TemplateName.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new template database and load it with views so it can be
%% used as an 'account'
%% @end
%%--------------------------------------------------------------------
-spec create_template_db/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
create_template_db(TemplateName, Context) ->
    TemplateDb = format_template_name(TemplateName, encoded),
    case couch_mgr:db_create(TemplateDb) of
        false ->
            ?LOG_SYS("failed to create database: ~s", [TemplateDb]),
            crossbar_util:response_db_fatal(Context);
        true ->
            ?LOG_SYS("created DB for template ~s", [TemplateName]),
            couch_mgr:revise_docs_from_folder(TemplateDb, crossbar, "account", false),
            couch_mgr:revise_doc_from_file(TemplateDb, crossbar, ?MAINTENANCE_VIEW_FILE),
            Context#cb_context{resp_status=success}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If a valid template database is provided import the non-design
%% documents into the account
%% @end
%%--------------------------------------------------------------------
-spec import_template/3 :: (undefined | ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
import_template(undefined, _, _) ->
    ok;
import_template(TemplateName, AccountId, AccountDb) ->
    TemplateDb = format_template_name(TemplateName, encoded),
    case couch_mgr:all_docs(TemplateDb) of
        {ok, Docs} ->
            Ids = [Id || Doc <- Docs,
                         begin
                             Id = wh_json:get_value(<<"id">>, Doc),
                             (fun(<<"_design/", _/binary>>) -> false;
                                 (_) -> true end)(Id)
                         end],
            import_template_docs(Ids, TemplateDb, AccountId, AccountDb);
        _ ->
            ok
    end.            

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a list of IDs in the template database, import them into the
%% account database, correcting the pvt fields.
%% @end
%%--------------------------------------------------------------------
-spec import_template_docs/4 :: ([] | [ne_binary(),...], ne_binary(), ne_binary(), ne_binary()) -> ok.
import_template_docs([], _, _, _) ->
    ok;
import_template_docs([Id|Ids], TemplateDb, AccountId, AccountDb) ->
    case couch_mgr:open_doc(TemplateDb, Id) of
        {ok, JObj} ->
            Correctors = [fun(J) -> wh_json:set_value(<<"pvt_account_id">>, AccountId, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_account_db">>, AccountDb, J) end
                         ],
            couch_mgr:ensure_saved(AccountDb, lists:foldr(fun(F, J) -> F(J) end, JObj, Correctors)),
            import_template_docs(Ids, TemplateDb, AccountId, AccountDb);
        {error, _} ->
            import_template_docs(Ids, TemplateDb, AccountId, AccountDb)
    end.
