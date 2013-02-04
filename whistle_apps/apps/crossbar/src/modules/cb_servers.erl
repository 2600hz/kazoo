%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Servers module
%%%
%%% Handle client requests for server documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_servers).

-export([init/0
         ,authorize/1
         ,authenticate/1
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,content_types_provided/3
         ,put/1, put/3
         ,post/2, post/3
         ,delete/2
        ]).

-include("include/crossbar.hrl").

-define(SERVER_CONF, [code:lib_dir(crossbar, priv), "/servers/servers.conf"]).
-define(SERVER_CONFIG_CATEGORY, <<"crossbar.servers">>).

-define(CB_LIST, <<"servers/crossbar_listing">>).
-define(VIEW_DEPLOY_ROLES, <<"servers/list_deployment_roles">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    init_templates(),

    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.servers">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.servers">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, authenticate),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.servers">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.servers">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.servers">>, ?MODULE, post),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.servers">>, ?MODULE, delete).

authorize(#cb_context{req_nouns=[{<<"servers">>, [_,<<"deployment">>]}
                                 ,{?WH_ACCOUNTS_DB,[_]}
                                ]
                      ,req_verb = <<"post">>}) ->
    true.

authenticate(#cb_context{req_nouns=[{<<"servers">>, [_,<<"deployment">>]}
                                    ,{?WH_ACCOUNTS_DB,[_]}
                                   ]
                         ,req_verb = <<"post">>}) ->
    true.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    ['GET', 'PUT'].
allowed_methods(_) ->
    ['GET', 'POST', 'DELETE'].
allowed_methods(_, <<"deployment">>) ->
    ['GET', 'POST', 'PUT'];
allowed_methods(_, <<"log">>) ->
    ['GET'].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() ->
    true.
resource_exists(_) ->
    true.
resource_exists(_, <<"deployment">>) ->
    true;
resource_exists(_, <<"log">>) ->
    true.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update the content type so users can pull down the deploy log as
%% plain text
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(#cb_context{}, path_token(), path_token()) -> #cb_context{}.
content_types_provided(Context, _, <<"log">>) ->
    Context#cb_context{content_types_provided=[{to_binary, [{<<"text">>, <<"plain">>}]}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(#cb_context{}) -> #cb_context{}.
-spec validate(#cb_context{}, path_token()) -> #cb_context{}.
-spec validate(#cb_context{}, path_token(), path_token()) -> #cb_context{}.

validate(#cb_context{req_verb = <<"get">>}=Context) ->
    load_server_summary(Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create_server(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, ServerId) ->
    load_server(ServerId, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, ServerId) ->
    update_server(ServerId, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, ServerId) ->
    load_server(ServerId, Context).

validate(#cb_context{req_verb = <<"get">>}=Context, ServerId, <<"deployment">>) ->
    case load_server(ServerId, Context) of
        #cb_context{resp_status=success, doc=JObj}=Context1 ->
            RespData = wh_json:from_list([{<<"status">>, wh_json:get_value(<<"pvt_deploy_status">>, JObj)}
                                          ,{<<"log">>,wh_json:get_value(<<"pvt_deploy_log">>, JObj)}
                                         ]),

            Context1#cb_context{resp_data=RespData};
        Else -> Else
    end;
validate(#cb_context{req_verb = <<"post">>}=Context, ServerId, <<"deployment">>) ->
    case load_server(ServerId, Context) of
        #cb_context{resp_status=success, doc=JObj, req_data=Data}=Context1 ->
            DeployLog = [Data | wh_json:get_value(<<"pvt_deploy_log">>, JObj, [])],
            Context1#cb_context{doc=wh_json:set_value(<<"pvt_deploy_log">>, DeployLog, JObj)};
        Else -> Else
    end;
validate(#cb_context{req_verb = <<"put">>}=Context, ServerId, <<"deployment">>) ->
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
validate(#cb_context{req_verb = <<"get">>}=Context, ServerId, <<"log">>) ->
    crossbar_doc:load_attachment(ServerId, <<"deployment.log">>, Context).

-spec post(#cb_context{}, path_token()) -> #cb_context{}.
-spec post(#cb_context{}, path_token(), path_token()) -> #cb_context{}.
post(Context, _) ->
    _ = cb_context:put_reqid(Context),
    crossbar_doc:save(Context).
post(Context, _, <<"deployment">>) ->
    _ = cb_context:put_reqid(Context),
    case crossbar_doc:save(Context) of
        #cb_context{resp_status=success, doc=JObj}=Context1 ->
            RespData = wh_json:from_list([{<<"status">>, wh_json:get_value(<<"pvt_deploy_status">>, JObj)}
                                          ,{<<"log">>,wh_json:get_value(<<"pvt_deploy_log">>, JObj)}
                                         ]),

            Context1#cb_context{resp_data=RespData};
        Else -> Else
    end.

-spec put(#cb_context{}) -> #cb_context{}.
-spec put(#cb_context{}, path_token(), path_token()) -> #cb_context{}.
put(#cb_context{doc=Doc}=Context) ->
    _ = cb_context:put_reqid(Context),
    Id = wh_util:to_hex_binary(crypto:md5([wh_json:get_value(<<"ip">>, Doc), wh_json:get_value(<<"ssh_port">>, Doc)])),
    crossbar_doc:save(Context#cb_context{doc=wh_json:set_value(<<"_id">>, Id, Doc)}).
put(Context, _, <<"deployment">>) ->
    _ = cb_context:put_reqid(Context),
    execute_deploy_command(Context).

-spec delete(#cb_context{}, path_token()) -> #cb_context{}.
delete(Context, _) ->
    _ = cb_context:put_reqid(Context),
    case crossbar_doc:delete(Context, permanent) of
        #cb_context{resp_status=success}=Context1 ->
            execute_delete_command(Context),
            Context1;
        Else ->
            Else
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init_templates() -> 'ok'.
init_templates() ->
    case get_configs() of
        {ok, Terms} ->
            lager:debug("loaded config from ~s", [?SERVER_CONF]),
            whapps_config:get(?SERVER_CONFIG_CATEGORY, <<"data_bag_tmpl">>, compile_template(props:get_value(data_bag_tmpl, Terms), cb_servers_data_bag)),
            whapps_config:get(?SERVER_CONFIG_CATEGORY, <<"databag_mapping">>, props:get_value(databag_mapping, Terms, [])),
            whapps_config:get(?SERVER_CONFIG_CATEGORY, <<"databag_path_tmpl">>, compile_template(props:get_value(databag_path_tmpl, Terms), cb_servers_databag_path_tmpl)),
            whapps_config:get(?SERVER_CONFIG_CATEGORY, <<"role_tmpl">>, compile_template(props:get_value(role_tmpl, Terms), cb_servers_role_tmpl)),
            whapps_config:get(?SERVER_CONFIG_CATEGORY, <<"role_path_tmpl">>, compile_template(props:get_value(role_path_tmpl, Terms), cb_servers_role_path_tmpl)),
            whapps_config:get(?SERVER_CONFIG_CATEGORY, <<"prod_deploy_tmpl">>, compile_template(props:get_value(prod_deploy_tmpl, Terms), cb_servers_prod_deploy_tmpl)),
            whapps_config:get(?SERVER_CONFIG_CATEGORY, <<"dev_deploy_tmpl">>, compile_template(props:get_value(dev_deploy_tmpl, Terms), cb_servers_dev_deploy_tmpl)),
            whapps_config:get(?SERVER_CONFIG_CATEGORY, <<"dev_role">>, wh_util:to_binary(props:get_value(dev_role, Terms, <<"all_in_one">>))),
            whapps_config:get(?SERVER_CONFIG_CATEGORY, <<"delete_tmpl">>, compile_template(props:get_value(delete_tmpl, Terms), cb_server_delete_tmpl)),
            ok;
        {error, _} ->
            lager:debug("could not read config from ~s", [?SERVER_CONF]),
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_configs() -> {'ok', proplist()} | {'error', file:posix() | 'badarg' | 'terminated' | 'system_limit'
                                                   | {integer(), module(), term()}}.
get_configs() ->
    file:consult(lists:flatten(?SERVER_CONF)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_server_summary(#cb_context{}) -> #cb_context{}.
load_server_summary(Context) ->
        crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new server document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_server(#cb_context{}) -> #cb_context{}.
create_server(#cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"servers">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
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
-spec load_server(ne_binary(), #cb_context{}) -> #cb_context{}.
load_server(ServerId, Context) ->
    crossbar_doc:load(ServerId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing server document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_server(ne_binary(), #cb_context{}) -> #cb_context{}.
update_server(ServerId, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"servers">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            crossbar_doc:load_merge(ServerId, JObj, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Optional command template to execute on the deletion of a server
%% @end
%%--------------------------------------------------------------------
-spec execute_delete_command(#cb_context{}) -> 'ok'.
execute_delete_command(#cb_context{doc=JObj}) ->
    case whapps_config:get_atom(?SERVER_CONFIG_CATEGORY, <<"delete_tmpl">>) of
        undefined ->
            lager:debug("no delete template defined");
        DeleteTmpl ->
            Props = wh_json:to_proplist(JObj),
            {ok, C} = DeleteTmpl:render([{<<"server">>, Props}]),
            Cmd = binary_to_list(iolist_to_binary(C)),
            lager:debug("executing delete template: ~s", [Cmd]),
            Res = os:cmd(Cmd),
            lager:debug("deleting template resulted in ~s", [Cmd, Res])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Execute the deploy command with the provided arguments, ensuring the
%% doc is updated afterward.  Because the doc is a "semaphore" for the
%% deployment
%% @end
%%--------------------------------------------------------------------
-spec execute_deploy_command(#cb_context{}) -> #cb_context{}.
execute_deploy_command(Context) ->
    case whapps_config:get(?SERVER_CONFIG_CATEGORY, <<"dev_deploy_tmpl">>) of
        undefined ->
            lager:debug("no development deploy template defined"),
            crossbar_util:response(error, <<"failed to start deployment">>, Context);
        _ ->
            case whapps_config:get(?SERVER_CONFIG_CATEGORY, <<"prod_deploy_tmpl">>) of
                undefined ->
                    lager:debug("no production deploy template defined"),
                    crossbar_util:response(error, <<"failed to start deployment">>, Context);
                _ ->
                    exec_deploy(Context)
            end
    end.

exec_deploy(#cb_context{db_name=Db, doc=JObj}=Context) ->
    ServerId = wh_json:get_value(<<"_id">>, JObj),
    Props = template_props(Context),
    CmdTmpl = get_command_tmpl(Context),
    {ok, C} = CmdTmpl:render(Props),
    Cmd = binary_to_list(iolist_to_binary(C)),
    case mark_deploy_running(Db, ServerId) of
        {ok, _} ->
            spawn(fun() ->
                          lager:debug("executing deploy command ~s", [Cmd]),
                          Res = os:cmd(Cmd),
                          lager:debug("deploy command execution completed", []),
                          {ok, _} = mark_deploy_complete(Db, ServerId),
                          lager:debug("attempting to upload log ~s", [Res]),
                          {ok, Log} = file:read_file(Res),
                          couch_mgr:put_attachment(Db, ServerId, <<"deployment.log">>, Log)
                  end),
            crossbar_util:response([], Context);
        {error, _} ->
            crossbar_util:response(error, <<"could not lock deployment">>, Context)
    end.

get_command_tmpl(#cb_context{doc=JObj}) ->
    DevRole = whapps_config:get(?SERVER_CONFIG_CATEGORY, <<"dev_role">>),
    Roles = wh_json:get_value(<<"roles">>, JObj, []),
    case lists:member(DevRole, Roles) of
        true ->
            lager:debug("use development template"),
            whapps_config:get_atom(?SERVER_CONFIG_CATEGORY, <<"dev_deploy_tmpl">>);
        false ->
            lager:debug("use production template"),
            whapps_config:get_atom(?SERVER_CONFIG_CATEGORY, <<"prod_deploy_tmpl">>)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create a proplist to provide to the templates during render
%% @end
%%--------------------------------------------------------------------
-spec template_props(#cb_context{}) -> [{ne_binary(), ne_binary() | proplist() | wh_json:json_objects()},...].
template_props(#cb_context{doc=JObj, req_data=Data, db_name=Db}=Context) ->
    Mappings = whapps_config:get(?SERVER_CONFIG_CATEGORY, <<"databag_mapping">>),
    RolePathTmpl = whapps_config:get_atom(?SERVER_CONFIG_CATEGORY, <<"role_path_tmpl">>),
    DatabagPathTmpl = whapps_config:get_atom(?SERVER_CONFIG_CATEGORY, <<"databag_path_tmpl">>),

    Server = wh_json:to_proplist(JObj),
    Servers = case couch_mgr:get_results(Db, ?CB_LIST, [{<<"include_docs">>, true}]) of
                  {ok, Srvs} ->
                      [wh_json:to_proplist(wh_json:get_value(<<"doc">>, Srv))
                       || Srv <- Srvs];
                  {error, _} -> []
              end,
    Account = case couch_mgr:open_doc(Db, wh_util:format_account_id(Db, raw)) of
                  {ok, A} -> wh_json:to_proplist(A);
                  {error, _} -> []
              end,
    {Role, RolePath} = case couch_mgr:get_results(Db, ?VIEW_DEPLOY_ROLES, [{<<"include_docs">>, true}]) of
                           {ok, []} ->
                               R = create_role(Account, Context),
                               {wh_json:to_proplist(R), write_role(Account, Server, R, RolePathTmpl)};
                           {ok, [R|_]} ->
                               R2 = wh_json:get_value(<<"doc">>, R),
                               {wh_json:to_proplist(R2), write_role(Account, Server, R2, RolePathTmpl)};
                           {error, _} ->
                               R = create_role(Account, Context),
                               {wh_json:to_proplist(R), write_role(Account, Server, R, RolePathTmpl)}
                       end,
    DatabagBase = wh_json:set_value(<<"id">>, props:get_value(<<"_id">>, Account), wh_json:new()),
    Databag = create_databag(Servers, Mappings, DatabagBase),
    DatabagPath = write_databag(Account, Server, Databag, DatabagPathTmpl),
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
-spec create_role(proplist(), #cb_context{}) -> wh_json:json_object().
create_role(Account, #cb_context{db_name=Db}) ->
    case whapps_config:get_atom(?SERVER_CONFIG_CATEGORY, <<"role_tmpl">>) of
        undefined -> wh_json:new();
        RoleTmpl ->
            try
                Props = [{<<"account">>, Account}
                         ,{<<"host">>, wh_util:to_binary(net_adm:localhost())}
                         %% The list index syntax of erlydtl doesnt seem to compile
                         ,{<<"rand_small_1">>, wh_util:to_hex_binary(crypto:rand_bytes(8))}
                         ,{<<"rand_small_2">>, wh_util:to_hex_binary(crypto:rand_bytes(8))}
                         ,{<<"rand_small_3">>, wh_util:to_hex_binary(crypto:rand_bytes(8))}
                         ,{<<"rand_small_4">>, wh_util:to_hex_binary(crypto:rand_bytes(8))}
                         ,{<<"rand_small_5">>, wh_util:to_hex_binary(crypto:rand_bytes(8))}
                         ,{<<"rand_large_1">>, wh_util:to_hex_binary(crypto:rand_bytes(24))}
                         ,{<<"rand_large_2">>, wh_util:to_hex_binary(crypto:rand_bytes(24))}
                         ,{<<"rand_large_3">>, wh_util:to_hex_binary(crypto:rand_bytes(24))}
                         ,{<<"rand_large_4">>, wh_util:to_hex_binary(crypto:rand_bytes(24))}
                         ,{<<"rand_large_5">>, wh_util:to_hex_binary(crypto:rand_bytes(24))}
                        ],
                {ok, Role} = RoleTmpl:render(Props),
                JObj = wh_json:decode(iolist_to_binary(Role)),
                {ok, R} = couch_mgr:save_doc(Db, wh_json:set_value(<<"pvt_type">>, <<"deployment_role">>, JObj)),
                R
            catch
                _:_ -> wh_json:new()
            end
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
-spec write_databag(proplist(), proplist(), wh_json:json_object(), atom()) -> ne_binary().
write_databag(_, _, _, undefined) -> <<>>;
write_databag(Account, Server, JObj, PathTmpl) ->
    JSON = wh_json:encode(wh_json:public_fields(JObj)),
    Props = [{<<"account">>, Account}
             ,{<<"server">>, Server}],
    {ok, P} = PathTmpl:render(Props),
    Path = iolist_to_binary(P),
    lager:debug("writing databag to ~s", [Path]),
    ok = file:write_file(Path, JSON),
    Path.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates a databag for this deployment
%% @end
%%--------------------------------------------------------------------
-spec create_databag(wh_json:json_objects(), list(), wh_json:json_object()) -> wh_json:json_object().
create_databag([], _, JObj) -> JObj;
create_databag([H|T], Mapping, JObj) ->
    Roles = props:get_value(<<"roles">>, H, []),
    Hostname = props:get_value(<<"hostname">>, H),
    IP = props:get_value(<<"ip">>, H),

    NewJ = lists:foldr(fun(Role, J) ->
                               case wh_json:get_value(Role, Mapping) of
                                   undefined -> J;
                                   Name -> wh_json:set_value([Name, <<"servers">>, Hostname], IP, J)
                               end
                       end, JObj, Roles),
    create_databag(T, Mapping, NewJ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Writes the existing role json object to a file as defined by
%% role_path_tmpl, then returns the path
%% @end
%%--------------------------------------------------------------------
-spec write_role(proplist(), proplist(), wh_json:json_object(), atom()) -> binary().
write_role(_, _, _, undefined) -> <<>>;
write_role(Account, Server, JObj, PathTmpl) ->
    JSON = wh_json:encode(wh_json:public_fields(JObj)),
    Props = [{<<"account">>, Account}
             ,{<<"server">>, Server}],
    {ok, P} = PathTmpl:render(Props),
    Path = binary_to_list(iolist_to_binary(P)),
    lager:debug("writing role to ~s", [Path]),
    ok = file:write_file(Path, JSON),
    Path.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reset the deploy log and set the status to "running", if this
%% conflicts it will require another request
%% @end
%%--------------------------------------------------------------------
-spec mark_deploy_running(ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} | {'error', atom()}.
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
-spec mark_deploy_complete(ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} | {'error', atom()}.
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
-spec compile_template(nonempty_string() | ne_binary() | 'undefined', Name) -> Name | 'undefined'.
compile_template(undefined, _) -> 'undefined';
compile_template(Template, Name) when not is_binary(Template) ->
    Path = case string:substr(Template, 1, 1) of
               "/" -> wh_util:to_binary(Template);
               _ ->
                   BasePath = code:lib_dir(crossbar, priv),
                   lists:concat([BasePath, "/servers/", Template])
           end,
    lager:debug("sourcing template from file at ~s", [Path]),
    do_compile_template(Path, Name);
compile_template(Template, Name) ->
    do_compile_template(Template, Name).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Compiles template string or path, normalizing the return
%% @end
%%--------------------------------------------------------------------
-spec do_compile_template(ne_binary() | nonempty_string(), Name) -> 'undefined' | Name.
do_compile_template(Template, Name) ->
    case erlydtl:compile(Template, Name) of
        ok ->
            lager:debug("compiled ~s template file", [Name]),
            Name;
        {ok, _} ->
            lager:debug("compiled ~s template file", [Name]),
            Name;
        _E ->
            lager:debug("could not compile ~s template, ignoring (~p)", [Name, _E]),
            undefined
    end.
