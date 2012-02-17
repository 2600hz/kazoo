%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Devices module
%%%
%%% Handle client requests for device documents
%%%
%%% @end
%%% Created : 05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_devices).

-behaviour(gen_server).

%% API
-export([start_link/0, lookup_regs/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).

-define(CB_LIST, <<"devices/crossbar_listing">>).

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
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.devices">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.devices">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.devices">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = validate(Params, RD, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
         end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.devices">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  case crossbar_doc:save(Context) of
                      #cb_context{resp_status=success, doc=Doc1}=Context1 ->
                          DeviceId = wh_json:get_value(<<"_id">>, Doc1),
                          IsRealmDefined = wh_util:is_empty(wh_json:get_value([<<"sip">>, <<"realm">>], Doc1)),
                          case couch_mgr:lookup_doc_rev(?WH_SIP_DB, DeviceId) of
                              {ok, Rev} when IsRealmDefined ->
                                  ?LOG("removing device from sip auth aggregate as it is using the account realm"),
                                  couch_mgr:del_doc(?WH_SIP_DB, wh_json:set_value(<<"_rev">>, Rev, Doc1));
                              {ok, Rev} ->
                                  ?LOG("updating device in sip auth aggregate"),
                                  couch_mgr:ensure_saved(?WH_SIP_DB, wh_json:set_value(<<"_rev">>, Rev, Doc1));
                              {error, not_found} ->
                                  ?LOG("adding device to the sip auth aggregate"),
                                  couch_mgr:ensure_saved(?WH_SIP_DB, wh_json:delete_key(<<"_rev">>, Doc1))
                          end,
                          provision(Context1, whapps_config:get_string(<<"crossbar.devices">>, <<"provisioning_type">>)),
                          Pid ! {binding_result, true, [RD, Context1, Params]};
                      Else ->
                          Pid ! {binding_result, true, [RD, Else, Params]}
                  end
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.devices">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  case crossbar_doc:save(Context) of
                      #cb_context{resp_status=success, doc=Doc1}=Context1 ->
                          case wh_json:get_ne_value([<<"sip">>, <<"realm">>], Doc1) of
                              undefined -> ok;
                              _Else ->
                                  ?LOG("adding device to the sip auth aggregate"),
                                  couch_mgr:ensure_saved(?WH_SIP_DB, wh_json:delete_key(<<"_rev">>, Doc1))
                          end,    
                          provision(Context1, whapps_config:get_string(<<"crossbar.devices">>, <<"provisioning_type">>)),
                          Pid ! {binding_result, true, [RD, Context1, Params]};
                      Else ->
                          Pid ! {binding_result, true, [RD, Else, Params]}
                  end
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.devices">>, [RD, #cb_context{doc=Doc}=Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  case crossbar_doc:delete(Context) of
                      #cb_context{resp_status=success}=Context1 ->
                          DeviceId = wh_json:get_value(<<"_id">>, Doc),
                          case couch_mgr:lookup_doc_rev(?WH_SIP_DB, DeviceId) of
                              {ok, Rev} ->
                                  couch_mgr:del_doc(?WH_SIP_DB, wh_json:set_value(<<"_rev">>, Rev, Doc));
                              {error, not_found} ->
                                  ok
                          end,
                          Pid ! {binding_result, true, [RD, Context1, Params]};
                      Else ->
                          Pid ! {binding_result, true, [RD, Else, Params]}
                  end
          end),
    {noreply, State};

handle_info({binding_fired, Pid, _, Payload}, State) ->
    Pid ! {binding_result, false, Payload},
    {noreply, State};

handle_info(timeout, State) ->
    _ = bind_to_crossbar(),
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
-spec bind_to_crossbar/0 :: () -> 'ok' | {'error', 'exists'}.
bind_to_crossbar() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.devices">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.devices">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.devices">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.devices">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/1 :: (path_tokens()) -> {boolean(), http_methods()}.
allowed_methods([]) ->
    {true, ['GET', 'PUT']};
allowed_methods([<<"status">>]) ->
    {true, ['GET']};
allowed_methods([_]) ->
    {true, ['GET', 'POST', 'DELETE']};
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
-spec resource_exists/1 :: (path_tokens()) -> {boolean(), []}.
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
-spec validate/3 :: (path_tokens(), #wm_reqdata{}, #cb_context{}) -> #cb_context{}.
validate([], _, #cb_context{req_verb = <<"get">>}=Context) ->
    load_device_summary(Context);
validate([], _, #cb_context{req_verb = <<"put">>}=Context) ->
    create_device(Context);
validate([<<"status">>], _, #cb_context{req_verb = <<"get">>}=Context) ->
    load_device_status(Context);
validate([DocId], _, #cb_context{req_verb = <<"get">>}=Context) ->
    load_device(DocId, Context);
validate([DocId], _, #cb_context{req_verb = <<"post">>}=Context) ->
    update_device(DocId, Context);
validate([DocId], _, #cb_context{req_verb = <<"delete">>}=Context) ->
    load_device(DocId, Context);
validate(_, _, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_device_summary/1 :: (#cb_context{}) -> #cb_context{}.
load_device_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new device document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_device/1 :: (#cb_context{}) -> #cb_context{}.
create_device(#cb_context{req_data=Req, db_name=Db}=Context) ->
    SIPRealm = wh_json:get_ne_value([<<"sip">>, <<"realm">>], Req, <<>>),
    AccountRealm = crossbar_util:get_account_realm(Context),
    Data = case AccountRealm =:= SIPRealm of
               true ->
                   wh_json:delete_key([<<"sip">>, <<"realm">>], Req);
               false ->
                   Req
           end,
    Username = wh_json:get_ne_value([<<"sip">>, <<"username">>], Data),
    Realm = wh_json:get_ne_value([<<"sip">>, <<"realm">>], Data),
    IsCredsUnique = is_sip_creds_unique(Db, Realm, Username),
    case wh_json_validator:is_valid(Data, <<"devices">>) of
        {fail, Errors} when not IsCredsUnique ->
            E = wh_json:set_value([<<"sip">>, <<"username">>, <<"unique">>]
                                  ,<<"SIP credentials are already in use">>
                                  ,Errors),
            crossbar_util:response_invalid_data(E, Context);
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, _} when not IsCredsUnique ->
            E = wh_json:set_value([<<"sip">>, <<"username">>, <<"unique">>]
                                  ,<<"SIP credentials are already in use">>
                                  ,wh_json:new()),
            crossbar_util:response_invalid_data(E, Context);
        {pass, JObj} ->
            Context#cb_context{resp_status=success
                               ,doc=wh_json:set_value(<<"pvt_type">>, <<"device">>, JObj)
                              }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a device document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_device/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_device(DocId, Context) ->
    crossbar_doc:load(DocId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing device document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_device/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update_device(DocId, #cb_context{req_data=Req, db_name=Db}=Context) ->
    SIPRealm = wh_json:get_ne_value([<<"sip">>, <<"realm">>], Req, <<>>),
    AccountRealm = crossbar_util:get_account_realm(Context),
    Data = case AccountRealm =:= SIPRealm of
               true ->
                   wh_json:delete_key([<<"sip">>, <<"realm">>], Req);
               false ->
                   Req
           end,
    Username = wh_json:get_ne_value([<<"sip">>, <<"username">>], Data),
    Realm = wh_json:get_ne_value([<<"sip">>, <<"realm">>], Data),
    IsCredsUnique = is_sip_creds_unique(Db, Realm, Username, DocId),
    case wh_json_validator:is_valid(Data, <<"devices">>) of
        {fail, Errors} when not IsCredsUnique ->
            E = wh_json:set_value([<<"sip">>, <<"username">>, <<"unique">>]
                                  ,<<"SIP credentials are already in use">>
                                  ,Errors),
            crossbar_util:response_invalid_data(E, Context);
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, _} when not IsCredsUnique ->
            E = wh_json:set_value([<<"sip">>, <<"username">>, <<"unique">>]
                                  ,<<"SIP credentials are already in use">>
                                  ,wh_json:new()),
            crossbar_util:response_invalid_data(E, Context);
        {pass, JObj} ->
            crossbar_doc:load_merge(DocId, JObj, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieve the status of the devices linked to the account
%% Reads registered devices in registrations, then map to devices of the account
%% @end
%%--------------------------------------------------------------------
-spec load_device_status/1 :: (#cb_context{}) -> #cb_context{}.
load_device_status(#cb_context{db_name=Db}=Context) ->
    {ok, JObjs} = couch_mgr:get_results(Db, ?CB_LIST, [{<<"include_docs">>, true}]),
    AccountRealm = crossbar_util:get_account_realm(Context),
    AccountDevices = lists:foldl(fun(JObj, Acc) -> 
                                         [{wh_json:get_ne_value([<<"doc">>, <<"sip">>, <<"realm">>], JObj, AccountRealm),
                                           wh_json:get_value([<<"doc">>, <<"sip">>, <<"username">>], JObj)} 
                                          | Acc
                                         ] 
                                 end, [], JObjs),
    Result = lists:foldl(fun(AuthorizingId, Acc) ->
                                 Props = [{<<"device_id">>, AuthorizingId}
                                          ,{<<"registered">>, true}
                                         ],
                                 [wh_json:from_list(Props)| Acc]
                         end, [], lookup_regs(AccountDevices)),
    crossbar_util:response(Result, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% For a given [{Realm, Username}|...], returns  [[Realm, Username]|...]
%% whose device is registered, ready to be used in a view filter
%% @end
%%--------------------------------------------------------------------
-spec lookup_regs/1 :: ([{ne_binary(), ne_binary()},...] | []) -> [[ne_binary(),...],...] | [].
lookup_regs([]) -> [];
lookup_regs(RealmUserList) ->
    Q = amqp_util:new_queue(),
    ok = amqp_util:bind_q_to_targeted(Q),
    ok = amqp_util:basic_consume(Q),
    _ = [spawn(fun() -> lookup_registration(Realm, User, Q) end) || {Realm, User} <- RealmUserList],
    wait_for_reg_resp(RealmUserList, []). %% number of devices we're supposed to get an answer from

-spec lookup_registration/3 :: (ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
lookup_registration(Realm, User, Q) ->
    ?LOG_SYS("looking up registration information for ~s@~s", [User, Realm]),
    Req = [{<<"Username">>, User}
           ,{<<"Realm">>, Realm}
           | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
          ],
    wapi_registration:publish_query_req(Req).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Collect Len number of registrations in Acc unless the timeout
%% occurs
%% @end
%%--------------------------------------------------------------------
-spec wait_for_reg_resp/2 :: (list(), list()) -> [[ne_binary(),...],...] | [].
wait_for_reg_resp([], Acc) -> Acc;
wait_for_reg_resp([_|T], Acc) ->
    try
        receive
            {amqp_host_down, _} ->
                ?LOG("lost AMQP connection"),
                Acc;
            {amqp_lost_channel,no_connection} ->
                ?LOG("lost AMQP connection"),
                Acc;
            {_, #amqp_msg{payload = Payload}} ->
                Resp = wh_json:decode(Payload),
                true = wapi_registration:query_resp_v(Resp),
                AuthorizingId = wh_json:get_value([<<"Fields">>, <<"Authorizing-ID">>], Resp),
                case lists:member(AuthorizingId, Acc) of
                    true ->
                        wait_for_reg_resp([ok|T], Acc);
                    false ->
                        wait_for_reg_resp(T, [AuthorizingId | Acc])
                end;
            #'basic.consume_ok'{} ->
                wait_for_reg_resp([ok|T], Acc)
        after
            1000 ->
                ?LOG("timeout for registration query"),
                Acc
        end
    catch
        _:_ ->
            wait_for_reg_resp([ok|T], Acc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if the device sip creds are unique
%% @end
%%--------------------------------------------------------------------
-spec is_sip_creds_unique/3 :: (undefined | ne_binary(), undefined | ne_binary(), undefined | ne_binary() ) -> boolean().
-spec is_sip_creds_unique/4 :: (undefined | ne_binary(), undefined | ne_binary(), undefined | ne_binary(), undefined | ne_binary()) 
                               -> boolean().

is_sip_creds_unique(AccountDb, Realm, Username) ->
    is_sip_creds_unique(AccountDb, Realm, Username, undefined).

%% no account id and no doc id (ie initial create with no account)
is_sip_creds_unique(undefined, _, _, undefined) ->
    true;
is_sip_creds_unique(_, _, undefined, undefined) ->
    true;
is_sip_creds_unique(AccountDb, undefined, Username, DocId) ->
    case couch_mgr:get_results(AccountDb, <<"devices/sip_credentials">>, [{<<"key">>, Username}]) of
        {ok, []} -> true;
        {ok, [JObj]} -> 
            wh_json:get_value(<<"id">>, JObj) =:= DocId;
        {error, not_found} -> true;
        _ -> false
    end;
is_sip_creds_unique(_, Realm, Username, DocId) ->
    ViewOptions = [{<<"key">>, [Realm, Username]}],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
        {ok, []} -> true;
        {ok, [JObj]} -> wh_json:get_value(<<"id">>, JObj) =:= DocId;
        {error, not_found} -> true;
        _ -> false
    end.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Spawn the function to post data to a provisioning server, if
%% provided with URL
%% @end
%%--------------------------------------------------------------------
-spec provision/2 :: (#cb_context{}, ne_binary() | 'undefined') -> 'ok' | pid().
provision(Context, "simple_provisioner") ->   
    spawn(fun() -> do_simple_provision(Context) end);
provision(Context, "provisioner.net") ->
    spawn(fun() -> do_awesome_provision(Context) end);
provision(_, _Else) ->
    ?LOG("unsupported provisioning type: ~s", [_Else]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do awesome provisioning
%% @end
%%--------------------------------------------------------------------
-spec do_awesome_provision/1 :: (#cb_context{}) -> 'ok'.
do_awesome_provision(#cb_context{doc=JObj, db_name=Db}) ->
    TemplateOverrides = wh_json:get_value([<<"provision">>, <<"template">>], JObj, wh_json:new()),
    TemplateId = wh_json:get_value([<<"provision">>, <<"id">>], JObj),
    case is_binary(TemplateId) andalso couch_mgr:open_doc(Db, TemplateId) of
        false ->
             ?LOG("unknown template id ~s", [TemplateId]),
             ok;
        {error, _R} ->
             ?LOG("could not fetch template doc ~s: ~p", [TemplateId, _R]),
             ok;
        {ok, TemplateJObj} ->
            TemplateBase = wh_json:get_value(<<"template">>, TemplateJObj),
            Template = wh_json:merge_recursive(TemplateBase, TemplateOverrides),
            ProvisionRequest = provision_device_line([<<"data">>, <<"globals">>, <<"globals">>, <<"lineloop|line_1">>]
                                                    ,JObj, Template),
            MACAddress = re:replace(wh_json:get_string_value(<<"mac_address">>, JObj, "")
                                    ,"[^0-9a-fA-F]", "", [{return, list}, global]),
            send_awesome_provisioning_request(ProvisionRequest, MACAddress)
    end,
    ok.

-spec provision_device_line/3 :: ([ne_binary(),...], wh_json:json_object(), wh_json:json_object()) -> wh_json:json_object().
provision_device_line(BaseKey, Device, Template) ->
    Mappings = [{[<<"username">>, <<"value">>], [<<"sip">>, <<"username">>]}
                ,{[<<"authname">>, <<"value">>], [<<"sip">>, <<"username">>]}
                ,{[<<"secret">>, <<"value">>], [<<"sip">>, <<"password">>]}
                ,{[<<"server_host">>, <<"value">>], [<<"sip">>, <<"realm">>]}
               ],
    provision_device_line(BaseKey, Device, Template, Mappings).

-spec provision_device_line/4 :: (wh_json:json_strings(), wh_json:json_object(), wh_json:json_object(), [{wh_json:json_strings(), wh_json:json_strings()},...] | []) 
                                 -> wh_json:json_object().
provision_device_line(_, _, Template, []) ->
    Template;
provision_device_line(BaseKey, Device, Template, [{TemplateKey, DeviceKey}|T]) ->
    case wh_json:get_ne_value(DeviceKey, Device) of
        undefined -> provision_device_line(BaseKey, Device, Template, T);
        Value when is_list(TemplateKey) ->
            NewTemplate = wh_json:set_value(BaseKey ++ TemplateKey, Value, Template),
            provision_device_line(BaseKey, Device, NewTemplate, T);
        Value ->
            NewTemplate = wh_json:set_value(BaseKey ++ [TemplateKey], Value, Template),
            provision_device_line(BaseKey, Device, NewTemplate, T)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send awesome provisioning request
%% @end
%%--------------------------------------------------------------------
-spec send_awesome_provisioning_request/2 :: (wh_json:json_object(), ne_binary()) -> 'ok'.
send_awesome_provisioning_request(ProvisionRequest, MACAddress) ->
    Url = whapps_config:get_string(<<"crossbar.devices">>, <<"provisioning_url">>),
    UrlString = lists:flatten([Url, MACAddress]),
    Headers = [{"User-Agent", wh_util:to_list(erlang:node())}
               ,{"Content-Type", "application/json"}
              ],
    Body = wh_json:encode(ProvisionRequest),
    HTTPOptions = [],
    ?LOG("provisioning via ~s with settings ~s", [UrlString, Body]),
    case ibrowse:send_req(UrlString, Headers, post, Body, HTTPOptions) of
        {ok, "200", _, Response} ->
            ?LOG("SUCCESS! BOOM! ~s", [Response]);
        {ok, Code, _, Response} ->
            ?LOG("ERROR! OH NO! ~s. ~s", [Code, Response])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% post data to a provisiong server
%% @end
%%--------------------------------------------------------------------
-spec do_simple_provision/1 :: (#cb_context{}) -> 'ok'.
do_simple_provision(#cb_context{doc=JObj}=Context) ->
    Url = whapps_config:get_string(<<"crossbar.devices">>, <<"provisioning_url">>),
    AccountRealm = crossbar_util:get_account_realm(Context),
    Headers = [{K, V}
               || {K, V} <- [{"Host", whapps_config:get_string(<<"crossbar.devices">>, <<"provisioning_host">>)}
                             ,{"Referer", whapps_config:get_string(<<"crossbar.devices">>, <<"provisioning_referer">>)}
                             ,{"User-Agent", wh_util:to_list(erlang:node())}
                             ,{"Content-Type", "application/x-www-form-urlencoded"}]
                      ,V =/= undefined],
    HTTPOptions = [],
    Body = [{"api[realm]", wh_json:get_string_value([<<"sip">>, <<"realm">>], JObj, AccountRealm)}
            ,{"mac", re:replace(wh_json:get_string_value(<<"mac_address">>, JObj, ""), "[^0-9a-fA-F]", "", [{return, list}, global])}
            ,{"label", wh_json:get_string_value(<<"name">>, JObj)}
            ,{"sip[username]", wh_json:get_string_value([<<"sip">>, <<"username">>], JObj)}
            ,{"sip[password]", wh_json:get_string_value([<<"sip">>, <<"password">>], JObj)}
            ,{"submit", "true"}],
    Encoded = mochiweb_util:urlencode(Body),
    ?LOG("posting to ~s with ~s", [Url, Encoded]),
    ibrowse:send_req(Url, Headers, post, Encoded, HTTPOptions).
