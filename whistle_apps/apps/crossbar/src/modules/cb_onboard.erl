%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%%
%%% Handle client requests for onboard documents
%%%
%%% @end
%%% Created : 05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_onboard).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).
-define(OB_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".onboard">>).
-define(DEFAULT_FLOW, "{\"data\": { \"id\": \"~s\" }, \"module\": \"user\", \"children\": { \"_\": { \"data\": { \"id\": \"~s\" }, \"module\": \"voicemail\", \"children\": {}}}}").

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
                 ,{RD, #cb_context{req_nouns=[{<<"onboard">>,[]}]
                                   ,req_verb = <<"put">>
                                   ,req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authorizing request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authenticate">>
                 ,{RD, #cb_context{req_nouns=[{<<"onboard">>,[]}]
                                   ,req_verb = <<"put">>
                                   ,req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authenticating request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.onboard">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.onboard">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.onboard">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = validate(Params, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.onboard">>, [RD, #cb_context{doc=Data}=Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = populate_new_account(Data, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
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
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.onboard">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.onboard">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.onboard">>),
    crossbar_bindings:bind(<<"v1_resource.execute.put.onboard">>).

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
-spec resource_exists/1 :: ([ne_binary(),...] | []) -> {boolean(), []}.
resource_exists([]) ->
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
validate([], #cb_context{req_data=JObj, req_verb = <<"put">>}=Context) ->
    Generators = [fun(R) -> create_extensions(JObj, Context, R) end
                  ,fun(R) -> create_account(JObj, Context, R) end
                 ],
    case lists:foldr(fun(F, Acc) -> F(Acc) end, {[], wh_json:new()}, Generators) of
        {P, ?EMPTY_JSON_OBJECT} ->
            Context#cb_context{
              doc=lists:flatten(P)
              ,resp_status=success
             };
        {_, Failures} ->
            crossbar_util:response_invalid_data(Failures, Context)
    end;
validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will loop over all the 'extensions' and collect
%% valid context's for users, voicemailboxes, devices, and callflows.
%% Any errors will also be collected.
%% @end
%%--------------------------------------------------------------------
-spec create_extensions/3 :: (json_object(), #cb_context{}, {proplist(), json_object()}) -> {proplist(), json_object()}. 
create_extensions(JObj, Context, Results) ->
    Extensions = wh_json:get_value(<<"extensions">>, JObj, []),
    create_extensions(Extensions, 1, Context, Results).
    
create_extensions([], _, _, Results) ->
    Results;
create_extensions([Exten|Extens], Iteration, Context, {PassAcc, FailAcc}) ->
    Generators = [fun(R) -> create_exten_callflow(Exten, Iteration, Context, R) end
                  ,fun(R) -> create_vmbox(Exten, Iteration, Context, R) end
                  ,fun(R) -> create_device(Exten, Iteration, Context, R) end
                  ,fun(R) -> create_user(Exten, Iteration, Context, R) end
                 ],
    case lists:foldr(fun(F, Acc) -> F(Acc) end, {[], wh_json:new()}, Generators) of
        {P, ?EMPTY_JSON_OBJECT} ->
            create_extensions(Extens, Iteration + 1, Context, {[P|PassAcc], FailAcc});
        {P, F} ->
            Failures = wh_json:set_value([<<"extensions">>, Iteration], F, FailAcc),
            create_extensions(Extens, Iteration + 1, Context, {[P|PassAcc], Failures})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use the bindings to validate and create a context
%% record to generate an account.  Any failure will be added to the error 
%% json object.
%% @end
%%--------------------------------------------------------------------
-spec create_account/3 :: (json_object(), #cb_context{}, {proplist(), json_object()}) -> {proplist(), json_object()}. 
create_account(JObj, Context, {Pass, Fail}) ->
    Account = wh_json:get_value(<<"account">>, JObj, wh_json:new()),
    Generators = [fun(J) ->
                          Id = couch_mgr:get_uuid(), 
                          wh_json:set_value(<<"_id">>, Id, J) 
                  end
                  ,fun(J) ->
                           case wh_json:get_value(<<"first_name">>, J) of
                               undefined -> 
                                   RealmSuffix = whapps_config:get_binary(?OB_CONFIG_CAT, <<"account_realm_suffix">>, <<"sip.2600hz.com">>),
                                   Strength = whapps_config:get_integer(?OB_CONFIG_CAT, <<"realm_strength">>, 3),
                                   wh_json:set_value(<<"realm">>, list_to_binary([rand_chars(Strength), ".", RealmSuffix]), J);
                               _ -> J
                           end
                   end
                 ],
    Payload = [undefined
               ,Context#cb_context{req_data=lists:foldr(fun(F, J) -> F(J) end, Account, Generators)
                                   ,req_nouns=[{<<"accounts">>, []}]}
              ],
    case crossbar_bindings:fold(<<"v1_resource.validate.accounts">>, Payload) of
        [_, #cb_context{resp_status=success}=Context1 | _] ->
            {[{<<"accounts">>, Context1}|Pass], Fail};
        [_, #cb_context{resp_data=Errors} | _] ->
            {Pass, wh_json:set_value(<<"account">>, Errors, Fail)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use the bindings to validate and create a context
%% record to generate a user.  Any failure will be added to the error 
%% json object.
%% @end
%%--------------------------------------------------------------------
-spec create_user/4 :: (json_object(), pos_integer(), #cb_context{}, {proplist(), json_object()}) 
                       -> {proplist(), json_object()}. 
create_user(JObj, Iteration, Context, {Pass, Fail}) ->
    User = wh_json:get_value(<<"user">>, JObj, wh_json:new()),
    Generators = [fun(J) ->
                          Id = couch_mgr:get_uuid(), 
                          wh_json:set_value(<<"_id">>, Id, J) 
                  end
                  ,fun(J) when Iteration =:= 1 ->
                           %% ensure the first user is a admin
                           wh_json:set_value(<<"priv_level">>, <<"admin">>, J);
                      (J) -> J
                   end
                  ,fun(J) ->
                           case wh_json:get_value(<<"credentials">>, J) of
                               undefined -> J;
                               Creds -> 
                                   wh_json:set_value(<<"pvt_md5_auth">>, Creds, 
                                                     wh_json:delete_key(<<"credentials">>, J))
                           end 
                   end
                  ,fun(J) ->
                           case {wh_json:get_ne_value(<<"username">>, User), 
                                 wh_json:get_ne_value(<<"email">>, User)} of
                               {undefined, undefined} -> J;
                               {undefined, Email} -> 
                                   wh_json:set_value(<<"username">>, Email, J);
                               {_, _} -> J
                           end
                   end
                  ,fun(J) ->
                           case wh_json:get_value(<<"first_name">>, J) of
                               undefined -> 
                                   wh_json:set_value(<<"first_name">>, <<"User">>, J);
                               _ -> J
                           end
                   end
                  ,fun(J) ->
                           case wh_json:get_value(<<"last_name">>, J) of
                               undefined -> 
                                   wh_json:set_value(<<"last_name">>, wh_util:to_binary(Iteration), J);
                               _ -> J
                           end
                   end
                 ],
    Payload = [undefined
               ,Context#cb_context{req_data=lists:foldr(fun(F, J) -> F(J) end, User, Generators)}
              ],
    case crossbar_bindings:fold(<<"v1_resource.validate.users">>, Payload) of
        [_, #cb_context{resp_status=success}=Context1 | _] ->
            {[{<<"users">>, Context1}|Pass], Fail};
        [_, #cb_context{resp_data=Errors} | _] ->
            {Pass, wh_json:set_value(<<"user">>, Errors, Fail)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use the bindings to validate and create a context
%% record to generate a device.  Any failure will be added to the error 
%% json object.
%% @end
%%--------------------------------------------------------------------
-spec create_device/4 :: (json_object(), pos_integer(), #cb_context{}, {proplist(), json_object()}) 
                         -> {proplist(), json_object()}. 
create_device(JObj, Iteration, Context, {Pass, Fail}) ->
    Device = wh_json:get_value(<<"device">>, JObj, wh_json:new()),
    Generators = [fun(J) -> 
                          Id = couch_mgr:get_uuid(),
                          wh_json:set_value(<<"_id">>, Id, J) 
                  end
                  ,fun(J) -> 
                           User = get_context_jobj(<<"users">>, Pass),
                           case wh_json:get_value(<<"_id">>, User) of
                               undefined -> J;
                               OwnerId ->
                                   wh_json:set_value(<<"owner_id">>, OwnerId, J)
                           end
                   end
                  ,fun(J) -> 
                           case wh_json:get_ne_value(<<"name">>, J) of
                               undefined ->
                                   User = get_context_jobj(<<"users">>, Pass),
                                   FirstName = wh_json:get_value(<<"first_name">>, User, <<"User">>),
                                   LastName = wh_json:get_value(<<"last_name">>, User, wh_util:to_binary(Iteration)),
                                   Name = list_to_binary([FirstName, " ", LastName, "'s Device"]),
                                   wh_json:set_value(<<"name">>, Name, J);
                               _ ->
                                   J
                           end
                   end
                  ,fun(J) -> 
                           case wh_json:get_ne_value([<<"sip">>, <<"username">>], J) of
                               undefined ->
                                   Strength = whapps_config:get_integer(?OB_CONFIG_CAT, <<"device_username_strength">>, 3),
                                   wh_json:set_value([<<"sip">>, <<"username">>]
                                                     ,list_to_binary(["user_", rand_chars(Strength)]), J);
                               _ ->
                                   J
                           end
                   end
                  ,fun(J) -> 
                           case wh_json:get_ne_value([<<"sip">>, <<"password">>], J) of
                               undefined ->
                                   Strength = whapps_config:get_integer(?OB_CONFIG_CAT, <<"device_pwd_strength">>, 6),
                                   wh_json:set_value([<<"sip">>, <<"password">>]
                                                     ,rand_chars(Strength), J);
                               _ ->
                                   J
                           end
                   end
                 ],
    Payload = [undefined
               ,Context#cb_context{req_data=lists:foldr(fun(F, J) -> F(J) end, Device, Generators)}
              ],
    case crossbar_bindings:fold(<<"v1_resource.validate.devices">>, Payload) of
        [_, #cb_context{resp_status=success}=Context1 | _] ->
            {[{<<"devices">>, Context1}|Pass], Fail};
        [_, #cb_context{resp_data=Errors} | _] ->
            {Pass, wh_json:set_value(<<"device">>, Errors, Fail)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use the bindings to validate and create a context
%% record to generate a vmbox.  Any failure will be added to the error 
%% json object.
%% @end
%%--------------------------------------------------------------------
-spec create_vmbox/4 :: (json_object(), pos_integer(), #cb_context{}, {proplist(), json_object()}) 
                        -> {proplist(), json_object()}. 
create_vmbox(JObj, Iteration, Context, {Pass, Fail}) ->
    VMBox = wh_json:get_value(<<"vmbox">>, JObj, wh_json:new()),
    Generators = [fun(J) -> 
                          Id = couch_mgr:get_uuid(),
                          wh_json:set_value(<<"_id">>, Id, J) 
                  end
                  ,fun(J) -> 
                           User = get_context_jobj(<<"users">>, Pass),
                           case wh_json:get_value(<<"_id">>, User) of
                               undefined -> J;
                               OwnerId ->
                                   wh_json:set_value(<<"owner_id">>, OwnerId, J)
                           end
                   end
                  ,fun(J) ->  
                           case wh_json:get_ne_value(<<"mailbox">>, J) of
                               undefined ->
                                   StartExten = whapps_config:get_integer(?OB_CONFIG_CAT, <<"default_vm_start_exten">>, 3000),
                                   wh_json:set_value(<<"mailbox">>, wh_util:to_binary(StartExten + Iteration), J);
                               _ ->
                                   J
                           end
                   end
                  ,fun(J) -> 
                           case wh_json:get_ne_value(<<"name">>, J) of
                               undefined ->
                                   User = get_context_jobj(<<"users">>, Pass),
                                   FirstName = wh_json:get_value(<<"first_name">>, User, <<"User">>),
                                   LastName = wh_json:get_value(<<"last_name">>, User, wh_util:to_binary(Iteration)),
                                   Name = list_to_binary([FirstName, " ", LastName, "'s Voicemail"]),
                                   wh_json:set_value(<<"name">>, Name, J);
                               _ ->
                                   J
                           end
                   end
                 ],
    Payload = [undefined
               ,Context#cb_context{req_data=lists:foldr(fun(F, J) -> F(J) end, VMBox, Generators)}
              ],
    case crossbar_bindings:fold(<<"v1_resource.validate.vmboxes">>, Payload) of
        [_, #cb_context{resp_status=success}=Context1 | _] ->
            {[{<<"vmboxes">>, Context1}|Pass], Fail};
        [_, #cb_context{resp_data=Errors} | _] ->
            {Pass, wh_json:set_value(<<"vmbox">>, Errors, Fail)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use the bindings to validate and create a context
%% record to generate a extension callflow.  Any failure will be added 
%% to the error json object.
%% @end
%%--------------------------------------------------------------------
-spec create_exten_callflow/4 :: (json_object(), pos_integer(), #cb_context{}, {proplist(), json_object()}) 
                                 -> {proplist(), json_object()}. 
create_exten_callflow(JObj, Iteration, Context, {Pass, Fail}) ->
    Callflow = wh_json:get_value(<<"callflow">>, JObj, wh_json:new()),
    Generators = [fun(J) -> 
                          User = get_context_jobj(<<"users">>, Pass),
                          VMBox = get_context_jobj(<<"vmboxes">>, Pass),
                          DefaultFlow = whapps_config:get_string(?OB_CONFIG_CAT, <<"default_extension_callflow">>
                                                                     ,wh_util:to_binary(?DEFAULT_FLOW)),
                          Flow = wh_json:decode(io_lib:format(DefaultFlow, [wh_json:get_value(<<"_id">>, User)
                                                                            ,wh_json:get_value(<<"_id">>, VMBox)
                                                                           ])),
                          wh_json:set_value(<<"flow">>, Flow, J) 
                  end
                  ,fun(J) ->
                           Id = couch_mgr:get_uuid(),
                           wh_json:set_value(<<"_id">>, Id, J) 
                   end
                  ,fun(J) ->
                           case wh_json:get_value(<<"numbers">>, J, []) of
                               [] ->
                                   StartExten = whapps_config:get_integer(?OB_CONFIG_CAT, <<"default_callflow_start_exten">>, 2000),
                                   wh_json:set_value(<<"numbers">>, [wh_util:to_binary(StartExten + Iteration)], J);
                               _ -> J
                           end     
                   end
                 ],
    Payload = [undefined
               ,Context#cb_context{req_data=lists:foldr(fun(F, J) -> F(J) end, Callflow, Generators)}
              ],
    case crossbar_bindings:fold(<<"v1_resource.validate.callflows">>, Payload) of
        [_, #cb_context{resp_status=success}=Context1 | _] ->
            {[{<<"callflows">>, Context1}|Pass], Fail};
        [_, #cb_context{resp_data=Errors} | _] ->
            {Pass, wh_json:set_value(<<"callflow">>, Errors, Fail)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will loop over the prevously generated context records
%% providing each to the respective 'put' binding in order to create
%% the objects.  Starts with the account :)
%% @end
%%--------------------------------------------------------------------
-spec populate_new_account/2 :: (proplist(), #cb_context{}) -> #cb_context{}.
-spec populate_new_account/3 :: (proplist(), ne_binary(), json_object()) -> json_object().

populate_new_account(Props, _) ->
    Payload = [undefined
               ,props:get_value(<<"accounts">>, Props)
              ],
    case crossbar_bindings:fold(<<"v1_resource.execute.put.accounts">>, Payload) of
        [_, #cb_context{resp_status=success, db_name=AccountDb}=Context | _] ->
            populate_new_account(proplists:delete(<<"accounts">>, Props), AccountDb, []),
            Context;
        [_, #cb_context{resp_data=Error} | _] ->
            complete_failure
    end.

populate_new_account([], _, Errors) ->
    Errors;
populate_new_account([{Event, Context}|Props], AccountDb, Errors) ->
    Payload = [undefined
               ,Context#cb_context{db_name=AccountDb}
              ],
    case crossbar_bindings:fold(<<"v1_resource.execute.put.", Event/binary>>, Payload) of
        [_, #cb_context{resp_status=success} | _] ->
            populate_new_account(Props, AccountDb, Errors);
        [_, #cb_context{resp_data=Error} | _] ->
            populate_new_account(Props, AccountDb, [Error|Errors])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Helper function to get the create object out of the successful 
%% context records for a specific key.
%% @end
%%--------------------------------------------------------------------
-spec get_context_jobj/2 :: (ne_binary(), proplist()) -> json_object().
get_context_jobj(Key, Pass) ->
    case props:get_value(Key, Pass) of
        #cb_context{doc=JObj} ->
            JObj;
        _ ->
            wh_json:new()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Helper function to generate random strings
%% @end
%%--------------------------------------------------------------------
-spec rand_chars/1 :: (pos_integer()) -> ne_binary().
rand_chars(Count) ->
    wh_util:to_binary(wh_util:to_hex(crypto:rand_bytes(Count))).
