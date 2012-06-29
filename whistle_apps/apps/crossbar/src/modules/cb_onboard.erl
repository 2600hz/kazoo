%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Handle client requests for onboard documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_onboard).

-export([init/0
         ,allowed_methods/0, allowed_methods/2
         ,resource_exists/0, resource_exists/2
         ,validate/1, validate/3
         ,authorize/1
         ,authenticate/1
         ,put/1
        ]).

-include_lib("crossbar/include/crossbar.hrl").

-define(OB_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".onboard">>).
-define(DEFAULT_FLOW, "{\"data\": { \"id\": \"~s\" }, \"module\": \"user\", \"children\": { \"_\": { \"data\": { \"id\": \"~s\" }, \"module\": \"voicemail\", \"children\": {}}}}").

% Invite defines
-define(INVITE_API, <<"invite">>).
-define(INVITE_DB, <<"invite_codes">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, authenticate),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.onboard">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.onboard">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.onboard">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.get.onboard">>, ?MODULE, get),
    crossbar_bindings:bind(<<"v1_resource.execute.put.onboard">>, ?MODULE, put).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
allowed_methods() ->
    ['PUT'].

allowed_methods(?INVITE_API, _) ->
    ['GET'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
resource_exists() ->
    true.

resource_exists(?INVITE_API, _) ->
    true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
validate(#cb_context{req_data=JObj, req_verb = <<"put">>}=Context) ->
    Generators = [fun(R) -> create_extensions(JObj, Context, R) end
                  ,fun(R) -> create_phone_numbers(JObj, Context, R) end
                  ,fun(R) -> create_braintree_cards(JObj, Context, R) end
                  ,fun(R) -> create_account(JObj, Context, R) end
                 ],
    case lists:foldr(fun(F, Acc) -> F(Acc) end, {[], wh_json:new()}, Generators) of
        {P, Failures} ->
            case wh_json:is_empty(Failures) of
                true ->
                    Context#cb_context{
                      doc=lists:flatten(P)
                      ,resp_status=success
                     };
                false ->
                    crossbar_util:response_invalid_data(Failures, Context)
            end
    end.

validate(#cb_context{req_verb = <<"get">>}=Context, ?INVITE_API, Id) ->
    load_invite_code(Context, Id).


authorize(#cb_context{req_nouns=[{<<"onboard">>,[]}]
                      ,req_verb = <<"put">>}) ->
    true;
authorize(#cb_context{req_nouns=[{<<"onboard">>,[?INVITE_API,_]}]
                      ,req_verb = <<"get">>}) ->
    true.

authenticate(#cb_context{req_nouns=[{<<"onboard">>,[]}]
                         ,req_verb = <<"put">>}) ->
    true;
authenticate(#cb_context{req_nouns=[{<<"onboard">>,[?INVITE_API,_]}]
                         ,req_verb = <<"get">>}) ->
    true.

put(#cb_context{req_data=JObj, doc=Data}=Context) ->
    case wh_json:get_ne_value(<<"invite_code">>, JObj) of
        undefined ->
            crossbar_util:response(error, <<"request must contain valid invite code">>, 401, Context);
        InviteId ->
            case load_invite_code(Context, InviteId) of
                #cb_context{resp_status=success, doc=Doc}=IContext ->
                    IContext1 = save_invite_code(IContext, wh_json:set_value(<<"status">>, <<"used">>, Doc)),
                    case populate_new_account(Data, Context) of
                        #cb_context{account_id=undefined}=Else ->
                            save_invite_code(IContext1, wh_json:set_value(<<"status">>, <<"unused">>, IContext1#cb_context.doc)),
                            create_response(Else);
                        #cb_context{account_id=AcctId}=Context1 ->
                            save_invite_code(IContext1, wh_json:set_value(<<"used_by">>, AcctId, IContext1#cb_context.doc)),
                            create_response(Context1)
                    end;
                Else ->
                    Else
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will loop over all the 'extensions' and collect
%% valid context's for users, voicemailboxes, devices, and callflows.
%% Any errors will also be collected.
%% @end
%%--------------------------------------------------------------------
-spec create_extensions/3 :: (wh_json:json_object(), #cb_context{}, {proplist(), wh_json:json_object()}) -> {proplist(), wh_json:json_object()}. 
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
        {P, F} ->
            case wh_json:is_empty(F) of
                true ->
                    create_extensions(Extens, Iteration + 1, Context, {[P|PassAcc], FailAcc});
                false ->
                    Failures = wh_json:set_value([<<"extensions">>, Iteration], F, FailAcc),
                    create_extensions(Extens, Iteration + 1, Context, {[P|PassAcc], Failures})
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use the bindings to validate and create a context
%% record to generate an account.  Any failure will be added to the error 
%% json object.
%% @end
%%--------------------------------------------------------------------
-spec create_account/3 :: (wh_json:json_object(), #cb_context{}, {proplist(), wh_json:json_object()}) -> {proplist(), wh_json:json_object()}. 
create_account(JObj, Context, {Pass, Fail}) ->
    Account = wh_json:get_value(<<"account">>, JObj, wh_json:new()),
    Generators = [fun(J) ->
                          Id = couch_mgr:get_uuid(), 
                          wh_json:set_value(<<"_id">>, Id, J) 
                  end
                 ],
    Payload = [Context#cb_context{req_data=lists:foldr(fun(F, J) -> F(J) end, Account, Generators)
                                  ,req_nouns=[{?WH_ACCOUNTS_DB, []}]}
              ],
    case crossbar_bindings:fold(<<"v1_resource.validate.accounts">>, Payload) of
        #cb_context{resp_status=success}=Context1 ->
            {[{?WH_ACCOUNTS_DB, Context1}|Pass], Fail};
        #cb_context{resp_data=Errors} ->
            {Pass, wh_json:set_value(<<"account">>, Errors, Fail)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use the bindings to validate and create a context
%% record to generate an account.  Any failure will be added to the error 
%% json object.
%% @end
%%--------------------------------------------------------------------
-spec create_phone_numbers/3 :: (wh_json:json_object(), #cb_context{}, {proplist(), wh_json:json_object()}) -> {proplist(), wh_json:json_object()}. 

create_phone_numbers(JObj, Context, Results) ->
    PhoneNumbers = wh_json:get_value(<<"phone_numbers">>, JObj),
    lists:foldr(fun(Number, R) ->
                        create_phone_number(Number
                                            ,wh_json:get_value([<<"phone_numbers">>, Number], JObj)
                                            ,Context, R)
                end, Results, wh_json:get_keys(PhoneNumbers)).

create_phone_number(Number, Properties, Context, {Pass, Fail}) ->
    Payload = [Context#cb_context{req_data=Properties
                                  ,db_name = <<"--">>}
               ,Number, <<"activate">>
              ],
    case crossbar_bindings:fold(<<"v1_resource.validate.phone_numbers">>, Payload) of
        #cb_context{resp_status=success}=Context1 ->
            {[{<<"phone_numbers">>, Context1#cb_context{storage=[{number, Number}]}}|Pass], Fail};
        #cb_context{resp_data=Errors} ->
            {Pass, wh_json:set_value([<<"phone_numbers">>, Number], Errors, Fail)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use the bindings to validate and create a context
%% record to generate an braintree_customer.  Any failure will be added to the error 
%% json object.
%% @end
%%--------------------------------------------------------------------
-spec create_braintree_cards/3 :: (wh_json:json_object(), #cb_context{}, {proplist(), wh_json:json_object()}) -> {proplist(), wh_json:json_object()}. 
create_braintree_cards(JObj, Context, {Pass, Fail}) ->
    Account = get_context_jobj(<<"accounts">>, Pass),
    case wh_json:get_value(<<"_id">>, Account) of
        undefined ->
            Error = wh_json:set_value([<<"account_id">>, <<"required">>], <<"account failed validation">>, wh_json:new()),
            {Pass, wh_json:set_value(<<"braintree">>, Error, Fail)};
        AccountId ->
            Customer = wh_json:get_value(<<"braintree">>, JObj, wh_json:new()),
            Generators = [fun(J) ->
                                  case wh_json:get_ne_value(<<"credit_card">>, J) of
                                      undefined -> wh_json:set_value(<<"credit_card">>, wh_json:new(), J);
                                      _Else -> J
                                  end
                          end
                         ],
            Payload = [Context#cb_context{req_data=lists:foldr(fun(F, J) -> F(J) end, Customer, Generators)
                                          ,account_id=AccountId
                                          ,req_verb = <<"post">>}
                       ,<<"customer">>
                      ],
            case crossbar_bindings:fold(<<"v1_resource.validate.braintree">>, Payload) of
                #cb_context{resp_status=success}=Context1 ->
                    {[{<<"braintree">>, Context1}|Pass], Fail};
                #cb_context{resp_data=Errors} ->
                    {Pass, wh_json:set_value(<<"braintree">>, Errors, Fail)}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use the bindings to validate and create a context
%% record to generate a user.  Any failure will be added to the error 
%% json object.
%% @end
%%--------------------------------------------------------------------
-spec create_user/4 :: (wh_json:json_object(), pos_integer(), #cb_context{}, {proplist(), wh_json:json_object()}) 
                       -> {proplist(), wh_json:json_object()}. 
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
                           case wh_json:get_ne_value(<<"credentials">>, J) of
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
                           case wh_json:get_ne_value(<<"first_name">>, J) of
                               undefined -> 
                                   wh_json:set_value(<<"first_name">>, <<"User">>, J);
                               _ -> J
                           end
                   end
                  ,fun(J) ->
                           case wh_json:get_ne_value(<<"last_name">>, J) of
                               undefined -> 
                                   wh_json:set_value(<<"last_name">>, wh_util:to_binary(Iteration), J);
                               _ -> J
                           end
                   end
                 ],
    Payload = [Context#cb_context{req_data=lists:foldr(fun(F, J) -> F(J) end, User, Generators)}],
    case crossbar_bindings:fold(<<"v1_resource.validate.users">>, Payload) of
        #cb_context{resp_status=success}=Context1 ->
            {[{<<"users">>, Context1#cb_context{storage=[{iteration, Iteration}]}}|Pass], Fail};
        #cb_context{resp_data=Errors} ->
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
-spec create_device/4 :: (wh_json:json_object(), pos_integer(), #cb_context{}, {proplist(), wh_json:json_object()}) 
                         -> {proplist(), wh_json:json_object()}. 
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
                                                     ,list_to_binary(["user_", wh_util:rand_hex_binary(Strength)]), J);
                               _ ->
                                   J
                           end
                   end
                  ,fun(J) -> 
                           case wh_json:get_ne_value([<<"sip">>, <<"password">>], J) of
                               undefined ->
                                   Strength = whapps_config:get_integer(?OB_CONFIG_CAT, <<"device_pwd_strength">>, 6),
                                   wh_json:set_value([<<"sip">>, <<"password">>]
                                                     ,wh_util:rand_hex_binary(Strength), J);
                               _ ->
                                   J
                           end
                   end
                 ],
    Payload = [Context#cb_context{req_data=lists:foldr(fun(F, J) -> F(J) end, Device, Generators)}],
    case crossbar_bindings:fold(<<"v1_resource.validate.devices">>, Payload) of
        #cb_context{resp_status=success}=Context1 ->
            {[{<<"devices">>, Context1#cb_context{storage=[{iteration, Iteration}]}}|Pass], Fail};
        #cb_context{resp_data=Errors} ->
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
-spec create_vmbox/4 :: (wh_json:json_object(), pos_integer(), #cb_context{}, {proplist(), wh_json:json_object()}) 
                        -> {proplist(), wh_json:json_object()}. 
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
    Payload = [Context#cb_context{req_data=lists:foldr(fun(F, J) -> F(J) end, VMBox, Generators)}],
    case crossbar_bindings:fold(<<"v1_resource.validate.vmboxes">>, Payload) of
        #cb_context{resp_status=success}=Context1 ->
            {[{<<"vmboxes">>, Context1#cb_context{storage=[{iteration, Iteration}]}}|Pass], Fail};
        #cb_context{resp_data=Errors} ->
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
-spec create_exten_callflow/4 :: (wh_json:json_object(), pos_integer(), #cb_context{}, {proplist(), wh_json:json_object()}) 
                                 -> {proplist(), wh_json:json_object()}. 
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
                           case [Num || Num <- wh_json:get_ne_value(<<"numbers">>, J, []), not wh_util:is_empty(Num)] of
                               [] ->
                                   StartExten = whapps_config:get_integer(?OB_CONFIG_CAT
                                                                          ,<<"default_callflow_start_exten">>
                                                                          ,2000),
                                   wh_json:set_value(<<"numbers">>, [wh_util:to_binary(StartExten + Iteration)], J);
                               _ -> J
                           end     
                   end
                 ],
    Payload = [Context#cb_context{req_data=lists:foldr(fun(F, J) -> F(J) end, Callflow, Generators)}],
    case crossbar_bindings:fold(<<"v1_resource.validate.callflows">>, Payload) of
        #cb_context{resp_status=success}=Context1 ->
            {[{<<"callflows">>, Context1#cb_context{storage=[{iteration, Iteration}]}}|Pass], Fail};
        #cb_context{resp_data=Errors} ->
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
-spec populate_new_account/3 :: (proplist(), ne_binary(), wh_json:json_object()) -> wh_json:json_object().

populate_new_account(Props, _) ->
    Context = props:get_value(?WH_ACCOUNTS_DB, Props),
    Payload = [Context],
    case crossbar_bindings:fold(<<"v1_resource.execute.put.accounts">>, Payload) of
        #cb_context{resp_status=success, db_name=AccountDb, account_id=AccountId, doc=JObj}=Context1 ->
            Results = populate_new_account(prepare_props(Props), AccountDb, wh_json:new()),
            Errors = wh_json:get_value(<<"errors">>, Results),
            case wh_util:is_empty(Errors) of
                true ->
                    lager:debug("new account created ~s (~s)", [AccountId, AccountDb]),
                    notfy_new_account(JObj),
                    Context1#cb_context{doc=wh_json:set_value(<<"account_id">>, AccountId, Results)};
                false ->
                    lager:debug("account creation errors: ~s", [wh_json:encode(Errors)]),
                    catch (crossbar_bindings:fold(<<"v1_resource.execute.delete.accounts">>, [Context1, AccountId])),
                    Context1#cb_context{doc=wh_json:delete_key(<<"owner_id">>, Results), account_id=undefined}
            end;
        #cb_context{resp_error_msg=ErrorMsg, resp_data=ErrorData}=ErrorContext ->
            AccountId = wh_json:get_value(<<"_id">>, Context#cb_context.req_data),
            couch_mgr:db_delete(wh_util:format_account_id(AccountId, encoded)),
            ErrorContext#cb_context{doc=wh_json:set_value(wh_util:to_binary(ErrorMsg), ErrorData, wh_json:new()), account_id=undefined}
    end.

populate_new_account([], _, Results) ->
    Results;

populate_new_account([{<<"phone_numbers">>, #cb_context{storage=[{number, Number}]}=Context}|Props], AccountDb, Results) ->
    AccountId = wh_util:format_account_id(AccountDb, raw),
    Payload = [Context#cb_context{db_name=AccountDb
                                  ,auth_account_id=AccountId
                                  ,account_id=AccountId}
               ,Number, <<"activate">>
              ],
    case crossbar_bindings:fold(<<"v1_resource.execute.put.phone_numbers">>, Payload) of
        #cb_context{resp_status=success} ->
            populate_new_account(Props, AccountDb, Results);
        #cb_context{resp_error_msg=ErrMsg, resp_error_code=ErrCode, resp_data=ErrData} ->
            Error = wh_json:from_list([{<<"error">>, ErrCode}, {<<"message">>, ErrMsg}, {<<"data">>, ErrData}]),
            populate_new_account(Props, AccountDb
                                 ,wh_json:set_value([<<"errors">>, <<"phone_numbers">>, Number], Error, Results))
    end;

populate_new_account([{<<"braintree">>, Context}|Props], AccountDb, Results) ->
    AccountId = wh_util:format_account_id(AccountDb, raw),
    Payload = [Context#cb_context{db_name=AccountDb
                                  ,account_id=AccountId
                                  ,req_verb = <<"post">>}
               ,<<"customer">>
              ],
    case crossbar_bindings:fold(<<"v1_resource.execute.post.braintree">>, Payload) of
        #cb_context{resp_status=success} ->
            populate_new_account(Props, AccountDb, Results);
        #cb_context{resp_error_msg=ErrMsg, resp_error_code=ErrCode, resp_data=ErrData} ->
            _ = crossbar_util:disable_account(AccountId),
            Error = wh_json:from_list([{<<"error">>, ErrCode}, {<<"message">>, ErrMsg}, {<<"data">>, ErrData}]),
            populate_new_account(Props, AccountDb
                                 ,wh_json:set_value([<<"errors">>, <<"braintree">>], Error, Results))
    end;

populate_new_account([{Event, #cb_context{storage=[{iteration, Iteration}]}=Context}|Props], AccountDb, Results) ->
    Payload = [Context#cb_context{db_name=AccountDb}],
    case crossbar_bindings:fold(<<"v1_resource.execute.put.", Event/binary>>, Payload) of
        #cb_context{resp_status=success, doc=JObj} ->
            case wh_json:get_value(<<"priv_level">>, JObj) of
                <<"admin">> ->
                    populate_new_account(Props, AccountDb
                                         ,wh_json:set_value(<<"owner_id">>, wh_json:get_value(<<"_id">>, JObj), Results));
                _ ->
                    populate_new_account(Props, AccountDb, Results)
            end;
        #cb_context{resp_error_msg=ErrMsg, resp_error_code=ErrCode, resp_data=ErrData} ->
            Error = wh_json:from_list([{<<"error">>, ErrCode}, {<<"message">>, ErrMsg}, {<<"data">>, ErrData}]),
            populate_new_account(Props, AccountDb
                                 ,wh_json:set_value([<<"errors">>, Event, Iteration], Error, Results))
    end.

prepare_props(Props) ->
    lists:sort(fun({<<"braintree">>, _}, {_, _}) -> true;
                   (_, _) -> false
               end, proplists:delete(?WH_ACCOUNTS_DB, Props)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Helper function to get the create object out of the successful 
%% context records for a specific key.
%% @end
%%--------------------------------------------------------------------
-spec get_context_jobj/2 :: (ne_binary(), proplist()) -> wh_json:json_object().
get_context_jobj(Key, Pass) ->
    case props:get_value(Key, Pass) of
        #cb_context{doc=JObj} -> JObj;
        _ -> wh_json:new()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token and save it to the token db
%% @end
%%--------------------------------------------------------------------
-spec create_response/1 :: (#cb_context{}) -> #cb_context{}.
create_response(#cb_context{doc=JObj, account_id=undefined}=Context) ->
    crossbar_util:response_invalid_data(JObj, Context);
create_response(#cb_context{doc=JObj, account_id=AccountId}=Context) ->
    Token = [{<<"account_id">>, AccountId}
             ,{<<"owner_id">>, wh_json:get_value(<<"owner_id">>, JObj)}
             ,{<<"created">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
             ,{<<"modified">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
             ,{<<"method">>, wh_util:to_binary(?MODULE)}
            ],
    case couch_mgr:save_doc(?TOKEN_DB, wh_json:from_list(Token)) of
        {ok, Doc} ->
            AuthToken = wh_json:get_value(<<"_id">>, Doc),
            lager:debug("created new local auth token ~s", [AuthToken]),
            crossbar_util:response(wh_json:set_value(<<"auth_token">>, AuthToken, JObj)
                                   ,Context#cb_context{auth_token=AuthToken, auth_doc=Doc});
        {error, R} ->
            lager:debug("could not create new local auth token, ~p", [R]),
            crossbar_util:response(error, JObj, 400, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token and save it to the token db
%% @end
%%--------------------------------------------------------------------
-spec notfy_new_account/1 :: (wh_json:json_object()) -> ok.
notfy_new_account(JObj) ->
    Notify = [{<<"Account-Name">>, wh_json:get_value(<<"name">>, JObj)}
              ,{<<"Account-Realm">>, wh_json:get_value(<<"realm">>, JObj)}
              ,{<<"Account-API-Key">>, wh_json:get_value(<<"pvt_api_key">>, JObj)}
              ,{<<"Account-ID">>, wh_json:get_value(<<"pvt_account_id">>, JObj)}
              ,{<<"Account-DB">>, wh_json:get_value(<<"pvt_account_db">>, JObj)}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_new_account(Notify).

load_invite_code(Context, Id) ->
    case crossbar_doc:load(Id, Context#cb_context{db_name=?INVITE_DB}) of
        #cb_context{resp_status=success, doc=Doc}=Context1 ->
            case wh_json:get_ne_value(<<"status">>, Doc) of
                undefined ->
                    Context1#cb_context{resp_data=wh_json:set_value(<<"status">>, <<"unused">>, wh_json:new())};
                <<"unused">> ->
                    Context1#cb_context{resp_data=wh_json:set_value(<<"status">>, <<"unused">>, wh_json:new())};
                _Else ->
                    crossbar_util:response(error, <<"invite code has already been used">>, 410, Context)
            end;
        _Else ->
            crossbar_util:response(error, <<"invite code not found">>, 404, Context)
    end.

save_invite_code(Context, Doc) ->
    crossbar_doc:save(Context#cb_context{db_name=?INVITE_DB, doc=Doc}).
