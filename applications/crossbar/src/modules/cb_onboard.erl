%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Handle client requests for onboard documents
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_onboard).

-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,validate/1
        ,authorize/1
        ,authenticate/1
        ,put/1
        ]).

-include("crossbar.hrl").

-define(OB_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".onboard">>).
-define(DEFAULT_FLOW, <<"{\"data\": { \"id\": \"~s\" }, \"module\": \"user\", \"children\": { \"_\": { \"data\": { \"id\": \"~s\" }, \"module\": \"voicemail\", \"children\": {}}}}">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.onboard">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.onboard">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.onboard">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.onboard">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.onboard">>, ?MODULE, 'put'),
    ok.

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize(cb_context:req_verb(Context), cb_context:req_nouns(Context)).
authorize(?HTTP_PUT, [{<<"onboard">>,[]}]) -> 'true';
authorize(_, _) -> 'false'.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate(cb_context:req_verb(Context), cb_context:req_nouns(Context)).
authenticate(?HTTP_PUT, [{<<"onboard">>,[]}]) -> 'true';
authenticate(_, _) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_PUT].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() ->
    'true'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate(Context, cb_context:req_verb(Context)).

validate(Context, ?HTTP_PUT) ->
    JObj = cb_context:req_data(Context),
    Generators = [fun(R) -> create_extensions(JObj, Context, R) end
                 ,fun(R) -> create_phone_numbers(JObj, Context, R) end
                 ,fun(R) -> create_braintree_cards(JObj, Context, R) end
                 ,fun(R) -> create_account(JObj, Context, R) end
                 ],
    {P, Failures} = lists:foldr(fun(F, Acc) -> F(Acc) end, {[], kz_json:new()}, Generators),
    case kz_json:is_empty(Failures) of
        'true' ->
            cb_context:setters(Context, [{fun cb_context:set_doc/2, lists:flatten(P)}
                                        ,{fun cb_context:set_resp_status/2, 'success'}
                                        ]);
        'false' ->
            crossbar_util:response_invalid_data(Failures, Context)
    end.

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Data = cb_context:doc(Context),
    Context1 = populate_new_account(Data, Context),
    create_response(Context1).

%%------------------------------------------------------------------------------
%% @doc This function will loop over all the 'extensions' and collect
%% valid context's for users, voicemail boxes, devices, and callflows.
%% Any errors will also be collected.
%% @end
%%------------------------------------------------------------------------------
-spec create_extensions(kz_json:object(), cb_context:context(), {kz_term:proplist(), kz_json:object()}) -> {kz_term:proplist(), kz_json:object()}.
create_extensions(JObj, Context, Results) ->
    Extensions = kz_json:get_value(<<"extensions">>, JObj, []),
    create_extensions(Extensions, 1, Context, Results).

create_extensions([], _, _, Results) ->
    Results;
create_extensions([Exten|Extens], Iteration, Context, {PassAcc, FailAcc}) ->
    Generators = [fun(R) -> create_exten_callflow(Exten, Iteration, Context, R) end
                 ,fun(R) -> create_vmbox(Exten, Iteration, Context, R) end
                 ,fun(R) -> create_device(Exten, Iteration, Context, R) end
                 ,fun(R) -> create_user(Exten, Iteration, Context, R) end
                 ],
    {P, F} = lists:foldr(fun(F, Acc) -> F(Acc) end, {[], kz_json:new()}, Generators),
    case kz_json:is_empty(F) of
        'true' ->
            create_extensions(Extens, Iteration + 1, Context, {[P|PassAcc], FailAcc});
        'false' ->
            Failures = kz_json:set_value([<<"extensions">>, kz_term:to_binary(Iteration)], F, FailAcc),
            create_extensions(Extens, Iteration + 1, Context, {[P|PassAcc], Failures})
    end.

%%------------------------------------------------------------------------------
%% @doc This function will use the bindings to validate and create a context
%% record to generate an account.  Any failure will be added to the error
%% json object.
%% @end
%%------------------------------------------------------------------------------
-spec create_account(kz_json:object(), cb_context:context(), {kz_term:proplist(), kz_json:object()}) ->
                            {kz_term:proplist(), kz_json:object()}.
create_account(JObj, Context, {Pass, Fail}) ->
    Account = kz_json:get_value(<<"account">>, JObj, kz_json:new()),
    Generators = [fun(J) -> kz_doc:set_id(J, kz_datamgr:get_uuid()) end
                 ],
    NewReqData = lists:foldr(fun(F, J) -> F(J) end, Account, Generators),
    Payload = [cb_context:setters(Context, [{fun cb_context:set_req_data/2, NewReqData}
                                           ,{fun cb_context:set_req_nouns/2, [{?KZ_ACCOUNTS_DB, []}]}
                                           ])
              ],
    Context1 = crossbar_bindings:fold(<<"*.validate.accounts">>, Payload),
    case cb_context:response(Context1) of
        {'ok', _} -> {[{?KZ_ACCOUNTS_DB, Context1}|Pass], Fail};
        {'error', {_, _, Errors}} -> {Pass, kz_json:set_value(<<"account">>, Errors, Fail)}
    end.

%%------------------------------------------------------------------------------
%% @doc This function will use the bindings to validate and create a context
%% record to generate an account.  Any failure will be added to the error
%% json object.
%% @end
%%------------------------------------------------------------------------------
-spec create_phone_numbers(kz_json:object(), cb_context:context(), {kz_term:proplist(), kz_json:object()}) ->
                                  {kz_term:proplist(), kz_json:object()}.
create_phone_numbers(JObj, Context, Results) ->
    PhoneNumbers = kz_json:get_value(<<"phone_numbers">>, JObj),
    lists:foldr(fun(Number, R) ->
                        create_phone_number(Number
                                           ,kz_json:get_value([<<"phone_numbers">>, Number], JObj)
                                           ,Context, R)
                end, Results, kz_json:get_keys(PhoneNumbers)).

create_phone_number(Number, Properties, Context, {Pass, Fail}) ->
    Payload = [cb_context:setters(Context, [{fun cb_context:set_req_data/2, Properties}
                                           ,{fun cb_context:set_account_db/2, <<"--">>}
                                           ])
              ,Number
              ,<<"activate">>
              ],
    Context1 = crossbar_bindings:fold(<<"*.validate.phone_numbers">>, Payload),
    case cb_context:response(Context1) of
        {'ok', _} -> {[{<<"phone_numbers">>, cb_context:store(Context1, 'number', Number)}|Pass], Fail};
        {'error', {_, _, Errors}} -> {Pass, kz_json:set_value(<<"phone_numbers">>, Errors, Fail)}
    end.

%%------------------------------------------------------------------------------
%% @doc This function will use the bindings to validate and create a context
%% record to generate an braintree_customer.  Any failure will be added to the error
%% json object.
%% @end
%%------------------------------------------------------------------------------
-spec create_braintree_cards(kz_json:object(), cb_context:context(), {kz_term:proplist(), kz_json:object()}) -> {kz_term:proplist(), kz_json:object()}.
create_braintree_cards(JObj, Context, {Pass, Fail}) ->
    Account = get_context_jobj(<<"accounts">>, Pass),
    case kz_doc:id(Account) of
        'undefined' ->
            Error = kz_json:set_value([<<"account_id">>, <<"required">>], <<"account failed validation">>, kz_json:new()),
            {Pass, kz_json:set_value(<<"braintree">>, Error, Fail)};
        AccountId ->
            Customer = kz_json:get_value(<<"braintree">>, JObj, kz_json:new()),
            Generators = [fun(J) ->
                                  case kz_json:get_ne_value(<<"credit_card">>, J) of
                                      'undefined' -> kz_json:set_value(<<"credit_card">>, kz_json:new(), J);
                                      _Else -> J
                                  end
                          end
                         ],
            NewReaData = lists:foldr(fun(F, J) -> F(J) end, Customer, Generators),
            Payload = [cb_context:setters(Context, [{fun cb_context:set_req_data/2, NewReaData}
                                                   ,{fun cb_context:set_account_id/2, AccountId}
                                                   ,{fun cb_context:set_req_verb/2, ?HTTP_POST}
                                                   ])
                      ,<<"customer">>
                      ],
            Context1 = crossbar_bindings:fold(<<"*.validate.braintree">>, Payload),
            case cb_context:response(Context1) of
                {'ok', _} -> {[{<<"braintree">>, Context1}|Pass], Fail};
                {'error', {_, _, Errors}} -> {Pass, kz_json:set_value(<<"braintree">>, Errors, Fail)}
            end
    end.

%%------------------------------------------------------------------------------
%% @doc This function will use the bindings to validate and create a context
%% record to generate a user.  Any failure will be added to the error
%% json object.
%% @end
%%------------------------------------------------------------------------------
-spec create_user(kz_json:object(), pos_integer(), cb_context:context(), {kz_term:proplist(), kz_json:object()})
                 -> {kz_term:proplist(), kz_json:object()}.
create_user(JObj, Iteration, Context, {Pass, Fail}) ->
    User = kz_json:get_value(<<"user">>, JObj, kz_json:new()),
    Generators = [fun(J) -> kz_doc:set_id(J, kz_datamgr:get_uuid()) end
                 ,fun(J) when Iteration =:= 1 ->
                          %% ensure the first user is a admin
                          kz_json:set_value(<<"priv_level">>, <<"admin">>, J);
                     (J) -> J
                  end
                 ,fun(J) ->
                          case kz_json:get_ne_value(<<"first_name">>, J) of
                              'undefined' ->
                                  kz_json:set_value(<<"first_name">>, <<"User">>, J);
                              _ -> J
                          end
                  end
                 ,fun(J) ->
                          case kz_json:get_ne_value(<<"last_name">>, J) of
                              'undefined' ->
                                  kz_json:set_value(<<"last_name">>, kz_term:to_binary(Iteration), J);
                              _ -> J
                          end
                  end
                 ,fun(J) ->
                          case kz_json:get_ne_value(<<"username">>, User) of
                              'undefined' ->
                                  Email = kz_json:get_ne_value(<<"email">>, J),
                                  FirstName = kz_json:get_ne_value(<<"first_name">>, J),
                                  LastName = kz_json:get_ne_value(<<"last_name">>, J),
                                  Username = generate_username(Email, FirstName, LastName),
                                  kz_json:set_value(<<"username">>, Username, J);
                              _Else -> J
                          end
                  end
                 ],
    NewReqData = lists:foldr(fun(F, J) -> F(J) end, User, Generators),
    Payload = [cb_context:set_req_data(Context, NewReqData)],
    Context1 = crossbar_bindings:fold(<<"*.validate.users">>, Payload),
    case cb_context:response(Context1) of
        {'ok', _} -> {[{<<"users">>, cb_context:store(Context1, 'iteration', Iteration)}|Pass], Fail};
        {'error', {_, _, Errors}} -> {Pass, kz_json:set_value(<<"users">>, Errors, Fail)}
    end.

%%------------------------------------------------------------------------------
%% @doc This function will use the bindings to validate and create a context
%% record to generate a device.  Any failure will be added to the error
%% json object.
%% @end
%%------------------------------------------------------------------------------
-spec create_device(kz_json:object(), pos_integer(), cb_context:context(), {kz_term:proplist(), kz_json:object()})
                   -> {kz_term:proplist(), kz_json:object()}.
create_device(JObj, Iteration, Context, {Pass, Fail}) ->
    Device = kz_json:get_value(<<"device">>, JObj, kz_json:new()),
    Generators = [fun(J) -> kz_doc:set_id(J, kz_datamgr:get_uuid()) end
                 ,fun(J) ->
                          User = get_context_jobj(<<"users">>, Pass),
                          case kz_doc:id(User) of
                              'undefined' -> J;
                              OwnerId -> kz_json:set_value(<<"owner_id">>, OwnerId, J)
                          end
                  end
                 ,fun(J) ->
                          case kz_json:get_ne_value(<<"name">>, J) of
                              'undefined' ->
                                  User = get_context_jobj(<<"users">>, Pass),
                                  FirstName = kz_json:get_value(<<"first_name">>, User, <<"User">>),
                                  LastName = kz_json:get_value(<<"last_name">>, User, kz_term:to_binary(Iteration)),
                                  Name = list_to_binary([FirstName, " ", LastName, "'s Device"]),
                                  kz_json:set_value(<<"name">>, Name, J);
                              _ ->
                                  J
                          end
                  end
                 ,fun(J) ->
                          case kzd_devices:sip_username(J) of
                              'undefined' ->
                                  Strength = kapps_config:get_integer(?OB_CONFIG_CAT, <<"device_username_strength">>, 3),
                                  kzd_devices:set_sip_username(J, list_to_binary(["user_", kz_binary:rand_hex(Strength)]));
                              _ ->
                                  J
                          end
                  end
                 ,fun(J) ->
                          case kzd_devices:sip_password(J) of
                              'undefined' ->
                                  Strength = kapps_config:get_integer(?OB_CONFIG_CAT, <<"device_pwd_strength">>, 6),
                                  kzd_devices:set_sip_password(J, kz_binary:rand_hex(Strength));
                              _ ->
                                  J
                          end
                  end
                 ],
    NewReqData = lists:foldr(fun(F, J) -> F(J) end, Device, Generators),
    Payload = [cb_context:set_req_data(Context, NewReqData)],
    Context1 = crossbar_bindings:fold(<<"*.validate.devices">>, Payload),
    case cb_context:response(Context1) of
        {'ok', _} -> {[{<<"devices">>, cb_context:store(Context1, 'iteration', Iteration)}|Pass], Fail};
        {'error', {_, _, Errors}} -> {Pass, kz_json:set_value(<<"devices">>, Errors, Fail)}
    end.

%%------------------------------------------------------------------------------
%% @doc This function will use the bindings to validate and create a context
%% record to generate a vmbox.  Any failure will be added to the error
%% json object.
%% @end
%%------------------------------------------------------------------------------
-spec create_vmbox(kz_json:object(), pos_integer(), cb_context:context(), {kz_term:proplist(), kz_json:object()})
                  -> {kz_term:proplist(), kz_json:object()}.
create_vmbox(JObj, Iteration, Context, {Pass, Fail}) ->
    VMBox = kz_json:get_value(<<"vmbox">>, JObj, kz_json:new()),
    Generators = [fun(J) -> kz_doc:set_id(J, kz_datamgr:get_uuid()) end
                 ,fun(J) ->
                          User = get_context_jobj(<<"users">>, Pass),
                          case kz_doc:id(User) of
                              'undefined' -> J;
                              OwnerId -> kz_json:set_value(<<"owner_id">>, OwnerId, J)
                          end
                  end
                 ,fun(J) ->
                          case kz_json:get_ne_value(<<"mailbox">>, J) of
                              'undefined' ->
                                  StartExten = kapps_config:get_integer(?OB_CONFIG_CAT, <<"default_vm_start_exten">>, 3 * ?MILLISECONDS_IN_SECOND),
                                  kz_json:set_value(<<"mailbox">>, kz_term:to_binary(StartExten + Iteration), J);
                              _ ->
                                  J
                          end
                  end
                 ,fun(J) ->
                          case kz_json:get_ne_value(<<"name">>, J) of
                              'undefined' ->
                                  User = get_context_jobj(<<"users">>, Pass),
                                  FirstName = kz_json:get_value(<<"first_name">>, User, <<"User">>),
                                  LastName = kz_json:get_value(<<"last_name">>, User, kz_term:to_binary(Iteration)),
                                  Name = list_to_binary([FirstName, " ", LastName, "'s Voicemail"]),
                                  kz_json:set_value(<<"name">>, Name, J);
                              _ ->
                                  J
                          end
                  end
                 ],
    NewReqData = lists:foldr(fun(F, J) -> F(J) end, VMBox, Generators),
    Payload = [cb_context:set_req_data(Context, NewReqData)],
    Context1 = crossbar_bindings:fold(<<"*.validate.vmboxes">>, Payload),
    case cb_context:response(Context1) of
        {'ok', _} -> {[{<<"vmboxes">>, cb_context:store(Context1, 'iteration', Iteration)}|Pass], Fail};
        {'error', {_, _, Errors}} -> {Pass, kz_json:set_value(<<"vmboxes">>, Errors, Fail)}
    end.

%%------------------------------------------------------------------------------
%% @doc This function will use the bindings to validate and create a context
%% record to generate a extension callflow.  Any failure will be added
%% to the error json object.
%% @end
%%------------------------------------------------------------------------------
-spec create_exten_callflow(kz_json:object(), pos_integer(), cb_context:context(), {kz_term:proplist(), kz_json:object()})
                           -> {kz_term:proplist(), kz_json:object()}.
create_exten_callflow(JObj, Iteration, Context, {Pass, Fail}) ->
    Callflow = kz_json:get_value(<<"callflow">>, JObj, kz_json:new()),
    Generators = [fun(J) ->
                          User = get_context_jobj(<<"users">>, Pass),
                          VMBox = get_context_jobj(<<"vmboxes">>, Pass),
                          DefaultFlow = kapps_config:get_string(?OB_CONFIG_CAT, <<"default_extension_callflow">>, ?DEFAULT_FLOW),
                          Flow = kz_json:decode(io_lib:format(DefaultFlow, [kz_doc:id(User)
                                                                           ,kz_doc:id(VMBox)
                                                                           ])),
                          kz_json:set_value(<<"flow">>, Flow, J)
                  end
                 ,fun(J) -> kz_doc:set_id(J, kz_datamgr:get_uuid()) end
                 ,fun(J) ->
                          case [Num || Num <- kz_json:get_ne_value(<<"numbers">>, J, [])
                                           , not kz_term:is_empty(Num)]
                          of
                              [] ->
                                  StartExten = kapps_config:get_integer(?OB_CONFIG_CAT
                                                                       ,<<"default_callflow_start_exten">>
                                                                       ,2 * ?MILLISECONDS_IN_SECOND),
                                  kz_json:set_value(<<"numbers">>, [kz_term:to_binary(StartExten + Iteration)], J);
                              Numbers -> kz_json:set_value(<<"numbers">>, Numbers, J)
                          end
                  end
                 ],
    NewReqData = lists:foldr(fun(F, J) -> F(J) end, Callflow, Generators),
    Payload = [cb_context:set_req_data(Context, NewReqData)],
    Context1 = crossbar_bindings:fold(<<"*.validate.callflows">>, Payload),
    case cb_context:response(Context1) of
        {'ok', _} -> {[{<<"callflows">>, cb_context:store(Context1, 'iteration', Iteration)}|Pass], Fail};
        {'error', {_, _, Errors}} -> {Pass, kz_json:set_value(<<"callflows">>, Errors, Fail)}
    end.

%%------------------------------------------------------------------------------
%% @doc This function will loop over the previously generated context records
%% providing each to the respective 'put' binding in order to create
%% the objects.  Starts with the account :)
%% @end
%%------------------------------------------------------------------------------

-spec populate_new_account(kz_term:proplist(), cb_context:context()) -> cb_context:context().
populate_new_account(Props, _) ->
    Context = props:get_value(?KZ_ACCOUNTS_DB, Props),
    Context1 = crossbar_bindings:fold(<<"*.execute.put.accounts">>, [cb_context:set_resp_status(Context, 'error')]),
    AccountDb = cb_context:account_db(Context1),
    AccountId = cb_context:account_id(Context1),
    case cb_context:response(Context1) of
        {'error', _} ->
            cb_context:setters(Context1, [{fun cb_context:set_account_id/2, 'undefined'}
                                         ,{fun cb_context:set_account_db/2, 'undefined'}
                                         ]);
        {'ok', _} ->
            Results = populate_new_account(prepare_props(Props), AccountDb, kz_json:new()),
            case kz_json:get_ne_value(<<"errors">>, Results) of
                'undefined' ->
                    lager:debug("new account created ~s (~s)", [AccountId, AccountDb]),
                    notfy_new_account(cb_context:doc(Context1)),
                    cb_context:set_doc(Context1, kz_json:set_value(<<"account_id">>, AccountId, Results));
                Failures ->
                    lager:debug("account creation errors: ~p", [Failures]),
                    catch (crossbar_bindings:fold(<<"*.execute.delete.accounts">>, [Context1, AccountId])),
                    Ctx2 = cb_context:setters(Context1, [{fun cb_context:set_doc/2, kz_json:delete_key(<<"owner_id">>, Results)}
                                                        ,{fun cb_context:set_account_id/2, 'undefined'}
                                                        ,{fun cb_context:set_account_db/2, 'undefined'}
                                                        ,{fun cb_context:set_resp_data/2, kz_json:new()}
                                                        ]),
                    crossbar_util:response_invalid_data(Failures, Ctx2)
            end
    end.

-spec populate_new_account(kz_term:proplist(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
populate_new_account([], _, Results) ->
    Results;

populate_new_account([{<<"phone_numbers">>, Context}|Props], AccountDb, Results) ->
    Number = cb_context:fetch(Context, 'number'),
    AccountId = kz_util:format_account_id(AccountDb, 'raw'),
    Payload = [cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'error'}
                                           ,{fun cb_context:set_account_db/2, AccountDb}
                                           ,{fun cb_context:set_auth_account_id/2, AccountId}
                                           ,{fun cb_context:set_account_id/2, AccountId}
                                           ])
              ,Number
              ,<<"activate">>
              ],
    Context1 = crossbar_bindings:fold(<<"*.execute.put.phone_numbers">>, Payload),
    case cb_context:response(Context1) of
        {'ok', _} -> populate_new_account(Props, AccountDb, Results);
        {'error', {_, _, Errors}} ->
            populate_new_account(Props, AccountDb
                                ,kz_json:set_value([<<"errors">>, <<"phone_numbers">>, Number], Errors, Results))
    end;

populate_new_account([{<<"braintree">>, Context}|Props], AccountDb, Results) ->
    AccountId = kz_util:format_account_id(AccountDb, 'raw'),
    Payload = [cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'error'}
                                           ,{fun cb_context:set_account_db/2, AccountDb}
                                           ,{fun cb_context:set_account_id/2, AccountId}
                                           ,{fun cb_context:set_req_verb/2, ?HTTP_POST}
                                           ])
              ,<<"customer">>
              ],
    Context1 = crossbar_bindings:fold(<<"*.execute.post.braintree">>, Payload),
    case cb_context:response(Context1) of
        {'ok', _} -> populate_new_account(Props, AccountDb, Results);
        {'error', {_, _, Errors}} ->
            populate_new_account(Props, AccountDb
                                ,kz_json:set_value([<<"errors">>, <<"braintree">>], Errors, Results))
    end;

populate_new_account([{Event, Context}|Props], AccountDb, Results) ->
    Iteration = cb_context:fetch(Context, 'iteration'),
    Payload = [cb_context:setters(Context, [{fun cb_context:set_account_db/2, AccountDb}
                                           ,{fun cb_context:set_resp_status/2, 'error'}
                                           ])],
    Context1 = crossbar_bindings:fold(<<"*.execute.put.", Event/binary>>, Payload),
    JObj = cb_context:doc(Context1),
    case cb_context:response(Context1) of
        {'ok', _} ->
            case kzd_users:is_account_admin(JObj) of
                'true' ->
                    populate_new_account(Props, AccountDb
                                        ,kz_json:set_value(<<"owner_id">>, kz_doc:id(JObj), Results));
                'false' ->
                    populate_new_account(Props, AccountDb, Results)
            end;
        {'error', {_, _, Errors}} ->
            populate_new_account(Props, AccountDb
                                ,kz_json:set_value([<<"errors">>, Event, kz_term:to_binary(Iteration)], Errors, Results))
    end.

prepare_props(Props) ->
    lists:sort(fun({<<"braintree">>, _}, {_, _}) -> 'true';
                  (_, _) -> 'false'
               end, props:delete(?KZ_ACCOUNTS_DB, Props)).

%%------------------------------------------------------------------------------
%% @doc Helper function to get the create object out of the successful
%% context records for a specific key.
%% @end
%%------------------------------------------------------------------------------
-spec get_context_jobj(kz_term:ne_binary(), kz_term:proplist()) -> kz_json:object().
get_context_jobj(Key, Pass) ->
    case props:get_value(Key, Pass) of
        Context when is_tuple(Context) -> cb_context:doc(Context);
        _ -> kz_json:new()
    end.

%%------------------------------------------------------------------------------
%% @doc Attempt to create a token and save it to the token db
%% @end
%%------------------------------------------------------------------------------
-spec create_response(cb_context:context()) -> cb_context:context().
create_response(Context) ->
    JObj = cb_context:doc(Context),
    case cb_context:account_id(Context) of
        'undefined' ->
            crossbar_util:response_invalid_data(JObj, Context);
        AccountId ->
            TS = kz_time:now_s(),
            Token = [{<<"account_id">>, AccountId}
                    ,{<<"owner_id">>, kz_json:get_value(<<"owner_id">>, JObj)}
                    ,{<<"created">>, TS}
                    ,{<<"modified">>, TS}
                    ,{<<"method">>, kz_term:to_binary(?MODULE)}
                    ],
            case kz_datamgr:save_doc(?KZ_TOKEN_DB, kz_json:from_list(Token)) of
                {'ok', Doc} ->
                    AuthToken = kz_doc:id(Doc),
                    lager:debug("created new local auth token ~s", [AuthToken]),
                    Context1 = cb_context:setters(Context, [{fun cb_context:set_auth_token/2, AuthToken}
                                                           ,{fun cb_context:set_auth_doc/2, Doc}
                                                           ]),
                    crossbar_util:response(kz_json:set_value(<<"auth_token">>, AuthToken, JObj), Context1);
                {'error', R} ->
                    lager:debug("could not create new local auth token, ~p", [R]),
                    crossbar_util:response('error', 'undefined', 400, JObj, Context)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Attempt to create a token and save it to the token db
%% @end
%%------------------------------------------------------------------------------
-spec notfy_new_account(kz_json:object()) -> 'ok'.
notfy_new_account(JObj) ->
    Notify = [{<<"Account-Name">>, kzd_accounts:name(JObj)}
             ,{<<"Account-Realm">>, kzd_accounts:realm(JObj)}
             ,{<<"Account-API-Key">>, kzd_accounts:api_key(JObj)}
             ,{<<"Account-ID">>, kz_doc:account_id(JObj)}
             ,{<<"Account-DB">>, kz_doc:account_db(JObj)}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kapps_notify_publisher:cast(Notify, fun kapi_notifications:publish_new_account/1).

-spec generate_username(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()) ->
                               kz_term:ne_binary().
generate_username('undefined', 'undefined', _) ->
    kz_binary:rand_hex(3);
generate_username('undefined', _, 'undefined') ->
    kz_binary:rand_hex(3);
generate_username('undefined', <<FirstLetter:1/binary, _/binary>>, LastName) ->
    <<FirstLetter/binary, (kz_term:to_binary(LastName))/binary>>;
generate_username(Email, _, _) ->
    Email.
