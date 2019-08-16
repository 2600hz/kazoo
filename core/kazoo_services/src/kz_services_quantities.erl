%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_quantities).

-export([fetch_account/1
        ,fetch_cascade/1
        ]).
-export([substitute_values/1]).
-export([category/2
        ,category/3
        ]).
-export([item/3]).
-export([cascade_category/2
        ,cascade_category/3
        ]).
-export([cascade_item/3]).
-export([calculate_updates/3]).

-include("services.hrl").

-type billable() :: kz_term:api_object().
-type billables() :: kz_json:objects().
-type quantity_kv() :: {kz_term:ne_binaries(), integer()}.
-type quantities_prop() :: [quantity_kv()].
-export_type([quantities_prop/0
             ,billables/0
             ,billable/0
             ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_account(kz_services:services()) -> kz_json:object().
fetch_account(Services) ->
    AccountId = kz_services:account_id(Services),
    lager:debug("fetching account quantities for ~s", [AccountId]),
    AccountDb = kz_util:format_account_db(AccountId),
    ViewOptions = ['reduce'
                  ,'group'
                  ],
    AccountQuantities = fetch(AccountDb, <<"services/quantify">>, ViewOptions),
    PortQuantities = fetch_account_port(AccountId),
    kz_json:merge([AccountQuantities, PortQuantities]).

-spec fetch_cascade(kz_services:services()) -> kz_json:object().
fetch_cascade(Services) ->
    AccountId = kz_services:account_id(Services),
    lager:debug("fetching cascade quantities for ~s", [AccountId]),
    AccountQuantities = fetch_account_cascade(AccountId),
    PortQuantities = fetch_account_port_cascade(AccountId),
    kz_json:merge([AccountQuantities, PortQuantities]).

-spec fetch_account_port(kz_term:ne_binary()) -> kz_json:object().
fetch_account_port(AccountId) ->
    fetch_port(AccountId, <<"port_services/account_quantities">>).

-spec fetch_account_port_cascade(kz_term:ne_binary()) -> kz_json:object().
fetch_account_port_cascade(AccountId) ->
    fetch_port(AccountId, <<"port_services/cascade_quantities">>).

-spec fetch_port(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
fetch_port(AccountId, View) ->
    ViewOptions = ['reduce'
                  ,'group'
                  ,{'group_level', 2}
                  ,{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(?KZ_PORT_REQUESTS_DB, View, ViewOptions) of
        {'ok', JObjs} ->
            Quantities = [{port_key(kz_json:get_value(<<"key">>, JObj))
                          ,kz_json:get_integer_value(<<"value">>, JObj, 0)
                          }
                          || JObj <- JObjs
                         ],
            kz_json:set_values(Quantities, kz_json:new());
        {'error', _Message} ->
            ?SUP_LOG_ERROR("failed to query port quantities account: ~s with error: ~p", [AccountId, _Message]),
            kz_json:new()
    end.


-spec port_key(kz_term:ne_binaries()) -> kz_term:ne_binaries().
port_key([_AccountId, State]) ->
    [<<"port_request">>, State].

-spec fetch_account_cascade(kz_term:ne_binary()) -> kz_json:object().
fetch_account_cascade(AccountId) ->
    ViewOptions = ['reduce'
                  ,'group'
                  ,{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    fetch(?KZ_SERVICES_DB, <<"services/cascade_quantities">>, ViewOptions).

-spec fetch(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> kz_json:object().
fetch(Database, View, ViewOptions) ->
    case kz_datamgr:get_results(Database, View, ViewOptions) of
        {'ok', JObjs} ->
            Quantities = [{kz_json:get_value(<<"key">>, JObj)
                          ,kz_json:get_integer_value(<<"value">>, JObj, 0)
                          }
                          || JObj <- JObjs
                         ],
            fetch_to_json(Quantities);
        {'error', _Message} ->
            ?SUP_LOG_ERROR("failed to query account quantities: ~s with error: ~p", [Database, _Message]),
            kz_json:new()
    end.

-spec fetch_to_json(quantities_prop()) -> kz_json:object().
fetch_to_json(Quantities) ->
    SubstituteValues = substitute_values(Quantities),
    lists:foldl(fun(Quantity, JObj) ->
                        fetch_to_json_fold(Quantity, SubstituteValues, JObj)
                end, kz_json:new(), Quantities).

-spec fetch_to_json_fold(quantity_kv(), kz_term:proplist(), kz_json:object()) -> kz_json:object().
fetch_to_json_fold({[_, Category, Item], Value}, SubstituteValues, JObj) ->
    %% NOTE: fetch results from the services db for cascade/reseller quantities
    %%  have the account id as the first key that we need to remove
    fetch_to_json_fold({[Category, Item], Value}, SubstituteValues, JObj);
fetch_to_json_fold({Key, Value}, SubstituteValues, JObj) when Value < 0 ->
    %% NOTE: negative quantities represent a substitute value
    %%   (for example, -2 should be replaced with the quantity of admin users)
    %% NOTE: do NOT use props:get_value here!
    case kz_term:to_integer(proplists:get_value(Value, SubstituteValues, 0)) of
        0 -> JObj;
        Quantity -> kz_json:set_value(Key, Quantity, JObj)
    end;
fetch_to_json_fold({_, 0}, _, JObj) ->
    %% NOTE: ignore nothing
    JObj;
fetch_to_json_fold({Key, Value}, _, JObj) ->
    kz_json:set_value(Key, Value, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec substitute_values(kz_services:services() | quantities_prop()) -> kz_term:proplist().
substitute_values(Props) when is_list(Props) ->
    %% NOTE: do NOT use props:get_value here!
    UsersAdminQuantity = kz_term:to_integer(
                           proplists:get_value([<<"users">>, <<"admin">>],  Props, 0)
                          ),
    UsersUserQuantity = kz_term:to_integer(
                          proplists:get_value([<<"users">>, <<"user">>], Props, 0)
                         ),
    [{-2, UsersAdminQuantity}
    ,{-1, UsersUserQuantity + UsersAdminQuantity}
    ];
substitute_values(Services) ->
    [{-2, item(Services, <<"users">>, <<"admin">>)}
    ,{-1, category(Services, <<"users">>)}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec category(kz_services:services(), kz_term:ne_binary()) -> non_neg_integer().
category(Services, CategoryName) ->
    category(Services, CategoryName, []).

-spec category(kz_services:services(), kz_term:ne_binary(), kz_term:ne_binaries()) -> non_neg_integer().
category(Services, CategoryName, ItemExceptions) ->
    maybe_log("account category ~s has a quantity of ~p"
             ,[CategoryName]
             ,logless_category(Services, CategoryName, ItemExceptions)
             ).

-spec logless_category(kz_services:services(), kz_term:ne_binary(), kz_term:ne_binaries()) -> non_neg_integer().
logless_category(Services, CategoryName, ItemExceptions) ->
    case kz_services:is_deleted(Services) of
        'true' -> 0;
        'false' ->
            AccountQuantities = kz_services:account_quantities(Services),
            ManualQuantities = kz_services:manual_quantities(Services),
            CategoryQuantities =
                kz_json:merge_recursive(
                  [kz_json:get_value(CategoryName, AccountQuantities, kz_json:new())
                  ,kz_json:get_value(CategoryName, ManualQuantities, kz_json:new())
                  ]
                 ),
            sum_values(kz_json:delete_keys(ItemExceptions, CategoryQuantities))
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec item(kz_services:services(), kz_term:ne_binary(), kz_term:ne_binary()) -> non_neg_integer().
item(Services, CategoryName, ItemName) ->
    maybe_log("account item ~s/~s has a quantity of ~p"
             ,[CategoryName, ItemName]
             ,logless_item(Services, CategoryName, ItemName)
             ).

-spec logless_item(kz_services:services(), kz_term:ne_binary(), kz_term:ne_binary()) -> non_neg_integer().
logless_item(Services, CategoryName, ItemName) ->
    case kz_services:is_deleted(Services) of
        'true' -> 0;
        'false' ->
            AccountQuantities = kz_services:account_quantities(Services),
            ManualQuantities = kz_services:manual_quantities(Services),
            case kz_json:get_integer_value([CategoryName, ItemName], ManualQuantities) of
                'undefined' ->
                    kz_json:get_integer_value([CategoryName, ItemName], AccountQuantities, 0);
                ManualQuantity -> ManualQuantity
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cascade_category(kz_services:services(), kz_term:ne_binary()) -> non_neg_integer().
cascade_category(Services, CategoryName) ->
    cascade_category(Services, CategoryName, []).

-spec cascade_category(kz_services:services(), kz_term:ne_binary(), kz_term:ne_binaries()) -> non_neg_integer().
cascade_category(Services, CategoryName, ItemExceptions) ->
    case kz_services:is_deleted(Services) of
        'true' -> 0;
        'false' ->
            CascadeQuantities = kz_services:cascade_quantities(Services),
            CategoryJObj = kz_json:get_value(CategoryName, CascadeQuantities, kz_json:new()),
            maybe_log("cascade category ~s has a quantity of ~p"
                     ,[CategoryName]
                     ,sum_values(kz_json:delete_keys(ItemExceptions, CategoryJObj))
                      + logless_category(Services, CategoryName, ItemExceptions)
                     )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cascade_item(kz_services:services(), kz_term:ne_binary(), kz_term:ne_binary()) -> non_neg_integer().
cascade_item(Services, CategoryName, ItemName) ->
    case kz_services:is_deleted(Services) of
        'true' -> 0;
        'false' ->
            CascadeQuantities = kz_services:cascade_quantities(Services),
            maybe_log("cascade item ~s/~s has a quantity of ~p"
                     ,[CategoryName, ItemName]
                     ,kz_json:get_integer_value([CategoryName, ItemName], CascadeQuantities, 0)
                      + logless_item(Services, CategoryName, ItemName)
                     )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec calculate_updates(kz_services:services(), billable() | billables(), billable() | billables()) ->
                               kz_json:object().
calculate_updates(Services, 'undefined', ProposedJObjs) ->
    lager:debug("calculating service updates for addition(s)", []),
    calculate_updates(Services, [], ProposedJObjs);
calculate_updates(Services, CurrentJObjs, 'undefined') ->
    lager:debug("calculating service updates for removal(s)", []),
    calculate_updates(Services, CurrentJObjs, []);

calculate_updates(Services, CurrentJObj, ProposedJObjs)
  when not is_list(CurrentJObj) ->
    calculate_updates(Services, [CurrentJObj], ProposedJObjs);
calculate_updates(Services, CurrentJObjs, ProposedJObj)
  when not is_list(ProposedJObj) ->
    calculate_updates(Services, CurrentJObjs, [ProposedJObj]);

calculate_updates(Services, CurrentJObjs, ProposedJObjs) ->
    Props = calculate_updates(Services, CurrentJObjs, ProposedJObjs, []),
    lager:debug("calculated ~p possible services change", [length(Props)]),
    kz_json:set_values(Props, kz_json:new()).

-spec calculate_updates(kz_services:services(), billables(), billables(), kz_term:proplist()) -> kz_term:proplist().
calculate_updates(_Services, [], [], Updates) -> Updates;
calculate_updates(Services, [CurrentJObj|RemainingCurrentJObjs], [], Updates) ->
    CurrentUpdates = calculate_updates(Services, CurrentJObj),
    Props = compare_service_updates(CurrentUpdates, []),
    calculate_updates(Services
                     ,RemainingCurrentJObjs
                     ,[]
                     ,sum_updates(Props, Updates)
                     );
calculate_updates(Services, CurrentJObjs, [ProposedJObj|RemainingProposedJObjs], Updates) ->
    Id = kz_doc:id(ProposedJObj),
    {CurrentJObj, RemainingCurrentJObjs} = split_jobjs(CurrentJObjs, Id),
    CurrentUpdates = calculate_updates(Services, CurrentJObj),
    ProposedUpdates = calculate_updates(Services, ProposedJObj),
    Props = compare_service_updates(CurrentUpdates, ProposedUpdates),
    calculate_updates(Services
                     ,RemainingCurrentJObjs
                     ,RemainingProposedJObjs
                     ,sum_updates(Props, Updates)
                     ).

-spec split_jobjs(kz_json:objects(), kz_term:ne_binary()) -> {kz_term:api_object(), kz_json:objects()}.
split_jobjs(JObjs, Id) ->
    case lists:splitwith(fun(JObj) ->
                                 kz_doc:id(JObj) =:= Id
                         end, JObjs)
    of
        {[], RemainingJObjs} ->
            {'undefined', RemainingJObjs};
        {[CurrentJObj], RemainingJObjs} ->
            {CurrentJObj, RemainingJObjs}
    end.

-spec calculate_updates(kz_services:services(), kz_term:api_object()) -> kz_term:proplist().
calculate_updates(_Services, 'undefined') -> [];
calculate_updates(Services, JObj) ->
    case kz_json:is_false(<<"enabled">>, JObj)
        orelse kz_json:is_true(<<"pvt_deleted">>, JObj, 'false')
    of
        'true' -> [];
        'false' ->
            Routines = [fun calculate_user_updates/2
                       ,fun calculate_device_updates/2
                       ,fun calculate_limits_updates/2
                       ,fun calculate_whitelabel_updates/2
                       ,fun calculate_dedicated_ip_updates/2
                       ,fun calculate_app_store_updates/2
                       ,fun calculate_number_updates/2
                       ,fun calculate_billing_updates/2
                       ,fun calculate_qubicle_queue_updates/2
                       ,fun calculate_qubicle_recipient_updates/2
                       ,fun calculate_vmbox_updates/2
                       ,fun calculate_faxbox_updates/2
                       ,fun calculate_conference_updates/2
                       ,fun calculate_port_request_updates/2
                       ],
            Updates = lists:foldl(fun(F, Updates) ->
                                          F(JObj, Updates)
                                  end, [], Routines),
            case has_substitute_values(Updates) of
                'false' -> Updates;
                'true' -> substitute_values(Services, Updates)
            end
    end.

-spec sum_updates(kz_term:proplist(), kz_term:proplist()) -> kz_term:proplist().
sum_updates([], Updates) -> Updates;
sum_updates([{Key, Value}|Props], Updates) ->
    %% NOTE: do NOT use props:get_value here!
    case proplists:get_value(Key, Updates) of
        'undefined' ->
            sum_updates(Props, [{Key, Value} | Updates]);
        CurrentValue ->
            %% NOTE: do NOT use props:set_value here!
            Sum = Value + CurrentValue,
            sum_updates(Props, [{Key, Sum}|Updates])
    end.

-spec has_substitute_values(kz_term:proplist()) -> boolean().
has_substitute_values([]) -> 'false';
has_substitute_values([{_, Value}|_]) when Value < 0 -> 'true';
has_substitute_values([_|Updates]) -> has_substitute_values(Updates).

-spec substitute_values(kz_services:services(), kz_term:proplist()) -> kz_term:proplist().
substitute_values(Services, Updates) ->
    SubstituteValues = substitute_values(Services),
    lists:map(fun({Key, Value}=KV) ->
                      %% NOTE: do NOT use props:get_value here!
                      case proplists:get_value(Value, SubstituteValues) of
                          'undefined' -> KV;
                          Substitution -> {Key, Substitution}
                      end
              end
             ,Updates
             ).

-spec calculate_user_updates(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
calculate_user_updates(JObj, Updates) ->
    case kz_doc:type(JObj) =:= <<"user">> of
        'false' -> Updates;
        'true' ->
            PrivLevel = kzd_users:priv_level(JObj, <<"user">>),
            Key = [<<"users">>, PrivLevel],
            [{Key, 1} | Updates]
    end.

-spec calculate_device_updates(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
calculate_device_updates(JObj, Updates) ->
    case kz_doc:type(JObj) =:= <<"device">> of
        'false' -> Updates;
        'true' ->
            Type = kzd_devices:device_type(JObj, <<"sip_device">>),
            Key = [<<"devices">>, Type],
            [{Key, 1} | Updates]
    end.

-spec calculate_limits_updates(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
calculate_limits_updates(JObj, Updates) ->
    case kz_doc:type(JObj) =:= <<"limits">> of
        'false' -> Updates;
        'true' ->
            Keys = [<<"twoway_trunks">>
                   ,<<"inbound_trunks">>
                   ,<<"outbound_trunks">>
                   ],
            Fun = calculate_limits_updates_foldl(JObj),
            lists:foldl(Fun, Updates, Keys)
    end.
-type limits_fold_fun() :: fun((kz_term:ne_binary(), kz_term:proplist()) -> kz_term:proplist()).
-spec calculate_limits_updates_foldl(kz_json:object()) -> limits_fold_fun().
calculate_limits_updates_foldl(JObj) ->
    fun(K, U) ->
            case kz_json:get_integer_value(K, JObj, 0) of
                0 -> U;
                Value ->
                    Key = [<<"limits">>, K],
                    [{Key, Value} | U]
            end
    end.

-spec calculate_whitelabel_updates(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
calculate_whitelabel_updates(JObj, Updates) ->
    case kz_doc:type(JObj) =:= <<"whitelabel">> of
        'false' -> Updates;
        'true' ->
            Key = [<<"branding">>, <<"whitelabel">>],
            [{Key, 1} | Updates]
    end.

-spec calculate_dedicated_ip_updates(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
calculate_dedicated_ip_updates(JObj, Updates) ->
    case kz_doc:type(JObj) =:= <<"dedicated_ip">> of
        'false' -> Updates;
        'true' ->
            Key = [<<"ips">>, <<"dedicated">>],
            [{Key, 1} | Updates]
    end.

-spec calculate_app_store_updates(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
calculate_app_store_updates(JObj, Updates) ->
    case kz_doc:id(JObj) =:= <<"apps_store">> of
        'false' -> Updates;
        'true' ->
            Blacklist = kz_json:get_list_value(<<"blacklist">>, JObj, []),
            Apps = kz_json:get_json_value(<<"apps">>, JObj, kz_json:new()),
            kz_json:foldl(fun(_K, App, U) ->
                                  Name = kz_json:get_ne_binary_value(<<"name">>, App),
                                  AccountKey = [<<"account_apps">>, Name],
                                  case kz_json:get_ne_value(<<"allowed_users">>, App) of
                                      <<"all">> ->
                                          UserKey = [<<"user_apps">>, Name],
                                          [{UserKey, -1}
                                          ,{AccountKey, 1}
                                           | U
                                          ];
                                      <<"admins">> ->
                                          UserKey = [<<"user_apps">>, Name],
                                          [{UserKey, -2}
                                          ,{AccountKey, 1}
                                           | U
                                          ];
                                      <<"specific">> ->
                                          UserKey = [<<"user_apps">>, Name],
                                          Value = length(kz_json:get_list_value(<<"users">>, App, [])),
                                          [{UserKey, Value}
                                          ,{AccountKey, 1}
                                           | U
                                          ]
                                  end
                          end
                         ,Updates
                         ,kz_json:delete_keys(Blacklist, Apps)
                         )
    end.

-spec calculate_number_updates(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
calculate_number_updates(JObj, Updates) ->
    case kz_doc:type(JObj) =:= <<"number">> of
        'false' -> Updates;
        'true' ->
            Routines = [fun calculate_phone_numbers_updates/2
                       ,fun calculate_number_carriers_updates/2
                       ,fun calculate_number_services_updates/2
                       ],
            lists:foldl(fun(F, U) -> F(JObj, U) end
                       ,Updates
                       ,Routines
                       )
    end.

-spec calculate_phone_numbers_updates(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
calculate_phone_numbers_updates(JObj, Updates) ->
    Number = kz_doc:id(JObj),
    Classification = knm_converters:classify(Number),
    Key = [<<"phone_numbers">>, Classification],
    case kz_json:get_value([<<"pvt_features">>, <<"local">>], JObj) =:= 'undefined' of
        'false' -> Updates;
        'true' -> [{Key, 1} | Updates]
    end.

-spec calculate_number_carriers_updates(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
calculate_number_carriers_updates(JObj, Updates) ->
    Module = kz_json:get_ne_binary_value(<<"pvt_module_name">>, JObj),
    Key = [<<"number_carriers">>, Module],
    [{Key, 1} | Updates].

-spec calculate_number_services_updates(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
calculate_number_services_updates(JObj, Updates) ->
    lists:foldl(fun(Feature, U) ->
                        Key = [<<"number_services">>, Feature],
                        [{Key, 1} | U]
                end
               ,Updates
               ,kz_json:get_keys(<<"pvt_features">>, JObj)
               ).

-spec calculate_billing_updates(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
calculate_billing_updates(JObj, Updates) ->
    Billings = kz_json:get_json_value(<<"billing">>, JObj, kz_json:new()),
    kz_json:foldl(fun(Name, Value, U) ->
                          Key = [<<"billing">>, Name],
                          [{Key, Value} | U]
                  end
                 ,Updates
                 ,Billings
                 ).

-spec calculate_qubicle_queue_updates(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
calculate_qubicle_queue_updates(JObj, Updates) ->
    case kz_doc:type(JObj) =:= <<"qubicle_queue">> of
        'false' -> Updates;
        'true' ->
            Key = [<<"qubicle">>, <<"queues">>],
            [{Key, 1} | Updates]
    end.

-spec calculate_qubicle_recipient_updates(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
calculate_qubicle_recipient_updates(JObj, Updates) ->
    case kz_doc:type(JObj) =:= <<"user">>
        andalso kz_json:is_true([<<"qubicle">>, <<"enabled">>], JObj, 'false')
    of
        'false' -> Updates;
        'true' ->
            Key = [<<"qubicle">>, <<"recipients">>],
            [{Key, 1} | Updates]
    end.

-spec calculate_vmbox_updates(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
calculate_vmbox_updates(JObj, Updates) ->
    case kz_doc:type(JObj) =:= <<"vmbox">> of
        'false' -> Updates;
        'true' ->
            Key = [<<"voicemails">>, <<"mailbox">>],
            [{Key, 1} | Updates]
    end.

-spec calculate_faxbox_updates(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
calculate_faxbox_updates(JObj, Updates) ->
    case kz_doc:type(JObj) =:= <<"faxbox">> of
        'false' -> Updates;
        'true' ->
            Key = [<<"faxes">>, <<"mailbox">>],
            [{Key, 1} | Updates]
    end.

-spec calculate_conference_updates(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
calculate_conference_updates(JObj, Updates) ->
    case kz_doc:type(JObj) =:= <<"conference">> of
        'false' -> Updates;
        'true' ->
            Key = [<<"conferences">>, <<"conference">>],
            [{Key, 1} | Updates]
    end.

-spec calculate_port_request_updates(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
calculate_port_request_updates(JObj, Updates) ->
    case kz_doc:type(JObj) =:= <<"port_request">> of
        'false' -> Updates;
        'true' ->
            Key = get_port_request_key(JObj),
            Numbers = kzd_port_requests:numbers(JObj, kz_json:new()),
            [{Key, length(kz_json:get_keys(Numbers))} | Updates]
    end.

-spec get_port_request_key(kz_json:object()) -> kz_term:ne_binaries().
get_port_request_key(JObj) ->
    State = kzd_port_requests:pvt_port_state(JObj, <<"unconfirmed">>),
    case is_first_transition_to_state(JObj) of
        'true' -> [<<"port_request">>, <<"first_", State/binary>>];
        'false' -> [<<"port_request">>, State]
    end.

-spec is_first_transition_to_state(kz_json:object()) -> boolean().
is_first_transition_to_state(JObj) ->
    StateTransitions =
        kzd_port_requests:get_transition(JObj
                                        ,kzd_port_requests:pvt_port_state(JObj)
                                        ),
    length(StateTransitions) =:= 1.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec compare_service_updates(kz_term:proplist(), kz_term:proplist()) -> kz_term:proplist().
compare_service_updates(Current, Proposed) ->
    compare_service_updates(Current, Proposed, []).

-spec compare_service_updates(kz_term:proplist(), kz_term:proplist(), kz_term:proplist()) -> kz_term:proplist().
compare_service_updates([], [], Updates) ->
    Updates;
compare_service_updates([{K, V}|Current], [], Updates) ->
    compare_service_updates(Current, [], [{K, V * -1} | Updates]);
compare_service_updates(Current, [{K, V}=KV|Proposed], Updates) ->
    %% NOTE: do NOT use props:get_value here!
    case proplists:get_value(K, Current) of
        'undefined' ->
            compare_service_updates(Current, Proposed, [KV|Updates]);
        CurrentValue ->
            compare_serivce_update(Current, CurrentValue, K, V, Proposed, Updates)
    end.

-spec compare_serivce_update(kz_term:proplist()
                            ,non_neg_integer()
                            ,kz_term:ne_binary()
                            ,non_neg_integer()
                            ,kz_term:proplist()
                            ,kz_term:proplist()
                            ) -> kz_term:proplist().
compare_serivce_update(Current, CurrentValue, Key, ProposedValue, Proposed, Updates) ->
    %% NOTE: do NOT use props:delete here!
    case ProposedValue - CurrentValue of
        0 ->
            compare_service_updates(proplists:delete(Key, Current), Proposed, Updates);
        Value ->
            compare_service_updates(proplists:delete(Key, Current), Proposed, [{Key, Value}|Updates])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sum_values(kz_json:object()) -> non_neg_integer().
sum_values(JObj) ->
    sum_values(JObj, 0).

-spec sum_values(kz_json:object(), non_neg_integer()) -> non_neg_integer().
sum_values(JObj, InitialSum) ->
    kz_json:foldl(fun(_ItemName, ItemQuantity, Sum) ->
                          ItemQuantity + Sum
                  end, InitialSum, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_log(iolist(), list(), non_neg_integer()) -> non_neg_integer().
maybe_log(Format, Args, Quantity) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"debug_services_quantities">>, 'false') of
        'false' -> Quantity;
        'true' ->
            lager:debug(Format, Args ++ [Quantity]),
            Quantity
    end.
