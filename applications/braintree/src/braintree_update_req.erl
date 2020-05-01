%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(braintree_update_req).

-export([handle_req/2]).

-include("braintree.hrl").

-record(update, {item :: kz_term:api_object()
                ,plan_id :: kz_term:api_ne_binary()
                ,addon_id :: kz_term:api_ne_binary()
                ,mapping :: kz_term:api_object()
                ,subscription :: braintree_subscription:subscription() | 'undefined'
                }).

-record(request, {activation_charges = [] :: kz_json:objects()
                 ,request_jobj = kz_json:new() :: kz_json:object()
                 ,account_id :: kz_term:api_ne_binary()
                 ,items = [] :: kz_json:objects()
                 ,vendor_id :: kz_term:api_ne_binary()
                 ,bookkeeper_id :: kz_term:api_ne_binary()
                 ,bookkeeper_jobj :: kz_term:api_object()
                 ,customer :: braintree_customer:customer() | 'undefined'
                 ,updates = dict:new() :: dict:dict()
                 ,results = [] :: results()
                 }).

-type results() :: [kz_term:proplist()].
-type update() :: #update{}.
-type request() :: #request{}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_bookkeepers:update_req_v(JObj),
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    lager:debug("received service update notification for ~s", [AccountId]),
    case kz_json:get_value(<<"Bookkeeper-Type">>, JObj) =:= ?APP_NAME of
        'false' ->
            lager:debug("skipping service update for another bookkeeper");
        'true' ->
            sync(#request{activation_charges=kz_json:get_list_value([<<"Invoice">>, <<"activation_charges">>], JObj, [])
                         ,request_jobj=JObj
                         ,account_id=AccountId
                         ,items=kz_json:get_list_value([<<"Invoice">>, <<"items">>], JObj, [])
                         ,vendor_id=kz_json:get_value(<<"Vendor-ID">>, JObj)
                         ,bookkeeper_id=kz_json:get_value(<<"Bookkeeper-ID">>, JObj)
                         }
                )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reply(request()) -> 'ok'.
reply(#request{results=Results}=Request) ->
    Reply = [{<<"Details">>, Results}
             | results_reply(Results, 'false')
            ],
    reply(Request, Reply).

-spec results_reply(results(), boolean()) -> kz_term:proplist().
results_reply([], 'false') ->
    [{<<"Status">>, <<"success">>}];
results_reply([], 'true') ->
    [{<<"Status">>, <<"error">>}
    ,{<<"Message">>, <<"Customer action is required">>}
    ,{<<"Reason">>, <<"action_required">>}
    ];
results_reply([Result|Results], HasErrors) ->
    case props:get_value(<<"Status">>, Result) of
        <<"success">> -> results_reply(Results, HasErrors);
        <<"error">> -> results_reply(Results, 'true');
        <<"fatal">> ->
            [{<<"Status">>, <<"fatal">>}
            ,{<<"Message">>, <<"Unable to update bookkeeper">>}
            ,{<<"Reason">>, <<"bookkeeper_fault">>}
            ]
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reply(request(), kz_term:proplist()) -> 'ok'.
reply(#request{request_jobj=JObj}, Reply) ->
    MessageId = kz_json:get_value(<<"Msg-ID">>, JObj),
    Response = kz_json:from_list_recursive(
                 [{<<"Msg-ID">>, MessageId}
                  | Reply
                 ] ++ kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ),
    RespQ = kz_json:get_value(<<"Server-ID">>, JObj),
    kz_amqp_worker:cast(Response, fun(P) -> kapi_bookkeepers:publish_update_resp(RespQ, P) end).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sync(request()) -> 'ok'.
sync(#request{bookkeeper_jobj='undefined'
             ,items=[]
             }=Request) ->
    _ = reply(Request),
    process_activations(Request);
sync(#request{bookkeeper_jobj='undefined'
             ,vendor_id=VendorId
             ,bookkeeper_id=BookkeeperId
             }=Request) ->
    lager:debug("fetching bookkeeper document ~s/~s"
               ,[VendorId, BookkeeperId]
               ),
    VendorDb = kzs_util:format_account_db(VendorId),
    case kz_datamgr:open_cache_doc(VendorDb, BookkeeperId) of
        {'ok', BookkeeperJObj} ->
            ?APP_NAME = kzd_bookkeeper:bookkeeper_type(BookkeeperJObj),
            sync(
              Request#request{bookkeeper_jobj=BookkeeperJObj}
             );
        {'error', _Reason} ->
            lager:info("unable to load bookkeeper document ~s/~s: ~p"
                      ,[VendorId, BookkeeperId, _Reason]
                      ),
            Reply = [{<<"Status">>, <<"fatal">>}
                    ,{<<"Message">>, <<"Unable to locate bookkeeper configuration">>}
                    ,{<<"Reason">>, <<"bookkeeper_fault">>}
                    ],
            reply(Request, Reply)
    end;
sync(#request{customer='undefined'
             ,account_id=AccountId
             }=Request) ->
    lager:debug("requesting braintree customer ~s", [AccountId]),
    try braintree_customer:find(AccountId) of
        Customer ->
            sync(Request#request{customer=Customer})
    catch
        'throw':Error ->
            reply(Request, braintree_util:error_to_props(Error))
    end;
sync(#request{items=[]
             ,updates=Updates
             }=Request) ->
    Results = [update_subscription(Subscription)
               || {_, Subscription} <- dict:to_list(Updates)
              ],
    reply(Request#request{results=Results});
sync(#request{items=[Item|Items]
             ,bookkeeper_jobj=BookkeeperJObj
             ,updates=Updates
             }=Request) ->
    CategoryName = kz_json:get_value(<<"category">>, Item),
    ItemName = kz_json:get_value(<<"item">>, Item),
    Mapping = kzd_bookkeeper:mapping(BookkeeperJObj, CategoryName, ItemName),
    case {kz_json:get_ne_binary_value(<<"plan">>, Mapping)
         ,kz_json:get_ne_binary_value(<<"addon">>, Mapping)
         } of
        {'undefined', _} ->
            lager:debug("service item ~s/~s had no plan id"
                       ,[CategoryName, ItemName]
                       ),
            sync(Request#request{items=Items});
        {_, 'undefined'} ->
            lager:debug("service item ~s/~s had no add on id"
                       ,[CategoryName, ItemName]
                       ),
            sync(Request#request{items=Items});
        {PlanId, AddOnId}->
            Update = #update{item=Item
                            ,plan_id=PlanId
                            ,addon_id=AddOnId
                            ,mapping=Mapping
                            ,subscription=get_plan_subscription(PlanId, Request)
                            },
            sync(Request#request{updates=
                                     dict:store(PlanId
                                               ,prepare_subscription_addon(Update)
                                               ,Updates
                                               )
                                ,items=Items
                                }
                )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_activations(request()) -> 'ok'.
process_activations(#request{activation_charges=[]}) ->
    'ok';
process_activations(Request) ->
    Transaction = create_activation_transaction(Request),
    handle_transaction_sale_response(kz_transaction:sale(Transaction)).

-spec handle_transaction_sale_response({'ok', kz_transaction:transaction()} | {'error', any()}) -> 'ok'.
handle_transaction_sale_response({'ok', Transaction}) ->
    case kz_transaction:status_completed(Transaction) of
        'false' ->
            lager:debug("activation charge failed: transaction_incomplete");
        'true' ->
            lager:debug("activation charge succeeded")
    end;
handle_transaction_sale_response({'error', 'invalid_bookkeeper' = _Reason}) ->
    lager:debug("activation charge failed: ~p", [_Reason]);
handle_transaction_sale_response({'error', _Reason}) ->
    lager:debug("activation charge failed: ~p", [_Reason]).

-spec create_activation_transaction(request()) -> kz_transaction:transaction().
create_activation_transaction(#request{account_id=AccountId
                                      ,activation_charges=Activations
                                      ,request_jobj=JObj
                                      ,vendor_id=VendorId
                                      }) ->
    AmountUnits = kz_currency:dollars_to_units(get_activation_amount(JObj, Activations)),
    AuditLog = kz_json:get_json_value(<<"Audit-Log">>, JObj, kz_json:new()),
    Metadata = kz_json:from_list([{<<"automatic_description">>, 'true'}]),
    Setters =
        props:filter_empty(
          [{fun kz_transaction:set_account_id/2, AccountId}
          ,{fun kz_transaction:set_account_name/2, kzd_accounts:fetch_name(AccountId)}
          ,{fun kz_transaction:set_audit/2, AuditLog}
          ,{fun kz_transaction:set_bookkeeper_type/2, ?APP_NAME}
          ,{fun kz_transaction:set_bookkeeper_vendor_id/2, VendorId}
          ,{fun kz_transaction:set_description/2, create_description(Activations)}
          ,{fun kz_transaction:set_executor_trigger/2, <<"service_update">>}
          ,{fun kz_transaction:set_executor_module/2, kz_term:to_binary(?MODULE)}
          ,{fun kz_transaction:set_metadata/2, Metadata}
          ,{fun kz_transaction:set_unit_amount/2, AmountUnits}
          ]
         ),
    kz_transaction:setters(Setters).

-spec get_activation_amount(kz_json:object(), kz_json:objects()) -> float().
get_activation_amount(JObj, Activations) ->
    Today = kz_json:get_float_value([<<"Invoice">>, <<"summary">>, <<"today">>], JObj),
    get_activation_amount(JObj, Activations, Today).

-spec get_activation_amount(kz_json:object(), kz_json:objects(), kz_term:api_float()) -> float().
get_activation_amount(_, Activations, 'undefined') ->
    lists:sum([kzd_activation_item:total(ActivationItem)
               || ActivationItem <- Activations
              ]
             );
get_activation_amount(_, _, Today) ->
    Today.

-spec create_description(kz_json:objects()) -> kz_term:ne_binary().
create_description([ActivationItem]) ->
    Name = kzd_activation_item:name(ActivationItem, <<>>),
    <<Name/binary, " activation charge">>;
create_description(Activations) ->
    Items = [create_activation_item_desc(ActivationItem)
             || ActivationItem <- Activations
            ],
    <<"activation charges for items ", (kz_binary:join(Items))/binary>>.

-spec create_activation_item_desc(kz_json:object()) -> kz_term:ne_binary().
create_activation_item_desc(ActivationItem) ->
    Billable = kz_term:to_binary(kzd_activation_item:billable(ActivationItem, 1)),
    Category = kzd_activation_item:category(ActivationItem, <<>>),
    ItemName = kzd_activation_item:item(ActivationItem, <<>>),
    CatItem = kz_binary:join([Bin || Bin <- [Category, ItemName],
                                     Bin =/= <<>>
                             ]
                            ,<<$/>>
                            ),
    case kzd_activation_item:name(ActivationItem, CatItem) of
        <<>> -> <<Billable/binary, " x Item(s)">>;
        Name -> <<Billable/binary, " x ", Name/binary>>
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_subscription(braintree_subscription:subscription()) -> kz_term:proplist().
update_subscription(Subscription) ->
    SubscriptionId = braintree_subscription:get_id(Subscription),
    try braintree_subscription:update(Subscription) of
        UpdatedSubscription ->
            [{<<"Status">>, <<"success">>}
            ,{<<"Subscription-ID">>, SubscriptionId}
            ,{<<"Details">>
             ,kz_json:to_proplist(braintree_subscription:record_to_json(UpdatedSubscription))
             }
            ]
    catch
        'throw':Error ->
            [{<<"Subscription-ID">>, SubscriptionId}
             | braintree_util:error_to_props(Error)
            ]
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_plan_subscription(kz_term:ne_binary(), request()) -> braintree_subscription:subscription().
get_plan_subscription(PlanId, #request{updates=Updates
                                      ,customer=Customer
                                      }=Request
                     ) ->
    case dict:find(PlanId, Updates) of
        {'ok', Subscription} -> Subscription;
        'error' ->
            get_plan_subscription(PlanId, Request, Customer)
    end.

-spec get_plan_subscription(kz_term:ne_binary(), request(), braintree_customer:customer()) ->
          braintree_subscription:subscription().
get_plan_subscription(PlanId, Request, #bt_customer{}=Customer) ->
    try braintree_customer:get_subscription(PlanId, Customer) of
        Subscription ->
            lager:debug("found subscription ~s for plan id ~s"
                       ,[braintree_subscription:get_id(Subscription), PlanId]
                       ),
            braintree_subscription:reset(Subscription)
    catch
        'throw':{'not_found', _} ->
            lager:debug("creating new subscription for plan id ~s", [PlanId]),
            braintree_customer:new_subscription(PlanId, Customer);
        'throw':Error ->
            reply(Request, braintree_util:error_to_props(Error))
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec prepare_subscription_addon(update()) -> braintree_subscription:subscription().
prepare_subscription_addon(Update) ->
    Routines = [fun update_addon_quantity/1
               ,fun update_addon_amount/1
               ,fun update_addon_discount_amount/1
               ],
    #update{subscription=Subscription} = lists:foldl(fun(F, U) -> F(U) end, Update, Routines),
    Subscription.

-spec update_addon_quantity(update()) -> update().
update_addon_quantity(#update{item=Item
                             ,addon_id=AddOnId
                             ,subscription=Subscription
                             }=Update) ->
    Quantity = kz_json:get_integer_value(<<"billable">>, Item, 0),
    UpdatedSubscription =
        braintree_subscription:update_addon_quantity(Subscription, AddOnId, Quantity),
    Update#update{subscription=UpdatedSubscription}.

-spec update_addon_amount(update()) -> update().
update_addon_amount(#update{item=Item
                           ,addon_id=AddOnId
                           ,subscription=Subscription
                           }=Update) ->
    Rate = kz_json:get_float_value(<<"rate">>, Item, 0.0),
    UpdatedSubscription =
        braintree_subscription:update_addon_amount(Subscription, AddOnId, Rate),
    Update#update{subscription=UpdatedSubscription}.

-spec update_addon_discount_amount(update()) -> update().
update_addon_discount_amount(#update{item=Item
                                    ,mapping=Mapping
                                    ,subscription=Subscription
                                    }=Update) ->
    DiscountId = kz_json:get_ne_binary_value([<<"discounts">>, <<"cumulative">>], Mapping),
    Rate = kz_json:get_ne_binary_value([<<"discounts">>, <<"total">>], Item),
    case kz_term:is_empty(Rate) of
        'true' ->
            UpdatedSubscription =
                braintree_subscription:update_discount_quantity(Subscription, DiscountId, 0),
            Update#update{subscription=UpdatedSubscription};
        'false' ->
            UpdatedQuantity = braintree_subscription:update_discount_quantity(Subscription, DiscountId, 1),
            UpdatedAmount =
                braintree_subscription:update_discount_amount(UpdatedQuantity
                                                             ,DiscountId
                                                             ,Rate
                                                             ),
            Update#update{subscription=UpdatedAmount}
    end.
