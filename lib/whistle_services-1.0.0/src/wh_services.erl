%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_services).

-export([empty/0]).
-export([allow_updates/1]).
-export([from_service_json/1]).
-export([reconcile/1, reconcile/2]).
-export([fetch/1]).
-export([update/4]).
-export([save/1]).
-export([delete/1]).

-export([account_id/1]).
-export([quantity/3]).
-export([update_quantity/3]).
-export([category_quantity/3]).
-export([cascade_quantity/3]).
-export([cascade_category_quantity/3]).
-export([reset_category/2]).
-export([get_service_module/1]).

-include_lib("whistle_services/src/whistle_services.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-record(wh_services, {account_id = undefined
                      ,dirty = false
                      ,jobj = wh_json:new()
                      ,updates = wh_json:new()
                      ,cascade_quantities = wh_json:new()
                     }).

-define(QUANTITIES, <<"quantities">>).

-type(services() :: #wh_services{}).
-export_type([services/0]).

%%%===================================================================
%%% Operations
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec empty/0 :: () -> services().
empty() ->
    #wh_services{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec new/1 :: (ne_binary()) -> services().
new(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    Account = get_account_definition(AccountDb, AccountId),
    IsReseller = depreciated_is_reseller(Account),
    Props = [{<<"_id">>, AccountId}
             ,{<<"pvt_created">>, wh_util:current_tstamp()}
             ,{<<"pvt_modified">>, wh_util:current_tstamp()}
             ,{<<"pvt_type">>, <<"service">>}
             ,{<<"pvt_vsn">>, <<"1">>}
             ,{<<"pvt_account_id">>, AccountId}
             ,{<<"pvt_account_db">>, AccountDb}
             ,{<<"pvt_status">>, <<"good_standing">>}
             ,{<<"pvt_reseller">>, IsReseller}
             ,{?QUANTITIES, wh_json:new()}
             ,{<<"billing_id">>, depreciated_billing_id(Account)}
             ,{<<"plans">>, depreciated_service_plans(Account)}
            ],
    #wh_services{account_id=AccountId, jobj=wh_json:from_list(Props)
                 ,cascade_quantities=cascade_quantities(AccountId, IsReseller)
                 ,dirty=true}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec from_service_json/1 :: (wh_json:json_object()) -> services().
from_service_json(JObj) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    IsReseller = wh_json:is_true(<<"pvt_reseller">>, JObj),
    #wh_services{account_id=AccountId, jobj=JObj
                 ,cascade_quantities=cascade_quantities(AccountId, IsReseller)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch/1 :: (ne_binary()) -> services().
fetch(Account) ->
    AccountId = wh_util:format_account_id(Account, raw),
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {ok, JObj} ->
            lager:debug("loaded account service doc ~s", [AccountId]),
            IsReseller = wh_json:is_true(<<"pvt_reseller">>, JObj),
            #wh_services{account_id=AccountId, jobj=JObj
                         ,cascade_quantities=cascade_quantities(AccountId, IsReseller)};
        {error, _R} ->
            lager:debug("unable to open account ~s services doc (creating new): ~p", [Account, _R]),
            new(AccountId)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec save/1 :: (services()) -> services().
save(#wh_services{jobj=JObj, updates=UpdatedQuantities, account_id=AccountId}=Services) ->
    CurrentQuantities = wh_json:get_value(?QUANTITIES, JObj, wh_json:new()),
    Dirty = have_quantities_changed(UpdatedQuantities, CurrentQuantities),
    PvtTree = get_pvt_tree(AccountId),
    Props = [{<<"_id">>, AccountId}
             ,{<<"pvt_dirty">>, Dirty}
             ,{<<"pvt_modified">>, wh_util:current_tstamp()}
             ,{<<"pvt_tree">>, PvtTree}
             ,{<<"pvt_reseller_id">>, get_reseller_id(PvtTree)}
             ,{?QUANTITIES, wh_json:merge_jobjs(UpdatedQuantities, CurrentQuantities)}
            ],
    UpdatedJObj = wh_json:set_values(props:filter_undefined(Props), JObj),
    case couch_mgr:save_doc(?WH_SERVICES_DB, UpdatedJObj) of
        {ok, NewJObj} ->
            lager:debug("saved services for ~s", [AccountId]),
            IsReseller = wh_json:is_true(<<"pvt_reseller">>, JObj),
            Services#wh_services{jobj=NewJObj
                                 ,cascade_quantities=cascade_quantities(AccountId, IsReseller)};
        {error, not_found} ->
            lager:debug("service database does not exist, attempting to create", []),
            true = couch_mgr:db_create(?WH_SERVICES_DB),
            save(Services);
        {error, conflict} ->
            lager:debug("services for ~s conflicted, merging changes and retrying", [AccountId]),
            {ok, Existing} = couch_mgr:open_doc(?WH_SERVICES_DB, AccountId),
            save(Services#wh_services{jobj=Existing})
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete/1 :: (ne_binary()) -> wh_std_return().
delete(Account) ->
    AccountId = wh_util:format_account_id(Account, raw),
    %% TODO: support other bookkeepers, and just cancel subscriptions....
    _ = (catch braintree_customer:delete(AccountId)),
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {ok, JObj} ->
            lager:debug("marking services for account ~s as deleted", [AccountId]),
            couch_mgr:save_doc(?WH_SERVICES_DB, wh_json:set_values([{<<"pvt_deleted">>, true}
                                                                    ,{<<"pvt_dirty">>, true}
                                                                    ]
                                                                   ,JObj));
        {error, not_found} -> {ok, wh_json:new()};
        {error, _R}=E ->
            lager:debug("unable to mark service plan ~s as deleted: ~p", [AccountId, _R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update/4 :: (ne_binary(), ne_binary(), integer(), services()) -> services().
update(Category, Item, Quantity, Services) when not is_integer(Quantity) ->
    update(Category, Item, wh_util:to_integer(Quantity), Services);
update(Category, Item, Quantity, #wh_services{updates=JObj}=Services) when is_binary(Category), is_binary(Item) ->
    Services#wh_services{updates=wh_json:set_value([Category, Item], Quantity, JObj)}.

%%%===================================================================
%%% Services functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec allow_updates/1 :: (ne_binary()) -> boolean().
allow_updates(_Account) ->
    %% TODO ensure card is on file and account is in good standing
    true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile/1 :: (ne_binary()) -> services().
reconcile(Account) ->
    lager:debug("reconcile all services for ~s", [Account]),
    ServiceModules = get_service_modules(),
    Services = lists:foldl(fun(M, S) ->
                                   M:reconcile(S)
                           end, fetch(Account), ServiceModules),
    save(Services).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile/2 :: ('undefined' | ne_binary(), atom()) -> 'false' | services().
reconcile(undefined, _) ->
    false;
reconcile(Account, Module) ->
    timer:sleep(1000),
    lager:debug("reconcile ~s services for ~s", [Module, Account]),
    case get_service_module(Module) of
        false -> false;
        ServiceModule ->
            CurrentServices = fetch(Account),
            UpdatedServices = ServiceModule:reconcile(CurrentServices),
            maybe_save(UpdatedServices)
    end.

%%%===================================================================
%%% Access functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec account_id/1 :: (services()) -> ne_binary().
account_id(#wh_services{account_id=AccountId}) ->
    AccountId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec quantity/3 :: (ne_binary(), ne_binary(), services()) -> integer().
quantity(Category, Item, #wh_services{updates=UpdatedQuantities, jobj=JObj}) ->
    CurrentQuantities = wh_json:get_value(?QUANTITIES, JObj, wh_json:new()),
    Quantities = wh_json:merge_jobjs(UpdatedQuantities, CurrentQuantities),
    wh_json:get_integer_value([Category, Item], Quantities, 0).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_quantity/3 :: (ne_binary(), ne_binary(), services()) -> integer().
update_quantity(Category, Item, #wh_services{updates=JObj}) ->
    wh_json:get_integer_value([Category, Item], JObj, 0).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec category_quantity/3 :: (ne_binary(), [ne_binary(),...] |[], services()) -> integer().
category_quantity(Category, Exceptions, #wh_services{updates=UpdatedQuantities, jobj=JObj}) ->
    CurrentQuantities = wh_json:get_value(?QUANTITIES, JObj, wh_json:new()),
    Quantities = wh_json:merge_jobjs(UpdatedQuantities, CurrentQuantities),
    lists:foldl(fun(Item, Sum) ->
                        case lists:member(Item, Exceptions) of
                            true -> Sum;
                            false ->
                                wh_json:get_integer_value([Category, Item], Quantities, 0) + Sum
                        end
                end, 0, wh_json:get_keys(Category, Quantities)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cascade_quantity/3 :: (ne_binary(), ne_binary(), services()) -> integer().
cascade_quantity(Category, Item, #wh_services{cascade_quantities=JObj}=Services) ->
    wh_json:get_integer_value([Category, Item], JObj, 0) 
        + quantity(Category, Item, Services).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cascade_category_quantity/3 :: (ne_binary(), [ne_binary(),...] |[], services()) -> integer().
cascade_category_quantity(Category, Exceptions, #wh_services{cascade_quantities=Quantities}=Services) ->
    lists:foldl(fun(Item, Sum) ->
                        case lists:member(Item, Exceptions) of
                            true -> Sum;
                            false ->
                                wh_json:get_integer_value([Category, Item], Quantities, 0) + Sum
                        end
                end, category_quantity(Category, Exceptions, Services)
                ,wh_json:get_keys(Category, Quantities)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_category/2 :: (ne_binary(), services()) -> services().
reset_category(Category, #wh_services{updates=JObj}=Services) ->
    Services#wh_services{updates=wh_json:set_value(Category, wh_json:new(), JObj)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_service_modules/0 :: () -> [atom(),...] | [].
get_service_modules() ->
    case whapps_config:get(?WHS_CONFIG_CAT, <<"modules">>) of
        undefined ->
            Mods = [Mod
                    || P <- filelib:wildcard([code:lib_dir(whistle_services), "/src/services/*.erl"])
                           ,begin
                                Name = wh_util:to_binary(filename:rootname(filename:basename(P))),
                                (Mod = wh_util:try_load_module(Name)) =/= false
                            end
                   ],
            lager:debug("found service modules: ~p", [Mods]),
            Mods;
        Modules ->
            lager:debug("configured service modules: ~p", [Modules]),
            [Module || M <- Modules, (Module = wh_util:try_load_module(M)) =/= false]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_service_module/1 :: (text()) -> atom() | 'false'.
get_service_module(Module) when not is_binary(Module) ->
    get_service_module(wh_util:to_binary(Module));
get_service_module(<<"wh_service_", _/binary>>=Module) ->
    ServiceModules = get_service_modules(),
    case [M 
          || M <- ServiceModules
                 ,wh_util:to_binary(M) =:= Module
         ] 
    of
        [M] -> M;
        _Else -> false
    end;        
get_service_module(Module) ->
    get_service_module(<<"wh_service_", Module/binary>>).
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_pvt_tree/1 :: (ne_binary()) -> [ne_binary(),...] | [].
get_pvt_tree(Account) ->
    AccountId = wh_util:format_account_id(Account, raw),
    AccountDb = wh_util:format_account_id(Account, encoded),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {ok, JObj} ->
            wh_json:get_value(<<"pvt_tree">>, JObj, []);
        {error, _R} ->
            lager:debug("unable to open account definition for ~s: ~p", [AccountId, _R]),
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cascade_quantities/2 :: (ne_binary(), boolean()) -> wh_json:json_object().

cascade_quantities(Account, false) ->
    do_cascade_quantities(Account, <<"services/cascade_quantities">>);
cascade_quantities(Account, true) ->
    do_cascade_quantities(Account, <<"services/reseller_quantities">>).

-spec do_cascade_quantities/2 :: (ne_binary(), ne_binary()) -> wh_json:json_object().
do_cascade_quantities(Account, View) ->
    AccountId = wh_util:format_account_id(Account, raw),
    ViewOptions = [group
                   ,reduce
                   ,{startkey, [AccountId]}
                   ,{endkey, [AccountId, wh_json:new()]}
                  ],
    {ok, JObjs} = couch_mgr:get_results(?WH_SERVICES_DB, View, ViewOptions),
    lists:foldl(fun(JObj, J) ->
                        Key = wh_json:get_value(<<"key">>, JObj),
                        Value = wh_json:get_integer_value(<<"value">>, JObj),
                        wh_json:set_value(tl(Key), Value, J)
                end, wh_json:new(), JObjs).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine the billing id as it is currently set on the account
%% definition as this will be depreciated in the future.
%% @end
%%--------------------------------------------------------------------
-spec depreciated_billing_id/1 :: (wh_json:json_object()) -> ne_binary().
depreciated_billing_id(JObj) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    wh_json:get_value(<<"billing_id">>, JObj, AccountId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine if pvt_reseller is currently set on the account
%% definition as this will be depreciated in the future.
%% @end
%%--------------------------------------------------------------------
-spec depreciated_is_reseller/1 :: (wh_json:json_object()) -> boolean().
depreciated_is_reseller(JObj) ->
    wh_json:is_true(<<"pvt_reseller">>, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine what service plans are currently set on the account
%% definition as this will be depreciated in the future.
%% @end
%%--------------------------------------------------------------------
-spec depreciated_service_plans/1 :: (wh_json:json_object()) -> 'undefined' | wh_json:json_object().
depreciated_service_plans(JObj) ->
    case wh_json:get_value(<<"pvt_service_plans">>, JObj) of
        undefined -> wh_json:new();
        PlanIds ->
            VendorId = case wh_json:get_value(<<"pvt_reseller_id">>, JObj) of
                           undefined -> get_reseller_id(wh_json:get_value(<<"pvt_tree">>, JObj));
                           Else -> Else
                       end,
            Plans = [{PlanId, wh_json:from_list([{<<"vendor_id">>, VendorId}])}
                     || PlanId <- PlanIds
                    ],
            wh_json:from_list(Plans)
    end.    

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_reseller_id/1 :: ([ne_binary(),...] | []) -> 'undefined' | ne_binary().
get_reseller_id([]) -> 
    case whapps_util:get_master_account_id() of
        {ok, MasterAccountId} -> MasterAccountId;
        {error, _} -> undefined
    end;
get_reseller_id([Parent|Ancestors]) ->
    case couch_mgr:open_doc(?WH_SERVICES_DB, Parent) of
        {error, _R} ->
            lager:debug("failed to open services doc ~s durning reseller search: ~p", [Parent, _R]),
            get_reseller_id(Ancestors);
        {ok, JObj} ->
            case wh_json:is_true(<<"pvt_reseller">>, JObj) of
                false -> get_reseller_id(Ancestors);
                true -> Parent
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_save/1 :: (services()) -> services().
maybe_save(#wh_services{jobj=JObj, updates=UpdatedQuantities}=Services) ->
    CurrentQuantities = wh_json:get_value(?QUANTITIES, JObj, wh_json:new()),
    case have_quantities_changed(UpdatedQuantities, CurrentQuantities) of
        true -> 
            lager:debug("quantities have changed, saving dirty services"),
            save(Services);
        false -> 
            lager:debug("no service quantity changes"),
            {error, no_change}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec have_quantities_changed/2 :: (wh_json:json_object(), wh_json:json_object()) -> boolean().
have_quantities_changed(Updated, Current) ->
    lists:any(fun(Key) -> wh_json:get_value(Key, Updated) =/= wh_json:get_value(Key, Current) end
              ,[[Category, Item] 
                || Category <- wh_json:get_keys(Updated)
                       ,Item <- wh_json:get_keys(Category, Updated)
               ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_account_definition/2 :: (ne_binary(), ne_binary()) -> wh_json:json_object().
get_account_definition(AccountDb, AccountId) ->
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {error, _R} ->
            lager:debug("unable to get account defintion for ~s: ~p", [AccountId, _R]),
            wh_json:new();
        {ok, JObj} -> JObj
    end.
