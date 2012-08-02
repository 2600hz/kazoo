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
-export([reconcile/1, reconcile/2]).
-export([fetch/1]).
-export([update/4]).
-export([save/1]).

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

-record(wh_services, {account_id
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
-spec allow_updates/1 :: (ne_binary()) -> boolean().
allow_updates(_Account) ->
    true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile/1 :: (ne_binary()) -> services().
reconcile(Account) ->
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
    case get_service_module(Module) of
        false -> false;
        ServiceModule ->
            CurrentServices = fetch(Account),
            UpdatedServices = ServiceModule:reconcile(CurrentServices),
            save(UpdatedServices)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch/1 :: (ne_binary()) -> services().
fetch(Account) ->
    AccountId = wh_util:format_account_id(Account, raw),
    AccountDb = wh_util:format_account_id(Account, encoded),
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {ok, JObj} ->
            #wh_services{account_id=AccountId, jobj=JObj
                         ,cascade_quantities=cascade_quantities(AccountId)};
        {error, _R} ->
            lager:debug("unable to open account ~s services doc: ~p", [Account, _R]),
            TStamp = wh_util:current_tstamp(),
            New = [{<<"_id">>, AccountId}
                   ,{<<"pvt_created">>, TStamp}
                   ,{<<"pvt_modified">>, TStamp}
                   ,{<<"pvt_type">>, <<"service">>}
                   ,{<<"pvt_vsn">>, <<"1">>}
                   ,{<<"pvt_account_id">>, AccountId}
                   ,{<<"pvt_account_db">>, AccountDb}
                   ,{<<"pvt_dirty">>, true}
                   ,{?QUANTITIES, wh_json:new()}
                  ],
            #wh_services{account_id=AccountId, jobj=wh_json:from_list(New)
                         ,cascade_quantities=cascade_quantities(AccountId)}
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec save/1 :: (services()) -> services().
save(#wh_services{jobj=JObj, updates=UpdatedQuantities, account_id=AccountId, dirty=Dirty}=Services) ->
    CurrentQuantities = wh_json:get_value(?QUANTITIES, JObj, wh_json:new()),
    Props = [{<<"_id">>, AccountId}
             ,{<<"pvt_dirty">>, Dirty}
             ,{<<"pvt_modified">>, wh_util:current_tstamp()}
             ,{<<"pvt_tree">>, get_pvt_tree(AccountId)}
             ,{?QUANTITIES, wh_json:merge_jobjs(UpdatedQuantities, CurrentQuantities)}
            ],
    UpdatedJObj = wh_json:set_values(Props, JObj),
    case couch_mgr:save_doc(?WH_SERVICES_DB, UpdatedJObj) of
        {ok, NewJObj} ->
            Services#wh_services{jobj=NewJObj
                                 ,cascade_quantities=cascade_quantities(AccountId)};
        {error, not_found} ->
            true = couch_mgr:db_create(?WH_SERVICES_DB),
            save(Services);
        {error, conflict} ->
            {ok, Existing} = couch_mgr:open_doc(?WH_SERVICES_DB, AccountId),
            save(Services#wh_services{jobj=Existing})
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
cascade_quantity(Category, Item, #wh_services{cascade_quantities=JObj}) ->
    wh_json:get_integer_value([Category, Item], JObj, 0).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cascade_category_quantity/3 :: (ne_binary(), [ne_binary(),...] |[], services()) -> integer().
cascade_category_quantity(Category, Exceptions, #wh_services{cascade_quantities=Quantities}) ->
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
            wh_json:get_value(<<"pvt_tree">>, JObj, []) ++ [AccountId];
        {error, _R} ->
            lager:debug("unable to open account definition for ~s: ~p", [AccountId, _R]),
            [AccountId]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cascade_quantities/1 :: (ne_binary()) -> wh_json:json_object().            
cascade_quantities(Account) ->
    AccountId = wh_util:format_account_id(Account, raw),
    ViewOptions = [group
                   ,reduce
                   ,{startkey, [AccountId]}
                   ,{endkey, [AccountId, wh_json:new()]}
                  ],
    {ok, JObjs} = couch_mgr:get_results(?WH_SERVICES_DB, <<"services/cascade_quantities">>, ViewOptions),
    lists:foldl(fun(JObj, J) ->
                        Key = wh_json:get_value(<<"key">>, JObj),
                        Value = wh_json:get_integer_value(<<"value">>, JObj),
                        wh_json:set_value(tl(Key), Value, J)
                end, wh_json:new(), JObjs).
