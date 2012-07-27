%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_services).

-export([reconcile/1]).
-export([fetch/1]).
-export([update/4]).
-export([save/1]).

-export([account_id/1]).
-export([get_quantity/3]).
-export([reset_category/2]).

-include_lib("whistle_services/src/whistle_services.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-record(wh_services, {account_id
                      ,dirty = false
                      ,jobj = wh_json:new()
                      ,updates = wh_json:new()
                     }).

-type(services() :: #wh_services{}).
-export_type([services/0]).

-spec reconcile/1 :: (ne_binary()) -> services().
reconcile(Account) ->
    ServiceModules = get_service_modules(),
    Services = lists:foldl(fun(M, S) ->
                                   M:reconcile(S)
                           end, fetch(Account), ServiceModules),
    save(Services).

-spec fetch/1 :: (ne_binary()) -> services().
fetch(Account) ->
    AccountId = wh_util:format_account_id(Account, raw),
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {ok, JObj} ->
            #wh_services{account_id=AccountId, jobj=JObj};
        {error, _R} ->
            lager:debug("unable to open account ~s services doc: ~p", [_R]),
            TStamp = wh_util:current_tstamp(),
            New = [{<<"_id">>, AccountId}
                   ,{<<"pvt_created">>, TStamp}
                   ,{<<"pvt_modified">>, TStamp}
                   ,{<<"pvt_type">>, <<"service">>}
                   ,{<<"pvt_vsn">>, <<"1">>}
                  ],
            #wh_services{account_id=AccountId, jobj=wh_json:from_list(New)}
    end.

-spec update/4 :: (ne_binary(), ne_binary(), integer(), services()) -> services().
update(Category, Item, Quantity, Services) when not is_integer(Quantity) ->
    update(Category, Item, wh_util:to_integer(Quantity), Services);
update(Category, Item, Quantity, #wh_services{updates=JObj}=Services) when is_binary(Category), is_binary(Item) ->
    Services#wh_services{updates=wh_json:set_value([Category, Item], Quantity, JObj)}.

-spec save/1 :: (services()) -> services().
save(#wh_services{jobj=JObj, updates=Updates, account_id=AccountId, dirty=Dirty}=Services) ->
    Props = [{<<"_id">>, AccountId}
             ,{<<"pvt_dirty">>, Dirty}
             ,{<<"pvt_modified">>, wh_util:current_tstamp()}
            ],
    UpdatedJObj = wh_json:merge_jobjs(wh_json:set_values(Props, Updates), JObj),
    case couch_mgr:save_doc(?WH_SERVICES_DB, UpdatedJObj) of
        {ok, NewJObj} ->
            Services#wh_services{jobj=NewJObj};
        {error, not_found} ->
            true = couch_mgr:db_create(?WH_SERVICES_DB),
            save(Services);
        {error, conflict} ->
            {ok, Existing} = couch_mgr:open_doc(?WH_SERVICES_DB, AccountId),
            save(Services#wh_services{jobj=Existing})
    end.

-spec account_id/1 :: (services()) -> ne_binary().
account_id(#wh_services{account_id=AccountId}) ->
    AccountId.

-spec get_quantity/3 :: (ne_binary(), ne_binary(), services()) -> integer().
get_quantity(Category, Item, #wh_services{updates=JObj}) ->
    wh_json:get_integer_value([Category, Item], JObj, 0).

-spec reset_category/2 :: (ne_binary(), services()) -> services().
reset_category(Category, #wh_services{updates=JObj}=Services) ->
    Services#wh_services{updates=wh_json:set_value(Category, wh_json:new(), JObj)}.

-spec get_service_modules/0 :: () -> [atom(),...] | [].
get_service_modules() ->
    case whapps_config:get(?WHS_CONFIG_CAT, <<"modules">>) of
        undefined ->
            Mods = [Mod
                    || P <- filelib:wildcard([code:lib_dir(whistle_services), "/ebin/wh_service_*.beam"])
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
