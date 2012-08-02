%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_invoice).

-include_lib("whistle_services/src/whistle_services.hrl").

-export([sync/1]).

-record(wh_invoice, {account_id = undefined
                     ,account_db = undefined
                     ,billing_id = undefined
                     ,items = wh_service_items:empty() :: wh_service_items:items()
                     ,service_plans = wh_service_plans:empty() :: wh_service_plans:plans()
                     ,services = wh_services:empty()
                    }).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec sync/1 :: (ne_binary()) -> #wh_invoice{}.
sync(Account) ->
    create(Account).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (ne_binary()) -> #wh_invoice{}.
create(Account) ->
    Routines = [fun(I) -> I#wh_invoice{account_id=wh_util:format_account_id(Account, raw)
                                       ,account_db=wh_util:format_account_id(Account, encoded)}
                end
                ,fun(#wh_invoice{account_id=AccountId}=I) ->
                         I#wh_invoice{services=wh_services:fetch(AccountId)}
                 end
                ,fun(#wh_invoice{account_db=AccountDb, account_id=AccountId}=I) ->
                         case couch_mgr:open_doc(AccountDb, AccountId) of
                             {ok, JObj} ->
                                 BillingId = wh_json:get_ne_value(<<"billing_id">>, JObj, AccountId),
                                 lager:debug("using billing id ~s for account ~s", [BillingId, AccountId]),
                                 I#wh_invoice{billing_id=BillingId};
                             {error, _R} ->
                                 lager:debug("unable to open account definition for ~s (using account as billing id): ~p", [AccountId, _R]),
                                 I#wh_invoice{billing_id=AccountId}
                         end
                 end
                ,fun(#wh_invoice{account_id=AccountId}=I) ->
                         I#wh_invoice{service_plans=wh_service_plans:fetch(AccountId)}
                 end
                ,fun(#wh_invoice{service_plans=ServicePlans, services=Services}=I) ->
                         I#wh_invoice{items=wh_service_plans:create_items(Services, ServicePlans)}
                 end
               ],
    lists:foldl(fun(F, I) -> F(I) end, #wh_invoice{}, Routines).
