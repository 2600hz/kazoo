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
    Invoice = initialize(Account),
    create_items(Invoice).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec initialize/1 :: (ne_binary()) -> #wh_invoice{}.
initialize(Account) ->
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
                                 I#wh_invoice{billing_id=BillingId};
                             {error, _R} ->
                                 error_database(AccountId)
                         end
                 end
                ,fun(#wh_invoice{account_id=AccountId}=I) ->
                         I#wh_invoice{service_plans=wh_service_plans:fetch(AccountId)}
                 end

               ],
    lists:foldl(fun(F, I) -> F(I) end, #wh_invoice{}, Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_items/1 :: (#wh_invoice{}) -> #wh_invoice{}.
create_items(#wh_invoice{service_plans=ServicePlans, services=Services}=Invoice) ->
    Invoice#wh_invoice{items=wh_service_plans:create_items(Services, ServicePlans)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec error_database/1 :: (atom()) -> no_return().
error_database(Reason) ->
    Error = <<"Unable to open document ", (wh_util:to_binary(Reason))/binary>>,
    lager:debug("~s", [Error]),
    throw({database_error, Error}).
