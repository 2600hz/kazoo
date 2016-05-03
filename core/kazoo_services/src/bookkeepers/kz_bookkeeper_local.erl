%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_bookkeeper_local).

-export([sync/2]).
-export([transactions/3]).
-export([commit_transactions/2]).
-export([charge_transactions/2]).

-include("kazoo_services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
sync(_Items, _AccountId) -> 'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec commit_transactions(ne_binary(), kz_transactions:kz_transactions()) -> 'ok'.
commit_transactions(_BillingId, Transactions) ->
    kz_transactions:save(Transactions),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec charge_transactions(ne_binary(), kz_transactions:kz_transactions()) -> [].
charge_transactions(_BillingId, _Transactions) -> [].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec transactions(ne_binary(), gregorian_seconds(), gregorian_seconds()) ->
                          {'ok', kz_transaction:transactions()} |
                          {'error', any()}.
transactions(AccountId, From, To) ->
    case kz_transactions:fetch_local(AccountId, From, To) of
        {'error', _Reason}=Error -> Error;
        {'ok', Transactions}=Res ->
            handle_topup(AccountId, Transactions),
            Res
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_topup(ne_binary(), kz_transactions:transactions()) -> 'ok'.
handle_topup(_, []) -> 'ok';
handle_topup(BillingId, [Transaction|Transactions]) ->
    case kz_transaction:code(Transaction) =:= ?CODE_TOPUP of
        'false' ->
            handle_topup(BillingId, Transactions);
        'true' ->
            send_topup_notification(BillingId, Transaction)
    end.

-spec send_topup_notification(ne_binary(), kz_transaction:transaction()) -> 'ok'.
send_topup_notification(BillingId, Transaction) ->
    Props = [{<<"Account-ID">>, BillingId}
             ,{<<"Amount">>, kz_transaction:amount(Transaction)}
             ,{<<"Response">>, <<"Authorized">>}
             ,{<<"Success">>, <<"true">>}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    case
        kapps_util:amqp_pool_send(
          Props
          ,fun kapi_notifications:publish_topup/1
         )
    of
        'ok' ->
            lager:debug("topup notification sent for ~s", [BillingId]);
        {'error', _R} ->
            lager:error(
              "failed to send topup notification for ~s : ~p"
                       ,[BillingId, _R]
             )
    end.
