%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_bookkeeper_local).

-export([sync/2]).
-export([transactions/3]).
-export([commit_transactions/2]).
-export([charge_transactions/2]).

-include("../whistle_services.hrl").

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
-spec commit_transactions(ne_binary(), wh_transactions:wh_transactions()) -> 'ok'.
commit_transactions(_BillingId, Transactions) ->
    wh_transactions:save(Transactions),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec charge_transactions(ne_binary(), wh_transactions:wh_transactions()) -> [].
charge_transactions(_BillingId, _Transactions) -> [].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec transactions(ne_binary(), gregorian_seconds(), gregorian_seconds()) ->
                          {'ok', wh_transaction:transactions()} |
                          {'error', any()}.
transactions(AccountId, From, To) ->
    case wh_transactions:fetch_local(AccountId, From, To) of
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
-spec handle_topup(ne_binary(), wh_transactions:transactions()) -> 'ok'.
handle_topup(_, []) -> 'ok';
handle_topup(BillingId, [Transaction|Transactions]) ->
    case wh_transaction:code(Transaction) =:= ?CODE_TOPUP of
        'false' ->
            handle_topup(BillingId, Transactions);
        'true' ->
            send_topup_notification(BillingId, Transaction)
    end.

-spec send_topup_notification(ne_binary(), wh_transaction:transaction()) -> 'ok'.
send_topup_notification(BillingId, Transaction) ->
    Props = [{<<"Account-ID">>, BillingId}
             ,{<<"Amount">>, wh_transaction:amount(Transaction)}
             ,{<<"Response">>, <<"Authorized">>}
             ,{<<"Success">>, <<"true">>}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    case
        whapps_util:amqp_pool_send(
          Props
          ,fun wapi_notifications:publish_topup/1
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
