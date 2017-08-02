%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_bookkeeper_local).
-behaviour(kz_gen_bookkeeper).

-export([is_good_standing/2]).
-export([sync/2]).
-export([transactions/3]).
-export([commit_transactions/2]).
-export([charge_transactions/2]).

-include("services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_good_standing(ne_binary(), ne_binary()) -> boolean().
is_good_standing(_AccountId, _Status) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec sync(kz_service_item:items(), ne_binary()) -> bookkeeper_sync_result().
sync(_Items, _AccountId) -> 'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec commit_transactions(ne_binary(), kz_transactions:kz_transactions()) -> ok | error.
commit_transactions(_BillingId, Transactions) ->
    kz_transactions:save(Transactions),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec charge_transactions(ne_binary(), kz_json:objects()) -> kz_json:objects().
charge_transactions(_BillingId, _Transactions) -> [].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec transactions(ne_binary(), gregorian_seconds(), gregorian_seconds()) ->
                          {'ok', kz_transaction:transactions()} |
                          {'error', atom()}.
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
            ,{<<"Amount">>, wht_util:units_to_dollars(kz_transaction:amount(Transaction))}
            ,{<<"Response">>, <<"Authorized">>}
            ,{<<"Success">>, <<"true">>}
            ,{<<"ID">>, kz_transaction:id(Transaction)}
            ,{<<"Timestamp">>, kz_time:current_tstamp()}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    kapps_notify_publisher:cast(Props, fun kapi_notifications:publish_topup/1).
