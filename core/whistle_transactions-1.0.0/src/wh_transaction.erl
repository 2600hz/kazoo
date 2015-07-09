%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(wh_transaction).

-export([id/1, set_id/2]).
-export([rev/1, set_rev/2]).
-export([description/1, set_description/2]).
-export([call_id/1, set_call_id/2]).
-export([sub_account_id/1, set_sub_account_id/2]).
-export([sub_account_name/1, set_sub_account_name/2]).
-export([set_sub_account_info/2]).
-export([event/1, set_event/2]).
-export([number/1, set_number/2]).
-export([feature/1, set_feature/2]).
-export([bookkeeper_info/1, set_bookkeeper_info/2]).
-export([metadata/1, set_metadata/2]).
-export([reason/1, set_reason/2]).
-export([code/1, set_code/2]).
-export([amount/1, set_amount/2]).
-export([type/1, set_type/2]).
-export([created/1, set_created/2]).
-export([modified/1, set_modified/2]).
-export([account_id/1, set_account_id/2]).
-export([account_db/1, set_account_db/2]).
-export([version/1, set_version/2]).
-export([status/1, set_status/2]).
-export([order_id/1, set_order_id/2]).

-export([new/0]).
-export([debit/2]).
-export([credit/2]).

-export([is_reason/2]).
-export([to_json/1]).
-export([to_public_json/1]).
-export([from_json/1]).
-export([remove/1]).
-export([save/1]).
-export([service_save/1]).

-include("../include/whistle_transactions.hrl").

-define(WH_SERVICES_DB, <<"services">>).

-record(wh_transaction, {id :: api_binary()
                         ,rev :: api_binary()
                         ,description :: api_binary()
                         ,call_id :: api_binary()
                         ,sub_account_id :: ne_binary()
                         ,sub_account_name :: api_binary()
                         ,event :: api_binary()
                         ,number :: api_binary()
                         ,feature :: api_binary()
                         ,bookkeeper_info :: api_object()
                         ,metadata :: api_object()
                         ,pvt_status :: api_binary()
                         ,pvt_reason :: api_binary()
                         ,pvt_code :: api_integer()
                         ,pvt_amount = 0 :: non_neg_integer()
                         ,pvt_type :: ne_binary()
                         ,pvt_created :: gregorian_seconds()
                         ,pvt_modified :: gregorian_seconds()
                         ,pvt_account_id :: ne_binary()
                         ,pvt_account_db :: ne_binary()
                         ,pvt_vsn = 2 :: integer()
                         ,order_id :: api_binary()
                        }).

-type transaction() :: #wh_transaction{}.
-type transactions() :: [transaction(),...] | [].
-export_type([transaction/0
              ,transactions/0
             ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% GET
%% @end
%%--------------------------------------------------------------------
-spec id(transaction()) -> ne_binary().
id(#wh_transaction{id=Id}) ->
    Id.

-spec rev(transaction()) -> ne_binary().
rev(#wh_transaction{rev=Rev}) ->
    Rev.

-spec description(transaction()) -> api_binary().
description(#wh_transaction{description=Description}) ->
    Description.

-spec call_id(transaction()) -> ne_binary().
call_id(#wh_transaction{call_id=CallId}) ->
    CallId.

-spec sub_account_id(transaction()) -> ne_binary().
sub_account_id(#wh_transaction{sub_account_id=SubAccountId}) ->
    SubAccountId.

-spec sub_account_name(transaction()) -> api_binary().
sub_account_name(#wh_transaction{sub_account_name=SubAccountName}) ->
    SubAccountName.

-spec event(transaction()) -> ne_binary().
event(#wh_transaction{event=Event}) ->
    Event.

-spec number(transaction()) -> ne_binary().
number(#wh_transaction{number=Number}) ->
    Number.

-spec feature(transaction()) -> ne_binary().
feature(#wh_transaction{feature=Feature}) ->
    Feature.

-spec bookkeeper_info(transaction()) -> wh_json:object().
bookkeeper_info(#wh_transaction{bookkeeper_info=BookkeeperInfo}) ->
    BookkeeperInfo.

-spec metadata(transaction()) -> api_object().
metadata(#wh_transaction{metadata=MetaData}) ->
    MetaData.

-spec reason(transaction()) -> ne_binary().
reason(#wh_transaction{pvt_reason=Reason}) ->
    Reason.

-spec status(transaction()) -> ne_binary().
status(#wh_transaction{pvt_status=Status}) ->
    Status.

-spec code(transaction()) -> api_integer().
code(#wh_transaction{pvt_code=Code}) ->
    Code.

-spec amount(transaction()) -> non_neg_integer().
amount(#wh_transaction{pvt_amount=Amount}) ->
    Amount.

-spec type(transaction()) -> ne_binary().
type(#wh_transaction{pvt_type=Type}) ->
    Type.

-spec created(transaction()) -> gregorian_seconds().
created(#wh_transaction{pvt_created='undefined'}) ->
    wh_util:current_tstamp();
created(#wh_transaction{pvt_created=Created}) ->
    Created.

-spec modified(transaction()) -> gregorian_seconds().
modified(#wh_transaction{pvt_modified='undefined'}) ->
    wh_util:current_tstamp();
modified(#wh_transaction{pvt_modified=Modified}) ->
    Modified.

-spec account_id(transaction()) -> ne_binary().
account_id(#wh_transaction{pvt_account_id=AccountId}) ->
    AccountId.

-spec account_db(transaction()) -> ne_binary().
account_db(#wh_transaction{pvt_account_db=AccountDb}) ->
    AccountDb.

-spec version(transaction()) -> integer().
version(#wh_transaction{pvt_vsn=Version}) ->
    Version.

-spec order_id(transaction()) -> api_binary().
order_id(#wh_transaction{order_id=OrderId}) ->
    OrderId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec new() -> transaction().
new() ->
    #wh_transaction{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create transaction record of type credit (with Amount & Reason)
%% @end
%%--------------------------------------------------------------------
-spec credit(ne_binary(), integer()) -> transaction().
credit(Ledger, Amount) ->
    create(Ledger, Amount, <<"credit">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create transaction record of type debit (with Amount & Reason)
%% @end
%%--------------------------------------------------------------------
-spec debit(ne_binary(), integer()) -> transaction().
debit(Ledger, Amount) ->
    create(Ledger, Amount, <<"debit">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% SET
%% @end
%%--------------------------------------------------------------------
-spec set_id(ne_binary(), transaction()) -> transaction().
set_id(Id, Transaction) ->
    Transaction#wh_transaction{id=Id}.

-spec set_rev(ne_binary(), transaction()) -> transaction().
set_rev(Rev, Transaction) ->
    Transaction#wh_transaction{rev=Rev}.

-spec set_description(ne_binary(), transaction()) -> transaction().
set_description(Desc, Transaction) when is_binary(Desc) ->
    Transaction#wh_transaction{description=Desc}.

-spec set_call_id(ne_binary(), transaction()) -> transaction().
set_call_id(CallId, Transaction) when is_binary(CallId) ->
    Transaction#wh_transaction{call_id=CallId}.

-spec set_sub_account_id(ne_binary(), transaction()) -> transaction().
set_sub_account_id(AccountId, Transaction) when is_binary(AccountId) ->
    Transaction#wh_transaction{sub_account_id=AccountId}.

-spec set_sub_account_name(ne_binary(), transaction()) -> transaction().
set_sub_account_name(AccountName, Transaction) when is_binary(AccountName) ->
    Transaction#wh_transaction{sub_account_name=AccountName}.

-spec set_sub_account_info(ne_binary(), transaction()) -> transaction().
set_sub_account_info(AccountId, Transaction) when is_binary(AccountId) ->
    case couch_mgr:open_cache_doc(<<"accounts">>, AccountId) of
        {'error', _R} ->
            lager:error("failed to open account ~s : ~p", [AccountId, _R]),
            Transaction#wh_transaction{sub_account_id=AccountId};
        {'ok', JObj} ->
            AccountName = wh_json:get_value(<<"name">>, JObj),
            Transaction#wh_transaction{sub_account_id=AccountId
                                       ,sub_account_name=AccountName
                                      }
    end.

-spec set_event(ne_binary(), transaction()) -> transaction().
set_event(Event, Transaction) ->
    Transaction#wh_transaction{event=Event}.

-spec set_number(ne_binary(), transaction()) -> transaction().
set_number(Number, Transaction) ->
    Transaction#wh_transaction{number=Number}.

-spec set_feature(ne_binary(), transaction()) -> transaction().
set_feature(Feature, Transaction) ->
    Transaction#wh_transaction{feature=Feature}.

-spec set_bookkeeper_info(wh_json:object(), transaction()) -> transaction().
set_bookkeeper_info(BookkeeperInfo, Transaction) ->
    Transaction#wh_transaction{bookkeeper_info=BookkeeperInfo}.

-spec set_metadata(wh_json:object(), transaction()) -> transaction().
set_metadata(MetaData, Transaction) ->
    Transaction#wh_transaction{metadata=MetaData}.

-spec set_reason(ne_binary(), transaction()) -> transaction().
set_reason(Reason, Transaction) ->
    Code = wht_util:reason_code(Reason),
    Transaction#wh_transaction{pvt_reason=Reason
                               ,pvt_code=Code
                              }.

-spec set_status(ne_binary(), transaction()) -> transaction().
set_status(Status, Transaction) ->
    Transaction#wh_transaction{pvt_status=Status}.

-spec set_code(pos_integer(), transaction()) -> transaction().
set_code(Code, Transaction) ->
    Reason = wht_util:code_reason(Code),
    Transaction#wh_transaction{pvt_reason=Reason
                               ,pvt_code=Code
                              }.

-spec set_amount(integer(), transaction()) -> transaction().
set_amount(Amount, Transaction) when Amount > 0 ->
    Transaction#wh_transaction{pvt_amount=Amount
                               ,pvt_type= <<"credit">>
                              };
set_amount(Amount, Transaction) when Amount < 0 ->
    Transaction#wh_transaction{pvt_amount=Amount
                               ,pvt_type= <<"debit">>
                              };
set_amount(Amount, Transaction) when is_binary(Amount) ->
    set_amount(wh_util:to_integer(Amount), Transaction);
set_amount(Amount, Transaction) ->
    Transaction#wh_transaction{pvt_amount=Amount}.

-spec set_type(ne_binary(), transaction()) -> transaction().
set_type(Type, Transaction) ->
    Transaction#wh_transaction{pvt_type=Type}.

-spec set_created(gregorian_seconds(), transaction()) -> transaction().
set_created(Created, Transaction) ->
    Transaction#wh_transaction{pvt_created=Created}.

-spec set_modified(gregorian_seconds(), transaction()) -> transaction().
set_modified(Modified, Transaction) ->
    Transaction#wh_transaction{pvt_modified=Modified}.

-spec set_account_id(ne_binary(), transaction()) -> transaction().
set_account_id(AccountId, Transaction) ->
    Transaction#wh_transaction{pvt_account_id=AccountId}.

-spec set_account_db(ne_binary(), transaction()) -> transaction().
set_account_db(AccountDb, Transaction) ->
    Transaction#wh_transaction{pvt_account_db=AccountDb}.

-spec set_version(integer(), transaction()) -> transaction().
set_version(Vsn, Transaction) ->
    Transaction#wh_transaction{pvt_vsn=Vsn}.

-spec set_order_id(api_binary(), transaction()) -> transaction().
set_order_id(OrderId, Transaction) ->
    Transaction#wh_transaction{order_id=OrderId}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_reason(ne_binary() | ne_binaries(), transaction()) -> boolean().
is_reason(Reason, #wh_transaction{pvt_reason=Reason}) -> 'true';
is_reason([Reason | _], #wh_transaction{pvt_reason=Reason}) -> 'true';
is_reason([_ | Reasons], Transaction) ->
    is_reason(Reasons, Transaction);
is_reason([], #wh_transaction{}) -> 'false';
is_reason(_, _) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Transform transaction record to Json object
%% @end
%%--------------------------------------------------------------------
-spec to_json(transaction()) -> wh_json:object().
to_json(#wh_transaction{}=T) ->
    Props = [{<<"_id">>, T#wh_transaction.id}
             ,{<<"_rev">>, T#wh_transaction.rev}
             ,{<<"description">>, T#wh_transaction.description}
             ,{<<"call_id">>, T#wh_transaction.call_id}
             ,{<<"sub_account_id">>, T#wh_transaction.sub_account_id}
             ,{<<"sub_account_name">>, T#wh_transaction.sub_account_name}
             ,{<<"event">>, T#wh_transaction.event}
             ,{<<"number">>, T#wh_transaction.number}
             ,{<<"feature">>, T#wh_transaction.feature}
             ,{<<"bookkeeper_info">>, T#wh_transaction.bookkeeper_info}
             ,{<<"metadata">>, T#wh_transaction.metadata}
             ,{<<"pvt_status">>, T#wh_transaction.pvt_status}
             ,{<<"pvt_reason">>, T#wh_transaction.pvt_reason}
             ,{<<"pvt_code">>, T#wh_transaction.pvt_code}
             ,{<<"pvt_amount">>, T#wh_transaction.pvt_amount}
             ,{<<"pvt_type">>, T#wh_transaction.pvt_type}
             ,{<<"pvt_created">>, T#wh_transaction.pvt_created}
             ,{<<"pvt_modified">>, T#wh_transaction.pvt_modified}
             ,{<<"pvt_account_id">>, T#wh_transaction.pvt_account_id}
             ,{<<"pvt_account_db">>, T#wh_transaction.pvt_account_db}
             ,{<<"pvt_vsn">>, T#wh_transaction.pvt_vsn}
             ,{<<"order_id">>, T#wh_transaction.order_id}
            ],
    Transaction = wh_json:from_list(props:filter_undefined(Props)),
    maybe_correct_transaction(Transaction).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Transform Json Object to transaction record
%% @end
%%--------------------------------------------------------------------
-spec to_public_json(transaction()) -> wh_json:object().
to_public_json(Transaction) ->
    JObj = to_json(Transaction),
    maybe_correct_transaction(clean_jobj(JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_correct_transaction(wh_json:object()) -> wh_json:object().
maybe_correct_transaction(JObj) ->
    Routines = [fun maybe_add_sub_account_name/1],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines).

-spec maybe_add_sub_account_name(wh_json:object()) -> wh_json:object().
maybe_add_sub_account_name(JObj) ->
    case
        {wh_json:get_value(<<"sub_account_name">>, JObj)
         ,wh_json:get_value(<<"sub_account_id">>, JObj)
        }
    of
        {'undefined', 'undefined'} -> JObj;
        {'undefined', AccountId} -> add_sub_account_name(AccountId, JObj);
        _ -> JObj
    end.

-spec add_sub_account_name(ne_binary(), wh_json:object()) -> wh_json:object().
add_sub_account_name(AccountId, JObj) ->
    case couch_mgr:open_cache_doc(<<"accounts">>, AccountId) of
        {'error', _R} ->
            lager:error("failed to open account doc ~s : ~p", [AccountId, _R]),
            JObj;
        {'ok', Doc} ->
            AccountName = wh_json:get_value(<<"name">>, Doc),
            wh_json:set_value(<<"sub_account_name">>, AccountName, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec clean_jobj(wh_json:object()) -> wh_json:object().
clean_jobj(JObj) ->
    CleanKeys = [{<<"_id">>, <<"id">>}
                 ,{<<"pvt_amount">>, <<"amount">>, fun wht_util:units_to_dollars/1}
                 ,{<<"pvt_reason">>, <<"reason">>}
                 ,{<<"pvt_status">>, <<"status">>}
                 ,{<<"pvt_type">>, <<"type">>}
                 ,{<<"pvt_created">>, <<"created">>}
                 ,{<<"pvt_vsn">>, <<"version">>}
                 ,{<<"pvt_code">>, <<"code">>}
                ],
    RemoveKeys = [<<"pvt_account_db">>
                  ,<<"pvt_account_id">>
                  ,<<"pvt_modified">>
                  ,<<"_rev">>
                 ],
    wh_json:normalize_jobj(JObj, RemoveKeys, CleanKeys).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Transform Json Object to transaction record
%% @end
%%--------------------------------------------------------------------
-spec from_json(wh_json:object()) -> transaction().
from_json(JObj) ->
    #wh_transaction{id = wh_doc:id(JObj)
                    ,rev = wh_doc:revision(JObj)
                    ,description = wh_json:get_ne_value(<<"description">>, JObj)
                    ,call_id = wh_json:get_ne_value(<<"call_id">>, JObj)
                    ,sub_account_id = wh_json:get_ne_value(<<"sub_account_id">>, JObj)
                    ,sub_account_name = wh_json:get_ne_value(<<"sub_account_name">>, JObj)
                    ,event = wh_json:get_ne_value(<<"event">>, JObj)
                    ,number = wh_json:get_ne_value(<<"number">>, JObj)
                    ,feature = wh_json:get_ne_value(<<"feature">>, JObj)
                    ,bookkeeper_info = wh_json:get_ne_value(<<"bookkeeper_info">>, JObj)
                    ,metadata = wh_json:get_ne_value(<<"metadata">>, JObj)
                    ,pvt_reason = wh_json:get_ne_value(<<"pvt_reason">>, JObj)
                    ,pvt_status = wh_json:get_ne_value(<<"pvt_status">>, JObj)
                    ,pvt_code = wh_json:get_integer_value(<<"pvt_code">>, JObj, 0)
                    ,pvt_amount = wh_json:get_integer_value(<<"pvt_amount">>, JObj, 0)
                    ,pvt_type = wh_doc:type(JObj)
                    ,pvt_created = wh_doc:created(JObj)
                    ,pvt_modified = wh_doc:modified(JObj)
                    ,pvt_account_id = wh_doc:account_id(JObj)
                    ,pvt_account_db = wh_doc:account_db(JObj)
                    ,pvt_vsn = wh_doc:vsn(JObj, 1)
                    ,order_id = wh_json:get_ne_value(<<"order_id">>, JObj)
                   }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec remove(transaction()) -> 'ok' | {'error', _}.
remove(#wh_transaction{}=Transaction) ->
    case prepare_transaction(Transaction) of
        {'error', _}=E -> E;
        #wh_transaction{}=T ->
            remove_transaction(T)
    end.

-spec remove_transaction(transaction()) -> 'ok' | {'error', _}.
remove_transaction(#wh_transaction{pvt_account_db=AccountDb}=Transaction) ->
    case couch_mgr:del_doc(AccountDb, to_json(Transaction)) of
        {'ok', _} -> 'ok';
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec save(transaction()) -> {'ok', transaction()} | {'error', _}.
save(#wh_transaction{}=Transaction) ->
    case prepare_transaction(Transaction) of
        {'error', _}=E -> E;
        #wh_transaction{}=T ->
            save_transaction(T)
    end.

-spec save_transaction(transaction()) -> {'ok', transaction()} | {'error', _}.
save_transaction(#wh_transaction{pvt_account_id=AccountId
                                 ,pvt_created=Created
                                }=Transaction) ->
    JObj = to_json(Transaction#wh_transaction{pvt_modified=wh_util:current_tstamp()}),
    case kazoo_modb:save_doc(AccountId, JObj, Created) of
        {'ok', J} -> {'ok', from_json(J)};
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec service_save(transaction()) ->
                          {'ok', wh_json:object()} |
                          {'error', _}.
service_save(#wh_transaction{}=Transaction) ->
    case prepare_transaction(Transaction) of
        {'error', _}=E -> E;
        #wh_transaction{}=T ->
            service_save_transaction(T)
    end.

-spec service_save_transaction(transaction()) ->
                                      {'ok', wh_json:object()} |
                                      {'error', _}.
service_save_transaction(#wh_transaction{pvt_account_id=AccountId}=Transaction) ->
    TransactionJObj = to_json(Transaction#wh_transaction{pvt_modified=wh_util:current_tstamp()}),
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {'error', _R}=Error ->
            lager:debug("unable to open account ~s services doc: ~p", [AccountId, _R]),
            Error;
        {'ok', JObj} ->
            Transactions = wh_json:get_value(<<"transactions">>, JObj, []),
            JObj1 = wh_json:set_values(
                      [{<<"transactions">>, [TransactionJObj|Transactions]}
                      ,{<<"pvt_dirty">>, 'true'}
                      ], JObj),
            couch_mgr:save_doc(?WH_SERVICES_DB, JObj1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec prepare_transaction(transaction()) ->
                                 transaction() |
                                 {'error', _}.
prepare_transaction(#wh_transaction{pvt_account_id='undefined'}) ->
    {'error', 'account_id_missing'};
prepare_transaction(#wh_transaction{pvt_account_db='undefined'}) ->
    {'error', 'account_db_missing'};
prepare_transaction(#wh_transaction{pvt_code=Code}=Transaction)
  when ?CODE_PER_MINUTE_CALL =:= Code
       orelse ?CODE_SUB_ACCOUNT_PER_MINUTE_CALL =:= Code ->
    prepare_call_transaction(Transaction);
prepare_transaction(#wh_transaction{pvt_code=Code}=Transaction)
  when ?CODE_FEATURE_ACTIVATION =:= Code
       orelse ?CODE_SUB_ACCOUNT_FEATURE_ACTIVATION =:= Code ->
    prepare_feature_activation_transaction(Transaction);
prepare_transaction(#wh_transaction{pvt_code=Code}=Transaction)
  when ?CODE_NUMBER_ACTIVATION =:= Code
       orelse ?CODE_SUB_ACCOUNT_NUMBER_ACTIVATION =:= Code ->
    prepare_number_activation_transaction(Transaction);
prepare_transaction(#wh_transaction{pvt_code=Code}=Transaction)
  when ?CODE_MANUAL_ADDITION =:= Code
       orelse ?CODE_SUB_ACCOUNT_MANUAL_ADDITION =:= Code ->
    prepare_manual_addition_transaction(Transaction);
prepare_transaction(#wh_transaction{pvt_code=Code}=Transaction)
  when ?CODE_DATABASE_ROLLUP =:= Code ->
    prepare_rollup_transaction(Transaction);
prepare_transaction(#wh_transaction{pvt_code=Code}=Transaction)
  when ?CODE_TOPUP =:= Code ->
    prepare_topup_transaction(Transaction);
prepare_transaction(Transaction) ->
    Transaction.

-spec prepare_call_transaction(transaction()) ->
                                      transaction() |
                                      {'error', _}.
prepare_call_transaction(#wh_transaction{call_id='undefined'}) ->
    {'error', 'call_id_missing'};
prepare_call_transaction(#wh_transaction{sub_account_id='undefined'
                                         ,pvt_code=?CODE_SUB_ACCOUNT_PER_MINUTE_CALL
                                        }) ->
    {'error', 'sub_account_id_missing'};
prepare_call_transaction(#wh_transaction{event='undefined'}) ->
    {'error', 'event_missing'};
prepare_call_transaction(#wh_transaction{call_id=CallId
                                         ,event=Event
                                        }=Transaction) ->
    Transaction#wh_transaction{id = <<CallId/binary, "-"
                                      ,(wh_util:to_upper_binary(Event))/binary
                                    >>
                                   ,event=wh_util:to_lower_binary(Event)
                              }.

-spec prepare_feature_activation_transaction(transaction()) ->
                                                    transaction() |
                                                    {'error', _}.
prepare_feature_activation_transaction(#wh_transaction{feature='undefined'}) ->
    {'error', 'feature_name_missing'};
prepare_feature_activation_transaction(#wh_transaction{number='undefined'}) ->
    {'error', 'number_missing'};
prepare_feature_activation_transaction(#wh_transaction{sub_account_id='undefined'
                                                       ,pvt_code=?CODE_SUB_ACCOUNT_FEATURE_ACTIVATION
                                                      }) ->
    {'error', 'sub_account_id_missing'};
prepare_feature_activation_transaction(Transaction) ->
    Transaction.

-spec prepare_number_activation_transaction(transaction()) ->
                                                   transaction() |
                                                   {'error', _}.
prepare_number_activation_transaction(#wh_transaction{number='undefined'}) ->
    {'error', 'number_missing'};
prepare_number_activation_transaction(#wh_transaction{sub_account_id='undefined'
                                                      ,pvt_code=?CODE_SUB_ACCOUNT_NUMBER_ACTIVATION
                                                     }) ->
    {'error', 'sub_account_id_missing'};
prepare_number_activation_transaction(Transaction) ->
    Transaction.

-spec prepare_manual_addition_transaction(transaction()) ->
                                                 transaction() |
                                                 {'error', _}.
prepare_manual_addition_transaction(#wh_transaction{bookkeeper_info='undefined'}) ->
    {'error', 'bookkeeper_info_missing'};
prepare_manual_addition_transaction(#wh_transaction{sub_account_id='undefined'
                                                    ,pvt_code=?CODE_SUB_ACCOUNT_MANUAL_ADDITION
                                                   }) ->
    {'error', 'sub_account_id_missing'};
prepare_manual_addition_transaction(Transaction) ->
    Transaction.

-spec prepare_rollup_transaction(transaction()) -> transaction().
prepare_rollup_transaction(Transaction) ->
    Id = <<"monthly_rollup">>,
    Transaction#wh_transaction{id=Id}.

-spec prepare_topup_transaction(transaction()) -> transaction().
prepare_topup_transaction(Transaction) ->
    {_, M, D} = erlang:date(),
    Month = wh_util:pad_month(M),
    Day = wh_util:pad_month(D),
    Id = <<"topup-", Month/binary, Day/binary>>,
    Transaction#wh_transaction{id=Id}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create transaction record
%% @end
%%--------------------------------------------------------------------
-spec create(ne_binary(), non_neg_integer(), ne_binary()) -> transaction().
create(Ledger, Amount, Type) ->
    #wh_transaction{pvt_type=Type
                    ,pvt_amount=abs(Amount)
                    ,pvt_account_id=wh_util:format_account_id(Ledger, 'raw')
                    ,pvt_account_db=wh_util:format_account_mod_id(Ledger)
                    ,pvt_created=wh_util:current_tstamp()
                    ,pvt_modified=wh_util:current_tstamp()
                   }.
