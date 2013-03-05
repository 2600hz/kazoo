%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------

-module(wh_transaction).

-include_lib("whistle/include/wh_types.hrl").

-export([description/1]).
-export([call_id/1]).
-export([sub_account_id/1]).
-export([event/1]).
-export([reason/1]).
-export([code/1]).
-export([amount/1]).
-export([type/1]).
-export([created/1]).
-export([modified/1]).
-export([account_id/1]).
-export([account_db/1]).
-export([debit/2]).
-export([credit/2]).
-export([set_reason/2]).
-export([set_event/2]).
-export([set_description/2]).
-export([set_call_id/2]).
-export([set_sub_account_id/2]).
-export([is_reason/2]).
-export([to_json/1]).
-export([from_json/1]).
-export([remove/1]).
-export([save/1]).

-record(wh_transaction, {id :: binary()
                         ,rev :: api_binary()
                         ,description :: api_binary()
                         ,call_id :: api_binary()
                         ,sub_account_id :: ne_binary()
                         ,event :: api_binary()
                         ,pvt_reason :: ne_binary()
                         ,pvt_code :: non_neg_integer()
                         ,pvt_amount :: non_neg_integer()
                         ,pvt_type :: ne_binary()
                         ,pvt_created :: wh_now()
                         ,pvt_modified :: wh_now()
                         ,pvt_account_id :: ne_binary()
                         ,pvt_account_db :: ne_binary()
                         ,pvt_vsn = 2 :: integer()
                        }).

-type transaction() :: #wh_transaction{}.
-export_type([transaction/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
description(#wh_transaction{description=Description}) ->
    Description.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
call_id(#wh_transaction{call_id=CallId}) ->
    CallId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
sub_account_id(#wh_transaction{sub_account_id=SubAccountId}) ->
    SubAccountId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
event(#wh_transaction{event=Event}) ->
    Event.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
reason(#wh_transaction{pvt_reason=Reason}) ->
    Reason.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
code(#wh_transaction{pvt_code=Code}) ->
    Code.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
amount(#wh_transaction{pvt_amount=Amount}) ->
    Amount.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
type(#wh_transaction{pvt_type=Type}) ->
    Type.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
created(#wh_transaction{pvt_created='undefined'}) ->
    wh_util:current_tstamp();
created(#wh_transaction{pvt_created=Created}) ->
    Created.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
modified(#wh_transaction{pvt_modified='undefined'}) ->
    wh_util:current_tstamp();
modified(#wh_transaction{pvt_modified=Modified}) ->
    Modified.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
account_id(#wh_transaction{pvt_account_id=AccountId}) ->
    AccountId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
account_db(#wh_transaction{pvt_account_db=AccountDb}) ->
    AccountDb.

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
%% Set a restricted reason
%% @end
%%--------------------------------------------------------------------
set_reason(Reason, #wh_transaction{}=Transaction) ->
    Code = wht_util:reason_code(Reason),
    Transaction#wh_transaction{pvt_reason=Reason
                               ,pvt_code=Code
                              }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set an event type that spawned the creation of this transaction
%% @end
%%--------------------------------------------------------------------
set_event(Event, #wh_transaction{}=Transaction) ->
    Transaction#wh_transaction{event=Event}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set free form description
%% @end
%%--------------------------------------------------------------------
-spec set_description(ne_binary(), transaction()) -> transaction().
set_description(Desc, #wh_transaction{}=Transaction) when is_binary(Desc) ->
    Transaction#wh_transaction{description=Desc}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set sub account ID
%% @end
%%--------------------------------------------------------------------
-spec set_sub_account_id(ne_binary(), transaction()) -> transaction().
set_sub_account_id(AccountId, #wh_transaction{}=Transaction) when is_binary(AccountId) ->
    Transaction#wh_transaction{sub_account_id=AccountId}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set Call-Id
%% @end
%%--------------------------------------------------------------------
-spec set_call_id(ne_binary(), transaction()) -> transaction().
set_call_id(CallId, #wh_transaction{}=Transaction) when is_binary(CallId) ->
    Transaction#wh_transaction{call_id=CallId}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_reason(ne_binary() | ne_binaries(), transaction()) -> boolean().
is_reason(Reason, #wh_transaction{pvt_reason=Reason}) -> true;
is_reason([Reason | _], #wh_transaction{pvt_reason=Reason}) -> true;
is_reason([_ | Reasons], #wh_transaction{}=Transaction) ->
    is_reason(Reasons, Transaction);
is_reason([], #wh_transaction{}) -> false;
is_reason(_, _) -> false.

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
             ,{<<"event">>, T#wh_transaction.event}
             ,{<<"pvt_reason">>, T#wh_transaction.pvt_reason}
             ,{<<"pvt_code">>, T#wh_transaction.pvt_code}
             ,{<<"pvt_amount">>, T#wh_transaction.pvt_amount}
             ,{<<"pvt_type">>, T#wh_transaction.pvt_type}
             ,{<<"pvt_created">>, T#wh_transaction.pvt_created}
             ,{<<"pvt_modified">>, T#wh_transaction.pvt_modified}
             ,{<<"pvt_account_id">>, T#wh_transaction.pvt_account_id}
             ,{<<"pvt_account_db">>, T#wh_transaction.pvt_account_db}
             ,{<<"pvt_vsn">>, T#wh_transaction.pvt_vsn}
            ],
    wh_json:from_list(props:filter_undefined(Props)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Transform Json Object to transaction record
%% @end
%%--------------------------------------------------------------------
-spec from_json/1 :: (wh_json:object()) -> transaction().
from_json(JObj) ->
    #wh_transaction{id = wh_json:get_ne_value(<<"_id">>, JObj)
                    ,rev = wh_json:get_ne_value(<<"_rev">>, JObj)
                    ,description = wh_json:get_ne_value(<<"description">>, JObj)
                    ,call_id = wh_json:get_ne_value(<<"call_id">>, JObj)
                    ,sub_account_id = wh_json:get_ne_value(<<"sub_account_id">>, JObj)
                    ,event = wh_json:get_ne_value(<<"event">>, JObj)
                    ,pvt_reason = wh_json:get_ne_value(<<"pvt_reason">>, JObj)
                    ,pvt_code = wh_json:get_integer_value(<<"pvt_code">>, JObj, 0)
                    ,pvt_amount = wh_json:get_integer_value(<<"pvt_amount">>, JObj, 0)
                    ,pvt_type = wh_json:get_ne_value(<<"pvt_type">>, JObj)
                    ,pvt_created = wh_json:get_ne_value(<<"pvt_created">>, JObj)
                    ,pvt_modified = wh_json:get_ne_value(<<"pvt_modified">>, JObj)
                    ,pvt_account_id = wh_json:get_ne_value(<<"pvt_account_id">>, JObj)
                    ,pvt_account_db = wh_json:get_ne_value(<<"pvt_account_db">>, JObj)
                    ,pvt_vsn = wh_json:get_integer_value(<<"pvt_vsn">>, JObj, 1)
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
save_transaction(#wh_transaction{pvt_amount=0}=Transaction) ->
    {'ok', Transaction};
save_transaction(#wh_transaction{pvt_account_db=AccountDb}=Transaction) ->
    JObj = to_json(Transaction#wh_transaction{pvt_modified=wh_util:current_tstamp()}),
    case couch_mgr:save_doc(AccountDb, JObj) of
        {'ok', J} -> {'ok', from_json(J)};
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
prepare_transaction(#wh_transaction{pvt_account_id='undefined'}) ->
    {'error', 'account_id_missing'};
prepare_transaction(#wh_transaction{pvt_account_db='undefined'}) ->
    {'error', 'account_id_missing'};
prepare_transaction(#wh_transaction{pvt_code=Code}=Transaction) when 1000 =< Code andalso Code < 2000 ->
    prepare_call_transaction(Transaction);
prepare_transaction(Transaction) ->
    Transaction.

prepare_call_transaction(#wh_transaction{call_id='undefined'}) ->
    {'error', 'call_id_missing'};
prepare_call_transaction(#wh_transaction{sub_account_id='undefined', pvt_code=1002}) ->
    {'error', 'sub_account_id_missing'};
prepare_call_transaction(#wh_transaction{event='undefined'}) ->
    {'error', 'event_missing'};
prepare_call_transaction(#wh_transaction{call_id=CallId, event=Event}=Transaction) ->
    Transaction#wh_transaction{id = <<CallId/binary, "-"
                                      ,(wh_util:to_upper_binary(Event))/binary>>
                                   ,event=wh_util:to_lower_binary(Event)}.

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
                    ,pvt_account_id=wh_util:format_account_id(Ledger, raw)
                    ,pvt_account_db=wh_util:format_account_id(Ledger, encoded)
                    ,pvt_created=wh_util:current_tstamp()
                    ,pvt_modified=wh_util:current_tstamp()
                   }.
