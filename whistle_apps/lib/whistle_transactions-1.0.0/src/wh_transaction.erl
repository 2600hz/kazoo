%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------

-module(wh_transaction).

-include_lib("whistle/include/wh_types.hrl").

-export([debit/2
         ,credit/2
        ]).

-export([set_description/2
         ,set_pvt_account_id/2
        ]).

-export([save/1
         ,fetch/2
         ,get_current_balance/1
        ]).

-export([to_json/1
         ,from_json/1
        ]).

-record(wh_transaction, {
          id :: binary()
         ,description :: binary()
         ,pvt_reason :: ne_binary()
         ,pvt_amount :: integer()
         ,pvt_type :: 'credit' | 'debit'
         ,pvt_created :: wh_now()
         ,pvt_account_id :: ne_binary()
         }).

-type wh_transaction() :: #wh_transaction{}.
-export_type([wh_transaction/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Transform transaction record to Json object
%% @end
%%--------------------------------------------------------------------
to_json(#wh_transaction{}=T) ->
    wh_json:from_list([{<<"_id">>, T#wh_transaction.id}
                       ,{<<"description">>, T#wh_transaction.description}
                       ,{<<"pvt_reason">>, T#wh_transaction.pvt_reason}
                       ,{<<"pvt_amount">>, T#wh_transaction.pvt_amount}
                       ,{<<"pvt_type">>, T#wh_transaction.pvt_type}
                       ,{<<"pvt_created">>, T#wh_transaction.pvt_created}
                       ,{<<"pvt_account_id">>, T#wh_transaction.pvt_account_id}
                      ]).
    
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Transform Json Object to transaction record
%% @end
%%--------------------------------------------------------------------
from_json(JObj) ->
    #wh_transaction{
      id = wh_json:get_ne_value(<<"_id">>, JObj)
      ,description = wh_json:get_ne_value(<<"description">>, JObj)
      ,pvt_reason = wh_json:get_ne_value(<<"pvt_reason">>, JObj)
      ,pvt_amount = wh_json:get_ne_value(<<"pvt_amount">>, JObj)
      ,pvt_type = wh_json:get_ne_value(<<"pvt_type">>, JObj)
      ,pvt_created = wh_json:get_ne_value(<<"pvt_created">>, JObj)
      ,pvt_account_id = wh_json:get_ne_value(<<"pvt_account_id">>, JObj)
     }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create transaction record of type credit (with Amount & Reason)
%% @end
%%--------------------------------------------------------------------
credit(Amount, Reason) ->
    create(Amount, 'credit', Reason).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create transaction record of type debit (with Amount & Reason)
%% @end
%%--------------------------------------------------------------------
debit(Amount, Reason) ->
    create(Amount, 'debit', Reason).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set free form description
%% @end
%%--------------------------------------------------------------------
set_description(Desc, Transaction) ->
    Transaction#wh_transaction{description=Desc}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set private account ID
%% @end
%%--------------------------------------------------------------------
set_pvt_account_id(AccountId, Transaction) ->
    Transaction#wh_transaction{pvt_account_id=AccountId}.

%%--------------------------------------------------------------------
%%
%% DATABASE FUNCTIONS
%%
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save transaction to database
%% @end
%%--------------------------------------------------------------------
save(#wh_transaction{pvt_account_id=AccountId}=Transaction) ->
    Transaction1 = set_private_properties(Transaction),
    Errors = validate_funs(Transaction1),
    case validate(Errors) of
        {true, _} ->
            AccountDB = wh_util:format_account_id(AccountId, encoded),
            case couch_mgr:save_doc(AccountDB, to_json(Transaction1)) of
                {ok, T} ->
                    from_json(T);
                {error, R} ->
                    {error, R}
            end;
        {false, R} ->
            {error, R}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetch a transaction from the database
%% @end
%%--------------------------------------------------------------------
fetch(AccountId, Id) ->    
    AccountDB = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_doc(AccountDB, Id) of
        {ok, T} ->
            from_json(T);
        R ->
            R
    end.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetch a transaction from the database
%% @end
%%--------------------------------------------------------------------
get_current_balance(AccountId) ->    
    AccountDB = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:get_results(AccountDB, <<"transactions/credit_remaining">>, []) of
        {ok, []} -> 
            io:format("no current balance for ~s", [AccountId]),
            0;
        {ok, [ViewRes|_]} -> 
            wh_json:get_integer_value(<<"value">>, ViewRes, 0);
        {error, _R} -> 
            io:format("unable to get current balance for ~s: ~p", [AccountId, _R]),
            0
    end.
    
            

%%--------------------------------------------------------------------
%%
%% PRIVATE FUNCTIONS
%%
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create transaction record
%% @end
%%--------------------------------------------------------------------
create(Amount, Op, Reason) when not is_integer(Amount) ->
    create(wh_util:to_integer(Amount), Op, Reason);
create(Amount, Op, Reason) when Amount < 0 ->
    create(Amount*-1, Op, Reason);
create(Amount, Op, Reason) ->
    #wh_transaction{pvt_reason=Reason
                    ,pvt_type=Op
                    ,pvt_amount=Amount
                   }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return Errors of record when trying to save
%% @end
%%--------------------------------------------------------------------
validate(Errors) ->
    validate(Errors, true, []).

validate([], Valid, Acc) ->
    {Valid, Acc};
validate([ok | Errors], Valid, Acc) ->
    validate(Errors, Valid, Acc);
validate([{error, Reason} | Errors], _, Acc) ->
    validate(Errors, false, [Reason | Acc]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Call all the different validate function
%% @end
%%--------------------------------------------------------------------
validate_funs(#wh_transaction{}=Tr) ->  
    Funs = [fun validate_account_id/1],
    lists:foldl(fun(F, Acc) -> [F(Tr) | Acc] end, [], Funs).

            
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the account ID
%% @end
%%--------------------------------------------------------------------
validate_account_id(#wh_transaction{pvt_account_id=AccountId}) ->
    case is_binary(AccountId) of
        true -> ok;
        false -> {error, account_id}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Call functions to set private properties 
%% @end
%%--------------------------------------------------------------------    
set_private_properties(Transaction) ->
    PvtFuns = [fun set_pvt_created/1 ],
    lists:foldl(fun(F, C) -> F(C) end, Transaction, PvtFuns).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Set creation date
%% @end
%%--------------------------------------------------------------------
set_pvt_created(Transaction) ->
    Transaction#wh_transaction{pvt_created=wh_util:current_tstamp()}.

