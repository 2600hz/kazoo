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

-export([debit/2
         ,credit/2
        ]).

-export([set_description/2
         ,set_pvt_account_id/2
         ,set_sub_account_id/2
        ]).

-export([save/1
         ,fetch/2
         ,get_current_balance/1
        ]).

-export([to_json/1
         ,from_json/1
        ]).

-define(REASONS, [per_minute
                  ,activation_charges
                  ,admin
                 ]).

-record(wh_transaction, {
          id :: binary()
         ,description :: binary()
         ,sub_account_id :: ne_binary()
         ,pvt_reason :: ne_binary()
         ,pvt_amount :: integer()
         ,pvt_type :: 'credit' | 'debit'
         ,pvt_created :: wh_now()
         ,pvt_modified :: wh_now()
         ,pvt_account_id :: ne_binary()
         ,pvt_account_db :: ne_binary()
         ,pvt_vsn = 2 :: integer()
         }).

-type wh_transaction() :: #wh_transaction{}.
-type wh_transactions() :: [wh_transaction(), ...].
-export_type([wh_transaction/0
              ,wh_transactions/0
             ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Transform transaction record to Json object
%% @end
%%--------------------------------------------------------------------
-spec to_json/1 :: (wh_transaction()) -> wh_json:object(). 
to_json(#wh_transaction{}=T) ->
    wh_json:from_list([{<<"_id">>, T#wh_transaction.id}
                       ,{<<"description">>, T#wh_transaction.description}
                       ,{<<"sub_account_id">>, T#wh_transaction.sub_account_id}
                       ,{<<"pvt_reason">>, T#wh_transaction.pvt_reason}
                       ,{<<"pvt_amount">>, T#wh_transaction.pvt_amount}
                       ,{<<"pvt_type">>, T#wh_transaction.pvt_type}
                       ,{<<"pvt_created">>, T#wh_transaction.pvt_created}
                       ,{<<"pvt_modified">>, T#wh_transaction.pvt_modified}
                       ,{<<"pvt_account_id">>, T#wh_transaction.pvt_account_id}
                       ,{<<"pvt_account_db">>, T#wh_transaction.pvt_account_db}
                       ,{<<"pvt_vsn">>, T#wh_transaction.pvt_vsn}
                      ]).
    
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Transform Json Object to transaction record
%% @end
%%--------------------------------------------------------------------
-spec from_json/1 :: (wh_json:object()) -> wh_transaction(). 
from_json(JObj) ->
    #wh_transaction{
      id = wh_json:get_ne_value(<<"_id">>, JObj)
      ,description = wh_json:get_ne_value(<<"description">>, JObj)
      ,sub_account_id = wh_json:get_ne_value(<<"sub_account_id">>, JObj)
      ,pvt_reason = wh_json:get_ne_value(<<"pvt_reason">>, JObj)
      ,pvt_amount = wh_json:get_ne_value(<<"pvt_amount">>, JObj)
      ,pvt_type = wh_json:get_ne_value(<<"pvt_type">>, JObj)
      ,pvt_created = wh_json:get_ne_value(<<"pvt_created">>, JObj)
      ,pvt_modified = wh_json:get_ne_value(<<"pvt_modified">>, JObj)
      ,pvt_account_id = wh_json:get_ne_value(<<"pvt_account_id">>, JObj)
      ,pvt_account_db = wh_json:get_ne_value(<<"pvt_account_db">>, JObj)
      ,pvt_vsn = wh_json:get_ne_value(<<"pvt_vsn">>, JObj)
     }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create transaction record of type credit (with Amount & Reason)
%% @end
%%--------------------------------------------------------------------
-spec credit/2 :: (integer(), ne_binary()) -> wh_transaction(). 
credit(Amount, Reason) ->
    create(Amount, 'credit', Reason).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create transaction record of type debit (with Amount & Reason)
%% @end
%%--------------------------------------------------------------------
-spec debit/2 :: (integer(), ne_binary()) -> wh_transaction(). 
debit(Amount, Reason) ->
    create(Amount, 'debit', Reason).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set free form description
%% @end
%%--------------------------------------------------------------------
-spec set_description/2 :: (ne_binary(), wh_transaction()) -> wh_transaction(). 
set_description(Desc, Transaction) ->
    Transaction#wh_transaction{description=Desc}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set private account ID
%% @end
%%--------------------------------------------------------------------
-spec set_pvt_account_id/2 :: (ne_binary(), wh_transaction()) -> wh_transaction(). 
set_pvt_account_id(AccountId, Transaction) ->
    Transaction#wh_transaction{pvt_account_id=AccountId}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set sub account ID
%% @end
%%--------------------------------------------------------------------
-spec set_sub_account_id/2 :: (ne_binary(), wh_transaction()) -> wh_transaction(). 
set_sub_account_id(AccountId, Transaction) ->
    Transaction#wh_transaction{sub_account_id=AccountId}.


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
-spec save/1 :: (wh_transaction()) -> wh_transaction(). 
save(#wh_transaction{pvt_account_id=AccountId}=Transaction) ->
    Transaction1 = set_private_properties(Transaction),
    case validate(Transaction1) of
        {true, Transaction2, _} ->
            AccountDB = wh_util:format_account_id(AccountId, encoded),
            case couch_mgr:save_doc(AccountDB, to_json(Transaction2)) of
                {ok, T} ->
                    from_json(T);
                {error, R} ->
                    {error, R}
            end;
        {false, Tr, R} ->
            {error, R, Tr}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetch a transaction from the database
%% @end
%%--------------------------------------------------------------------
-spec fetch/2 :: (ne_binary(), ne_binary()) -> wh_transaction(). 
fetch(AccountId, Id) ->    
    AccountDB = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_doc(AccountDB, Id) of
        {ok, T} ->
            from_json(T)
    end.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetch a transaction from the database
%% @end
%%--------------------------------------------------------------------
-spec get_current_balance/1 :: (ne_binary()) -> integer(). 
get_current_balance(AccountId) ->    
    AccountDB = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:get_results(AccountDB, <<"transactions/credit_remaining">>, []) of
        {ok, []} -> 
            lager:debug("no current balance for ~s", [AccountId]),
            0;
        {ok, [ViewRes|_]} -> 
            wh_json:get_integer_value(<<"value">>, ViewRes, 0);
        {error, _R} -> 
            lager:warning("unable to get current balance for ~s: ~p", [AccountId, _R]),
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
-spec create/3 :: (integer(), atom(), atom()) -> wh_transaction() | {'error', 'unknow_reason'}.
create(Amount, Op, Reason) when not is_integer(Amount) ->
    create(wh_util:to_integer(Amount), Op, Reason);
create(Amount, Op, Reason) when Amount < 0 ->
    create(Amount*-1, Op, Reason);
create(Amount, Op, Reason) ->
    case lists:member(Reason, ?REASONS) of
        true ->
            #wh_transaction{pvt_reason=Reason
                            ,pvt_type=Op
                            ,pvt_amount=Amount
                           };
        false ->
            {error, unknow_reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return Errors of record when trying to save
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (wh_transaction()) -> {true, wh_transaction(), undefined} | {false, wh_transaction(), list()}. 
validate(Transaction) ->
    {Tr, Errors} = validate_funs(Transaction),
    case Errors of
        [] ->
            {true, Tr, undefined};
        R ->
            {false, Tr, R}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Call all the different validate function
%% @end
%%--------------------------------------------------------------------
-spec validate_funs/1 :: (wh_transaction()) -> {wh_transaction(), list()}. 
validate_funs(#wh_transaction{}=Transaction) ->  
    Funs = [fun validate_reason/1
            ,fun validate_account_id/1
            ,fun validate_sub_account_id/1
           ],
    lists:foldl(fun(F, {Tr, Errs}) -> 
                        case F(Tr) of 
                            {ok, Tr1} -> 
                                {Tr1, Errs}; 
                            {error, Tr1, Err} -> 
                                {Tr1, [Err | Errs]} 
                        end 
                end,
                {Transaction, []}, Funs).
            
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the account ID
%% @end
%%--------------------------------------------------------------------
-spec validate_reason/1 :: (wh_transaction()) -> {ok, wh_transaction()} | {error, wh_transaction(), unknow_reason}. 
validate_reason(#wh_transaction{pvt_reason=Reason}=Tr) ->
    case lists:member(Reason, ?REASONS) of
        true -> {ok, Tr};
        false -> {error, Tr, unknow_reason}
    end.
         
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the account ID
%% @end
%%--------------------------------------------------------------------
-spec validate_account_id/1 :: (wh_transaction()) -> {ok, wh_transaction()} | {error, wh_transaction(), account_id}. 
validate_account_id(#wh_transaction{pvt_account_id=AccountId}=Tr) ->
    case is_binary(AccountId) of
        true -> {ok, Tr};
        false -> {error, Tr, account_id}
    end.
            
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the account ID
%% @end
%%--------------------------------------------------------------------
-spec validate_sub_account_id/1 :: (wh_transaction()) -> {ok, wh_transaction()} | {error, wh_transaction(), sub_account_id}. 
validate_sub_account_id(#wh_transaction{sub_account_id=SubAccountId, pvt_account_id=AccountId}=Tr) ->
    case SubAccountId =:= undefined of
        true -> 
            {ok, Tr#wh_transaction{sub_account_id=AccountId}};
        false -> 
            case is_binary(SubAccountId) of
                true -> {ok, Tr};
                false -> {error, Tr, sub_account_id}
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Call functions to set private properties 
%% @end
%%--------------------------------------------------------------------    
-spec set_private_properties/1 :: (wh_transaction()) -> wh_transaction(). 
set_private_properties(Transaction) ->
    PvtFuns = [fun set_pvt_created/1
               ,fun set_pvt_modified/1
               ,fun set_pvt_account_db/1
              ],
    lists:foldl(fun(F, C) -> F(C) end, Transaction, PvtFuns).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Set creation date
%% @end
%%--------------------------------------------------------------------
-spec set_pvt_created/1 :: (wh_transaction()) -> wh_transaction(). 
set_pvt_created(Transaction) ->
    Transaction#wh_transaction{pvt_created=wh_util:current_tstamp()}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Set modification date
%% @end
%%--------------------------------------------------------------------
-spec set_pvt_modified/1 :: (wh_transaction()) -> wh_transaction(). 
set_pvt_modified(Transaction) ->
    Transaction#wh_transaction{pvt_modified=wh_util:current_tstamp()}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Set private account DB
%% @end
%%--------------------------------------------------------------------
-spec set_pvt_account_db/1 :: (wh_transaction()) -> wh_transaction(). 
set_pvt_account_db(#wh_transaction{pvt_account_id=AccountId}=Transaction) ->
    AccountDB = wh_util:format_account_id(AccountId, encoded),
    Transaction#wh_transaction{pvt_account_db=AccountDB}.
