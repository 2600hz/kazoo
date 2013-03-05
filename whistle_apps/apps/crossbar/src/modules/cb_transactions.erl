%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors:
%%%     Peter Defebvre
%%%-------------------------------------------------------------------
-module(cb_transactions).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
        ]).

-include("include/crossbar.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init/0 :: () -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.transactions">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.transactions">>, ?MODULE, resource_exists),
    crossbar_bindings:bind(<<"v1_resource.validate.transactions">>, ?MODULE, validate).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods() | [].
-spec allowed_methods/1 :: (path_token()) -> http_methods() | [].
allowed_methods() ->
    ['GET'].
allowed_methods(_) -> 
    ['GET'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /transactions => []
%%    /transactions/foo => [<<"foo">>]
%%    /transactions/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
resource_exists() -> true.
resource_exists(_) -> true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /transactions mights load a list of transactions objects
%% /transactions/123 might load the transactions object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>,  query_json=Query}=Context) ->
    From = wh_json:get_value(<<"created_from">>, Query, undefined),
    Reason = wh_json:get_value(<<"reason">>, Query, undefined),
    case Reason of
        <<"no_call">> ->
            Reasons = wh_transaction:get_reasons(false),
            fetch_by_date_and_reason(From, Reasons, Context);
        _ ->
            fetch_by_date(From, Context)    
    end.

validate(#cb_context{req_verb = <<"get">>, account_id=AccountId}=Context, <<"current_balance">>) ->
    Balance = wh_transactions:get_current_balance(AccountId),
    JObj = wh_json:from_list([{<<"balance">>, Balance}]),
    Context#cb_context{resp_status=success, resp_data=JObj};
validate(#cb_context{req_verb = <<"get">>, account_id=AccountId}=Context, TransactionId) ->
    try wh_transaction:fetch(AccountId, TransactionId) of
        Transaction ->
            JObj = clean_json_obj(wh_transaction:to_json(Transaction)),
            Context#cb_context{resp_status=success, resp_data=JObj}
    catch
        _:_ ->
            cb_context:add_system_error(bad_identifier, [{details, <<"Unknow transaction ID">>}], Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec fetch_by_date/2 :: (integer(), #cb_context{}) -> #cb_context{}.
fetch_by_date(From, Context) ->
    case validate_date(From) of
        {true, Date} ->
            Resp = fetch_since(Date, Context),
            send_resp(Resp, Context);
        {false, undefined} ->
            Month = (wh_util:current_tstamp() - 60*60*24*30),
            Resp = fetch_since(Month, Context),
            send_resp(Resp, Context);
        {false, R} ->
            cb_context:add_validation_error(<<"created_from">>
                                                ,<<"date_range">>
                                                ,R
                                            ,Context
                                           )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec fetch_by_date_and_reason/3 :: (integer(), ne_binary(), #cb_context{}) -> #cb_context{}.
fetch_by_date_and_reason(From, Reason, Context) ->
    case validate_date(From) of
        {true, Date} ->
            filter_by_date_reason(Date, Reason, Context);
        {false, undefined} ->
            Month = (wh_util:current_tstamp() - 60*60*24*30),
            filter_by_date_reason(Month, Reason, Context);
        {false, R} ->
            cb_context:add_validation_error(<<"created_from">>
                                                ,<<"date_range">>
                                                ,R
                                            ,Context
                                           )
    end.
        
%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec fetch_since/2 :: (integer(), cb_context:context()) -> cb_context:context().
fetch_since(Date, #cb_context{account_id=AccountId}=Context) ->
    try wh_transactions:fetch_since(AccountId, Date) of
        Transactions ->
            {ok, Transactions}
    catch
        _:_ ->
            {error, Context}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec filter_by_date_reason/3 :: (integer(), ne_binary(), #cb_context{}) -> #cb_context{}.
filter_by_date_reason(Date, Reason, Context) ->
    case fetch_since(Date, Context) of
        {ok, Transactions}  ->
            Filtered = wh_transactions:filter_by_reason(Reason, Transactions),
            send_resp({ok, Filtered}, Context);
        Error ->
            send_resp(Error, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
send_resp(Resp, Context) ->
    case Resp of 
        {ok, Transactions} ->
            JObj = transactions_to_jobj(Transactions),
            Context#cb_context{resp_status=success, resp_data=JObj};
        {error, C} ->
            cb_context:add_system_error(bad_identifier, [{details,<<"Unknow transaction ID">>}], C)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec validate_date/1 :: (any()) -> {true, integer()} | {false, 0} | {false, ne_binary()}.
validate_date(undefined) ->
    {false, undefined};
validate_date(Date) when is_integer(Date) ->
    Now = wh_util:current_tstamp(),
    Max = 60*60*24*30*12,
    Diff = Now - Date,
    case {Diff < 0, Diff > Max} of
        {true, _} ->
            {false, <<"created_from is gretter than current timestamp">>};
        {_, true} ->                    
            {false, <<"Max range is a year from now">>};
        {false, false} ->
            {true, Date}
    end;
validate_date(Date) ->
    try wh_util:to_integer(Date) of
        Date1 ->
            validate_date(Date1)
    catch
        _:_ ->            
            {false, <<"created_from filter is not a timestamp">>}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec transactions_to_jobj/1 :: (wh_transaction:wh_transactions()) -> [wh_json:object(), ...].
transactions_to_jobj(Transactions) ->
    JObj = [wh_transaction:to_json(Tr) ||  Tr <- Transactions],
    [clean_json_obj(Obj) ||  Obj <- JObj].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec clean_json_obj/1 :: (wh_json:object()) -> wh_json:object().
clean_json_obj(JObj) ->
    CleanKeys = [{<<"_id">>, <<"id">>}
                 ,{<<"pvt_amount">>, <<"amount">>}
                 ,{<<"pvt_reason">>, <<"reason">>}
                 ,{<<"pvt_type">>, <<"type">>}
                 ,{<<"pvt_created">>, <<"created">>}
                 ,{<<"pvt_vsn">>, <<"version">>}
                ],
    RemoveKeys = [<<"pvt_account_db">>
                  ,<<"pvt_account_id">>
                  ,<<"pvt_modified">>
                 ],
    CleanJObj = clean(CleanKeys, JObj),
    wh_json:delete_keys(RemoveKeys, CleanJObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec clean/2 :: ([{ne_binary(), ne_binary()}, ...] ,wh_json:object()) -> wh_json:object().
clean([], JObj) ->
    JObj;
clean([{OldKey, NewKey} | T], JObj) ->
    Value = wh_json:get_value(OldKey, JObj),
    J1 = wh_json:set_value(NewKey, Value, JObj),
    clean(T, wh_json:delete_key(OldKey, J1)).
