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
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
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
-spec allowed_methods/2 :: (path_token(), path_token()) -> http_methods() | [].
allowed_methods() ->
    ['GET'].
allowed_methods(_) -> 
    ['GET'].
allowed_methods(_, _) -> 
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
-spec resource_exists/2 :: (path_token(), path_token()) -> 'true'.
resource_exists() -> true.
resource_exists(_) -> true.
resource_exists(_, _) -> true.

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
-spec validate/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>, account_id=AccountId}=Context) ->
    Month = (wh_util:current_tstamp() - 60*60*24*30),
    Transactions = wh_transactions:fetch_since(AccountId, Month),
    JObj = transactions_to_jobj(Transactions),
    Context#cb_context{resp_status=success, resp_data=JObj}.

validate(#cb_context{req_verb = <<"get">>, account_id=AccountId}=Context, <<"current_balance">>) ->
    Balance = wh_transaction:get_current_balance(AccountId),
    JObj = wh_json:from_list([{<<"balance">>, Balance}]),
    Context#cb_context{resp_status=success, resp_data=JObj};
validate(#cb_context{req_verb = <<"get">>, account_id=AccountId}=Context, TransactionId) ->
    Transaction = wh_transaction:fetch(AccountId, TransactionId),
    JObj = clean_json_obj(wh_transaction:to_json(Transaction)),
    Context#cb_context{resp_status=success, resp_data=JObj}.

validate(#cb_context{req_verb = <<"get">>, account_id=AccountId}=Context, <<"from">>, Date) ->
    Context#cb_context{resp_status=success}.

transactions_to_jobj(Transactions) ->
    JObj = [wh_transaction:to_json(Tr) ||  Tr <- Transactions],
    [clean_json_obj(Obj) ||  Obj <- JObj].

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

clean([], JObj) ->
    JObj;
clean([{OldKey, NewKey} | T], JObj) ->
    Value = wh_json:get_value(OldKey, JObj),
    J1 = wh_json:set_value(NewKey, Value, JObj),
    clean(T, wh_json:delete_key(OldKey, J1)).

