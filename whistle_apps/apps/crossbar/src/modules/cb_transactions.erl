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

-include("src/crossbar.hrl").

%% 1 month
-define(FETCH_DEFAULT, 60*60*24*30).
%% 1 year
-define(FETCH_MAX, 60*60*24*30*12).


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
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.transactions">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.transactions">>, ?MODULE, 'resource_exists'),
    crossbar_bindings:bind(<<"v1_resource.validate.transactions">>, ?MODULE, 'validate').

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
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

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
    From = wh_json:get_integer_value(<<"created_from">>, Query, 0),
    To = wh_json:get_integer_value(<<"created_to">>, Query, 0),
    Reason = wh_json:get_value(<<"reason">>, Query, 'undefined'),
    case Reason of
        <<"no_call">> ->
            Reasons = wht_util:reasons(2000),
            fetch(From, To, Context, Reasons);
        _ ->
            fetch(From, To, Context)    
    end.

validate(#cb_context{req_verb = <<"get">>, account_id=AccountId}=Context, <<"current_balance">>) ->
    Balance = wht_util:units_to_dollars(wht_util:current_balance(AccountId)),
    JObj = wh_json:from_list([{<<"balance">>, Balance}]),
    Context#cb_context{resp_status=success, resp_data=JObj};
validate(Context, _) ->
    cb_context:add_system_error('bad_identifier',  Context).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec fetch/3 :: (integer(), integer(), #cb_context{}) -> #cb_context{}.
-spec fetch/4 :: (integer(), integer(), #cb_context{}, ne_binary()) -> #cb_context{}.
fetch(From, To, Context) ->
    case validate_date(From, To) of
        {'true', VFrom, VTo} ->
            filter(VFrom, VTo, Context);
        {'false', R} ->
            cb_context:add_validation_error(<<"created_from/created_to">>
                                                ,<<"date_range">>
                                                ,R
                                            ,Context
                                           )
    end.
fetch(From, To, Context, Reason) ->
    case validate_date(From, To) of
        {'true', VFrom, VTo} ->
            filter(VFrom, VTo, Context, Reason);
        {'false', R} ->
            cb_context:add_validation_error(<<"created_from/created_to">>
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
-spec filter/3 :: (integer(), integer(), cb_context:context()) -> cb_context:context().
-spec filter/4 :: (integer(), integer(), #cb_context{}, ne_binary())  -> #cb_context{}.
filter(From, To, #cb_context{account_id=AccountId}=Context) ->
    try wh_transactions:fetch_since(AccountId, From, To) of
        Transactions ->
            send_resp({'ok', Transactions}, Context)
    catch
        _:_ ->
            send_resp({'error', Context}, Context)
    end.
filter(From, To, #cb_context{account_id=AccountId}=Context, Reason) ->
    try wh_transactions:fetch_since(AccountId, From, To) of
        Transactions ->
            Filtered = wh_transactions:filter_by_reason(Reason, Transactions),
            send_resp({'ok', Filtered}, Context)
    catch
        _:_ ->
            send_resp({'error', Context}, Context)
    end.

%%--------------------------------------------------------------------
%% @private 
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
send_resp(Resp, Context) ->
    case Resp of 
        {'ok', Transactions} ->
            JObj = wh_transactions:to_public_json(Transactions),
            Context#cb_context{resp_status=success
                               ,resp_data=wht_util:collapse_call_transactions(JObj)
                              };
        {'error', C} ->
            cb_context:add_system_error('bad_identifier', [{'details',<<"something went wrong while fetching the transaction">>}], C)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec validate_date/2 :: (any(), any()) -> {true, integer(), integer()} | {false, ne_binary()}.
validate_date(0, 0) ->
    validate_date(wh_util:current_tstamp()-?FETCH_DEFAULT, wh_util:current_tstamp());
validate_date(0, To) ->
    validate_date(To-?FETCH_DEFAULT, To);
validate_date(From, 0) ->
    validate_date(From, From+?FETCH_DEFAULT);
validate_date(From, To) when is_integer(From) andalso is_integer(To) ->
    Max = ?FETCH_MAX,
    Diff = To - From,
    case {Diff < 0, Diff > Max} of
        {'true', _} ->
            {'false', <<"created_from is gretter than created_to">>};
        {_, 'true'} ->                    
            {'false', <<"Max range is a year">>};
        {'false', 'false'} ->
            {'true', From, To}
    end;
validate_date(From, To) ->
    try {wh_util:to_integer(From), wh_util:to_integer(To)} of
        {From1, To1} ->
            validate_date(From1, To1)
    catch
        _:_ ->            
            {'false', <<"created_from or created_to filter is not a timestamp">>}
    end.
