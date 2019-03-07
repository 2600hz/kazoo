%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(braintree_standing_req).

-export([handle_req/2]).

-include("braintree.hrl").

-record(request, {account_id :: kz_term:ne_binary()
                 ,request_jobj :: kz_json:object()
                 ,customer :: braintree_customer:customer()
                 ,card :: bt_card()
                 }
       ).

-type request() :: #request{}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type routine_return() :: {'ok', request()} | 'ok'.
-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_bookkeepers:standing_req_v(JObj),
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    lager:debug("received service standing check for ~s", [AccountId]),
    case kz_json:get_value(<<"Bookkeeper-Type">>, JObj) =:= ?APP_NAME of
        'false' ->
            lager:debug("skipping service standing check for another bookkeeper");
        'true' ->
            Request = #request{account_id = AccountId
                              ,request_jobj = JObj
                              },
            Routines = [fun find_braintree_customer/1
                       ,fun find_payment_token/1
                       ,fun check_subscriptions/1
                       ],
            check_braintree(Request, Routines)
    end.

-spec check_braintree(request(), any()) -> 'ok'.
check_braintree(Request, []) ->
    reply_good_standing(Request);
check_braintree(Request, [Routine|Routines]) ->
    case Routine(Request) of
        {'ok', UpdatedRequest} ->
            check_braintree(UpdatedRequest, Routines);
        _Else -> 'ok'
    end.

-spec find_braintree_customer(request()) -> routine_return().
find_braintree_customer(#request{account_id = AccountId} = Request) ->
    lager:debug("requesting braintree customer ~s", [AccountId]),
    try Request#request{customer = braintree_customer:find(AccountId)} of
        UpdatedRequest -> {'ok', UpdatedRequest}
    catch
        _:_ ->
            reply_missing_payment_token(Request)
    end.

-spec find_payment_token(request()) -> routine_return().
find_payment_token(#request{customer = #bt_customer{credit_cards = Cards}} = Request) ->
    lager:debug("requesting braintree default payment card"),
    try Request#request{card = braintree_card:default_payment_card(Cards)} of
        UpdatedRequest -> {'ok', UpdatedRequest}
    catch
        _:_ ->
            reply_missing_payment_token(Request)
    end.

-spec check_subscriptions(request()) -> routine_return().
check_subscriptions(#request{customer = Customer} = Request) ->
    Subscriptions = braintree_customer:get_subscriptions(Customer),
    case lists:any(fun braintree_subscription:is_past_due/1, Subscriptions) of
        'true' -> reply_past_due(Request);
        'false' -> {'ok', Request}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reply_good_standing(request()) -> 'ok'.
reply_good_standing(Request) ->
    Reply = [{<<"Status">>, kzd_services:status_good()}
            ,{<<"Message">>, <<"Credit card on file and all active subscriptions valid">>}
            ],
    reply(Request, Reply).
    
%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reply_missing_payment_token(request()) -> 'ok'.
reply_missing_payment_token(Request) ->
    Reply = [{<<"Status">>, <<"error">>}
            ,{<<"Message">>, <<"There is no credit card on file for this account">>}
            ,{<<"Reason">>, <<"no_payment_token">>}
            ],
    reply(Request, Reply).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reply_past_due(request()) -> 'ok'.
reply_past_due(Request) ->
    Reply = [{<<"Status">>, <<"error">>}
            ,{<<"Message">>, <<"Your account is delinquent, please contact your sales representative.">>}
            ,{<<"Reason">>, <<"delinquent">>}
            ],
    reply(Request, Reply).
        
%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reply(request(), kz_term:proplist()) -> 'ok'.
reply(#request{request_jobj=JObj}, Reply) ->
    MessageId = kz_json:get_value(<<"Msg-ID">>, JObj),
    Response = kz_json:from_list(
                 [{<<"Msg-ID">>, MessageId}
                  | Reply
                 ] ++ kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ),
    RespQ = kz_json:get_value(<<"Server-ID">>, JObj),
    kz_amqp_worker:cast(Response
                       ,fun(P) ->
                                kapi_bookkeepers:publish_standing_resp(RespQ, P)
                        end
                       ).
