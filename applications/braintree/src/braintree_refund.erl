%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(braintree_refund).

-export([handle_req/2]).

-include("braintree.hrl").

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_bookkeepers:refund_req_v(JObj),
    case kz_json:get_ne_binary_value(<<"Bookkeeper-Type">>, JObj) =:= <<"braintree">> of
        'false' -> 'ok';
        'true' -> validate_request(JObj)
    end.

-spec validate_request(kz_json:object()) -> 'ok'.
validate_request(JObj) ->
    _VendorId = kz_json:get_ne_binary_value(<<"Vendor-ID">>, JObj),
    AccountId = kz_json:get_ne_binary_value(<<"Account-ID">>, JObj),
    DollarAmount =
        kz_currency:units_to_dollars(
          kz_json:get_integer_value(<<"Amount">>, JObj, 0)
         ),
    Result = do_request(AccountId, DollarAmount),
    Resp =
        kz_json:from_list(
          [{?KEY_MSG_ID, kz_api:msg_id(JObj)}
          ,{<<"Transaction-ID">>, kz_json:get_ne_binary_value(<<"Transaction-ID">>, JObj)}
          ,{<<"Transaction-DB">>, kz_json:get_ne_binary_value(<<"Transaction-DB">>, JObj)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ] ++ Result
         ),
    ServerId = kz_api:server_id(JObj),
    Publisher = fun(P) -> kapi_bookkeepers:publish_refund_resp(ServerId, P) end,
    kz_amqp_worker:cast(Resp, Publisher).

-spec do_request(kz_term:ne_binary(), kz_currency:dollars()) -> kz_term:proplist().
do_request(AccountId, DollarAmount) ->
    try braintree_transaction:quick_credit(AccountId, DollarAmount) of
        Transaction ->
            [{<<"Status">>, <<"success">>}
            ,{<<"Details">>, braintree_transaction:record_to_json(Transaction)}
            ]
    catch
        'throw':Error ->
            braintree_util:error_to_props(Error)
    end.
