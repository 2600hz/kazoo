%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(braintree_collect_recurring_req).

-export([handle_req/2]).

-include("braintree.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_bookkeepers:collect_recurring_req_v(JObj),
    Response = kz_json:from_list(
                 [{<<"Account-ID">>, kz_json:get_ne_binary_value(<<"Account-ID">>, JObj)}
                 ,{<<"Bookkeeper-ID">>, kz_json:get_ne_binary_value(<<"Bookkeeper-ID">>, JObj)}
                 ,{<<"Bookkeeper-Type">>, kz_json:get_ne_binary_value(<<"Bookkeeper-Type">>, JObj)}
                 ,{<<"Message">>, <<"Braintree performs recurring charges automatically">>}
                 ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
                 ,{<<"Status">>, kz_services_recurring:status_good()}
                 ] ++ kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ),
    RespQ = kz_json:get_ne_binary_value(<<"Server-ID">>, JObj),

    PublishFun = fun(P) -> kapi_bookkeepers:publish_collect_recurring_resp(RespQ, P) end,

    case kz_json:get_ne_binary_value(<<"Bookkeeper-Type">>, JObj) =:= ?APP_NAME of
        'false' ->
            lager:debug("skipping collect recurring for another bookkeeper");
        'true' ->
            kz_amqp_worker:cast(Response, PublishFun)
    end.
