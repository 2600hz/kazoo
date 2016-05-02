%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(ci_filter_req).

-export([handle_req/2]).

-include("call_inspector.hrl").

-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_inspector:filter_req_v(JObj),
    CallIds = [CallId
               || CallId <- kz_json:get_value(<<"Call-IDs">>, JObj, []),
                  ci_datastore:callid_exists(CallId)
              ],
    Q = kz_json:get_value(<<"Server-ID">>, JObj),
    MessageId = kz_json:get_value(<<"Msg-ID">>, JObj),
    send_response(CallIds, Q, MessageId).

-spec send_response(ne_binaries(), ne_binary(), ne_binary()) -> 'ok'.
send_response(CallIds, Q, MessageId) ->
    JObj = kz_json:from_list(
             [{<<"Call-IDs">>, CallIds}
              ,{<<"Msg-ID">>, MessageId}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]
            ),
    kapi_inspector:publish_filter_resp(Q, JObj).
