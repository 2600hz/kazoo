%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(ci_filter_req).

-export([handle_req/2]).

-include("call_inspector.hrl").

-spec handle_req(kz_json:object(), kz_proplist()) -> ok.
handle_req(JObj, _Props) ->
    true = kapi_inspector:filter_req_v(JObj),
    CallIds = [CallId
               || CallId <- kz_json:get_value(<<"Call-IDs">>, JObj, []),
                  ci_datastore:callid_exists(CallId)
              ],
    send_response(CallIds, kz_api:server_id(JObj), kz_api:msg_id(JObj)).

-spec send_response(ne_binaries(), ne_binary(), ne_binary()) -> ok.
send_response(CallIds, Q, MessageId) ->
    JObj = kz_json:from_list(
             [{<<"Call-IDs">>, CallIds}
             ,{<<"Msg-ID">>, MessageId}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]
            ),
    kapi_inspector:publish_filter_resp(Q, JObj).
