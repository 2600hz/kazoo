%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(ci_filter_req).

-export([handle_req/2]).

-include("call_inspector.hrl").

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_inspector:filter_req_v(JObj),
    CallIds = [CallId
               || CallId <- wh_json:get_value(<<"Call-IDs">>, JObj, [])
                      ,ci_datastore:callid_exists(CallId)
              ],
    Q = wh_json:get_value(<<"Server-ID">>, JObj),
    MessageId = wh_json:get_value(<<"Msg-ID">>, JObj),
    send_response(CallIds, Q, MessageId).

-spec send_response(ne_binaries(), ne_binary(), ne_binary()) -> 'ok'.
send_response(CallIds, Q, MessageId) ->
    io:format("TEST: ~p~n", [CallIds]),
    JObj = wh_json:from_list(
             [{<<"Call-IDs">>, CallIds}
             ,{<<"Msg-ID">>, MessageId}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]
            ),
    wapi_inspector:publish_filter_resp(Q, JObj).
