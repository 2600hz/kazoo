%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(ci_lookup_req).

-export([handle_req/2]).

-include("call_inspector.hrl").

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_inspector:lookup_req_v(JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    case ci_datastore:lookup_callid(CallId) of
        [] -> 'ok';
        Props ->
            Q = wh_json:get_value(<<"Server-ID">>, JObj),
            MessageId = wh_json:get_value(<<"Msg-ID">>, JObj),
            send_response(Props, Q, MessageId)
    end.

-spec send_response(wh_proplist(), ne_binary(), ne_binary()) -> 'ok'.
send_response(_Props, Q, MessageId) ->
    JObj = wh_json:from_list(
             [{<<"Chunks">>, wh_json:new()}
             ,{<<"Analysis">>, wh_json:new()}
             ,{<<"Msg-ID">>, MessageId}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]
            ),
    wapi_inspector:publish_lookup_resp(Q, JObj).
