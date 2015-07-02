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
    Props = ci_datastore:lookup_callid(CallId),
    Q = wh_json:get_value(<<"Server-ID">>, JObj),
    MessageId = wh_json:get_value(<<"Msg-ID">>, JObj),
    send_response(Props, Q, MessageId).

-spec send_response(wh_proplist(), ne_binary(), ne_binary()) -> 'ok'.
send_response(Props, Q, MessageId) ->
    JObj = wh_json:from_list(
             [{<<"Chunks">>, chunks_as_json(Props)}
              ,{<<"Analysis">>, analysis_as_json(Props)}
              ,{<<"Msg-ID">>, MessageId}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]
            ),
    wapi_inspector:publish_lookup_resp(Q, JObj).

-spec chunks_as_json(wh_proplist()) -> wh_json:objects().
chunks_as_json(Props) ->
    [ci_chunk:to_json(Chunk)
     || Chunk <- props:get_value('chunks', Props, [])
    ].

-spec analysis_as_json(wh_proplist()) -> wh_json:object().
analysis_as_json(Props) ->
    [ci_analysis:to_json(Analysis)
     || Analysis <- props:get_value('analysis', Props, [])
    ].
