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

-include_lib("call_inspector/src/call_inspector.hrl").

-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_inspector:lookup_req_v(JObj),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    Props = ci_datastore:lookup_callid(CallId),
    Q = kz_json:get_value(<<"Server-ID">>, JObj),
    MessageId = kz_json:get_value(<<"Msg-ID">>, JObj),
    send_response(Props, Q, MessageId).

-spec send_response(ci_datastore:data(), ne_binary(), ne_binary()) -> 'ok'.
send_response(Props, Q, MessageId) ->
    JObj = kz_json:from_list(
             [{<<"Chunks">>, chunks_as_json(Props)}
              ,{<<"Dialog-Entities">>, ci_chunk:get_dialog_entities(get_chunks(Props))}
              ,{<<"Analysis">>, analysis_as_json(Props)}
              ,{<<"Msg-ID">>, MessageId}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]
            ),
    kapi_inspector:publish_lookup_resp(Q, JObj).

get_chunks(Props) ->
    props:get_value('chunks', Props, []).

-spec chunks_as_json(ci_datastore:data()) -> kz_json:objects().
chunks_as_json(Props) ->
    [ci_chunk:to_json(Chunk) || Chunk <- get_chunks(Props)].

-spec analysis_as_json(ci_datastore:data()) -> kz_json:objects().
analysis_as_json(Props) ->
    [ci_analysis:to_json(Analysis)
     || Analysis <- props:get_value('analysis', Props, [])
    ].
