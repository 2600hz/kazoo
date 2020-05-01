%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ci_lookup_req).

-export([handle_req/2]).

-include("call_inspector.hrl").

-spec handle_req(kz_json:object(), kz_term:proplist()) -> ok.
handle_req(JObj, _) ->
    true = kapi_inspector:lookup_req_v(JObj),
    Props = ci_datastore:lookup_callid(kz_json:get_value(<<"Call-ID">>, JObj)),
    Chunks = props:get_value(chunks, Props, []),
    Analysis = props:get_value(analysis, Props, []),
    Data = kz_json:from_list(
             [{<<"Chunks">>, chunks_as_json(Chunks)}
             ,{<<"Dialog-Entities">>, ci_chunk:get_dialog_entities(Chunks)}
             ,{<<"Analysis">>, analysis_as_json(Analysis)}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]
            ),
    kapi_inspector:publish_lookup_resp(kz_api:server_id(JObj), Data).

chunks_as_json(Chunks) ->
    [ci_chunk:to_json(Chunk) || Chunk <- Chunks].

analysis_as_json(As) ->
    [ci_analysis:to_json(Analysis) || Analysis <- As].
