%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author SIPLABS, LLC (Maksim Krzhemenevskiy)
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_agent).

-include("kz_documents.hrl").

%% API
-export([maybe_add_queue/2, maybe_add_queue/3
        ,maybe_rm_queue/2, maybe_rm_queue/3
        ]).

-spec maybe_add_queue(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
maybe_add_queue(AgentJObj, QueueId) ->
    maybe_add_queue(AgentJObj, QueueId, AgentJObj).

-spec maybe_add_queue(kz_json:object(), kz_term:ne_binary(), Default) -> kz_json:object() | Default.
maybe_add_queue(AgentJObj, QueueId, WhenExists) ->
    Qs = kz_json:get_value(<<"queues">>, AgentJObj, []),
    case lists:member(QueueId, Qs) of
        'false' -> kz_json:set_value(<<"queues">>, [QueueId | Qs], AgentJObj);
        'true' -> WhenExists
    end.

-spec maybe_rm_queue(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
maybe_rm_queue(AgentJObj, QueueId) ->
    maybe_rm_queue(AgentJObj, QueueId, AgentJObj).

-spec maybe_rm_queue(kz_json:object(), kz_term:ne_binary(), Default) -> kz_json:object() | Default.
maybe_rm_queue(AgentJObj, QueueId, WhenNotExists) ->
    Qs = kz_json:get_value(<<"queues">>, AgentJObj, []),
    case lists:member(QueueId, Qs) of
        'true' -> kz_json:set_value(<<"queues">>, lists:delete(QueueId, Qs), AgentJObj);
        'false' -> WhenNotExists
    end.
