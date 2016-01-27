%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(kzd_agent).

-include("kz_documents.hrl").

%% API
-export([maybe_add_queue/2, maybe_add_queue/3
         ,maybe_rm_queue/2, maybe_rm_queue/3
        ]).

-spec maybe_add_queue(wh_json:object(), ne_binary()) -> wh_json:object().
maybe_add_queue(AgentJObj, QueueId) ->
    maybe_add_queue(AgentJObj, QueueId, AgentJObj).

-spec maybe_add_queue(wh_json:object(), ne_binary(), Default) -> wh_json:object() | Default.
maybe_add_queue(AgentJObj, QueueId, WhenExists) ->
    Qs = wh_json:get_value(<<"queues">>, AgentJObj, []),
    case lists:member(QueueId, Qs) of
        'false' -> wh_json:set_value(<<"queues">>, [QueueId | Qs], AgentJObj);
        'true' -> WhenExists
    end.

-spec maybe_rm_queue(wh_json:object(), ne_binary()) -> wh_json:object().
maybe_rm_queue(AgentJObj, QueueId) ->
    maybe_rm_queue(AgentJObj, QueueId, AgentJObj).

-spec maybe_rm_queue(wh_json:object(), ne_binary(), Default) -> wh_json:object() | Default.
maybe_rm_queue(AgentJObj, QueueId, WhenNotExists) ->
    Qs = wh_json:get_value(<<"queues">>, AgentJObj, []),
    case lists:member(QueueId, Qs) of
        'true' -> wh_json:set_value(<<"queues">>, lists:delete(QueueId, Qs), AgentJObj);
        'false' -> WhenNotExists
    end.
