%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(blackhole_data_emitter).

-include("blackhole.hrl").

-export([event/4]).
-export([reply/4]).

-define(MAX_QUEUED_MESSAGES, kapps_config:get_integer(?CONFIG_CAT, <<"max_queued_messages">>, 50)).

-spec event(map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
event(#{subscribed_key := SubscribedKey
       ,subscription_key := SubscriptionKey
       ,session_pid := SessionPid
       }
     ,RK, Name, Data
     ) ->
    Msg = [{<<"action">>, <<"event">>}
          ,{<<"subscribed_key">>, SubscribedKey}
          ,{<<"subscription_key">>, SubscriptionKey}
          ,{<<"name">>, Name}
          ,{<<"routing_key">>, RK}
          ,{<<"data">>, Data}
          ],
    lager:debug("sending event with routing key ~s to session PID ~p", [RK, SessionPid]),
    maybe_send(SessionPid, kz_json:from_list(Msg)).

-spec reply(pid(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
reply(SessionPid, RequestId, Status, Data) ->
    lager:debug("sending reply data: ~s : ~s : ~p", [RequestId, Status, Data]),
    Msg = [{<<"action">>, <<"reply">>}
          ,{<<"request_id">>, RequestId}
          ,{<<"status">>, Status}
          ,{<<"data">>, Data}
          ],
    maybe_send(SessionPid, kz_json:from_list(Msg)).

maybe_send(SessionPid, Data) ->
    maybe_send(SessionPid, Data, process_info(SessionPid, ['message_queue_len'])).

maybe_send(SessionPid, Data, [{'message_queue_len', QueueLen}]) ->
    maybe_send(SessionPid, Data, QueueLen, ?MAX_QUEUED_MESSAGES);
maybe_send(_SessionPid, _Data, 'undefined') ->
    lager:info("failed to find session ~p, dropping data").

maybe_send(SessionPid, _Data, QueueLen, MaxLen) when QueueLen > MaxLen ->
    lager:error("~p queue length ~p (max: ~p), dropping event", [SessionPid, QueueLen, MaxLen]);
maybe_send(SessionPid, Data, _QueueLen, _MaxLen) ->
    SessionPid ! {'send_data', Data},
    'ok'.
