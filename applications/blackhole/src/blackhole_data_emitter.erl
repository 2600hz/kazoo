%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------

-module(blackhole_data_emitter).

-include("blackhole.hrl").

-export([event/4]).
-export([reply/4]).

-spec event(map(), ne_binary(), ne_binary(), kz_json:object()) -> any().
event(Binding, RK, Name, Data) ->
    #{subscribed_key := SubscribedKey
     ,subscription_key := SubscriptionKey
     ,session_pid := SessionPid
     } = Binding,
    Msg = [{<<"action">>, <<"event">>}
          ,{<<"subscribed_key">>, SubscribedKey}
          ,{<<"subscription_key">>, SubscriptionKey}
          ,{<<"name">>, Name}
          ,{<<"routing_key">>, RK}
          ,{<<"data">>, Data}
          ],
    SessionPid ! {'send_data', kz_json:from_list(Msg)}.

-spec reply(pid(), ne_binary(), ne_binary(), kz_json:object()) -> any().
reply(SessionPid, RequestId, Status, Data) ->
    lager:debug("sending reply data: ~s : ~s : ~p", [RequestId, Status, Data]),
    Msg = [{<<"action">>, <<"reply">>}
          ,{<<"request_id">>, RequestId}
          ,{<<"status">>, Status}
          ,{<<"data">>, Data}
          ],
    SessionPid ! {'send_data', kz_json:from_list(Msg)}.
