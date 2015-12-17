-module(apns_SUITE).
-author('Brujo Benavides <elbrujohalcon@inaka.net>').

-include("apns.hrl").
-include("localized.hrl").
-define(DEVICE_TOKEN,
        "139D3CAB173FB230B97E4A19D288E3FBCD4B037F9B18ABA17FE4CDE72085E994").

-define(TEST_CONNECTION, 'test-connection').

-export([all/0, init_per_suite/1, end_per_suite/1, minimal/1]).

-spec all() -> [atom()].
all() -> [minimal].

-spec init_per_suite(Config) -> Config.
init_per_suite(Config) -> Config.

-spec end_per_suite(Config) -> Config.
end_per_suite(Config) ->
  apns:stop(),
  Config.

%%% Tests
-spec minimal(any()) -> {comment, []}.
minimal(_Config) ->
  Now = lists:flatten(io_lib:format("~p", [calendar:local_time()])),
  ok = apns:start(),
  {ok, Pid} =
    apns:connect(?TEST_CONNECTION, fun log_error/2, fun log_feedback/1),
  Ref = erlang:monitor(process, Pid),
  ok =
    apns:send_message(
      ?TEST_CONNECTION, ?DEVICE_TOKEN,
      Now ++ " - Test Alert", random:uniform(10), "chime"),
  receive
    {'DOWN', Ref, _, _, _} = DownMsg ->
      throw(DownMsg);
    DownMsg ->
      throw(DownMsg)
    after 1000 ->
      ok
  end,
  ok =
    apns:send_message(
      ?TEST_CONNECTION,
      ?DEVICE_TOKEN,
      #loc_alert{ action = "ACTION",
                  args   = ["arg1", "arg2"],
                  body   = Now ++ " - Localized Body",
                  image  = none,
                  key    = "KEY"},
      random:uniform(10),
      "chime"),
  receive
    {'DOWN', Ref, _, _, _} = DownMsg2 ->
      throw(DownMsg2);
    DownMsg2 ->
      throw(DownMsg2)
    after 1000 ->
      ok
  end,
  ok =
    apns:send_message(
      ?TEST_CONNECTION, ?DEVICE_TOKEN, #loc_alert{key = "EMPTY"},
      random:uniform(10), "chime"),
  receive
    {'DOWN', Ref, _, _, _} = DownMsg3 ->
      throw(DownMsg3);
    DownMsg3 ->
      throw(DownMsg3)
    after 1000 ->
      ok
  end,
  ok =
    apns:send_message(
      ?TEST_CONNECTION, ?DEVICE_TOKEN, Now ++ " - Test Alert",
      random:uniform(10), "chime",
      apns:expiry(86400), [{<<"acme1">>, 1}]),
  receive
    {'DOWN', Ref, _, _, _} = DownMsg4 ->
      throw(DownMsg4);
    DownMsg4 ->
      throw(DownMsg4)
    after 1000 ->
      ok
  end,
  ok =
    apns:send_message(
      ?TEST_CONNECTION, ?DEVICE_TOKEN,
      #loc_alert{ action = "ACTION",
                  args   = ["arg1", "arg2"],
                  body   = Now ++ " - Localized Body",
                  image  = none,
                  key    = "KEY"},
      random:uniform(10), "chime",
      apns:expiry(86400),
      [ {<<"acme2">>, <<"x">>},
        {<<"acme3">>, {[{<<"acme4">>, false}]}}]),
  receive
    {'DOWN', Ref, _, _, _} = DownMsg5 ->
      throw(DownMsg5);
    DownMsg5 ->
      throw(DownMsg5)
    after 1000 ->
      ok
  end,
  ok = apns:send_message(?TEST_CONNECTION, #apns_msg{device_token = ?DEVICE_TOKEN,
                                 sound = "chime",
                                 badge = 12,
                                 expiry = apns:expiry(86400),
                                 alert = "Low Priority alert",
                                 priority = 0}),
  receive
    {'DOWN', Ref, _, _, _} = DownMsg6 ->
      throw(DownMsg6);
    DownMsg6 ->
      throw(DownMsg6)
    after 1000 ->
      ok
  end.

log_error(MsgId, Status) ->
  error_logger:error_msg("Error on msg ~p: ~p~n", [MsgId, Status]).

log_feedback(Token) ->
  error_logger:warning_msg("Device with token ~p removed the app~n", [Token]).
