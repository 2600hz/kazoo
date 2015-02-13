-module(gcm_test).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
    error_logger:tty(false),
    {ok, _} = gcm:start_link(test, "APIKEY"),
    meck:new(gcm_api),
    _Pid = self().

stop(_Pid) ->
    meck:unload(gcm_api),
    gcm:stop(test).

gcm_sync_test_() ->
    [{"sync_push returns the result", ?setup(fun receive_results_from_sync_push/1)}].

receive_results_from_sync_push(_) ->
    RegIds = [<<"RegId0">>, <<"RegId1">>, <<"RegId2">>],
    Message = [{<<"data">>, [{<<"key">>, <<"value">>}]}],

    mock_gcm_api(),

    Actual = gcm:sync_push(test, RegIds, Message),

    Expected = [{<<"RegId0">>, ok}, {<<"RegId1">>, <<"InvalidRegistration">>},
                {<<"RegId2">>, {<<"NewRegistrationId">>, <<"NewRegId">>}}],
    [
     {"Results are passed to the caller", ?_assertMatch(Expected, Actual)},
     {"Calls gcm_api", ?_assert(meck:validate(gcm_api))}
    ].

mock_gcm_api() ->
    Result = {<<"anyMulticastId">>, 2, 1, 1, [[{<<"message_id">>,<<"1:0408">>}],
                                              [{<<"error">>,<<"InvalidRegistration">>}],
                                              [{<<"message_id">>,<<"1:2342">>},
                                               {<<"registration_id">>,<<"NewRegId">>}]]},

    meck:expect(gcm_api, push, fun(_RegIs, _Message, _APIKey) -> {ok, Result} end).
