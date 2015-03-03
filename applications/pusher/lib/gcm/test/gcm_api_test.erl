-module(gcm_api_test).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
    error_logger:tty(false),
    meck:new(httpc).

stop(_Pid) ->
    meck:unload(httpc).

retry_after_test_() ->
    [{"Receive 50x without retry-after", ?setup(fun get_retry_after_when_missing/1)},
     {"Receive 50x with retry-after as integer", ?setup(fun get_retry_after_when_integer/1)},
     {"Receive 50x with retry-after as HTTP date", ?setup(fun get_retry_after_when_http_date/1)}].

get_retry_after_when_missing(_Pid) ->
    mock_httpc_with_retry(""),

    Reply = gcm_api:push([<<"regids">>], [{<<"data">>, [{<<"message">>, <<"a message">>}]}], "apikey"),
    ?_assertMatch({error, {retry, no_retry}}, Reply).

get_retry_after_when_integer(_Pid) ->
    mock_httpc_with_retry("120"),

    Reply = gcm_api:push([<<"regids">>], [{<<"data">>, [{<<"message">>, <<"a message">>}]}], "apikey"),
    ?_assertMatch({error, {retry, 120}}, Reply).

get_retry_after_when_http_date(_Pid) ->
    application:start(qdate),
    mock_httpc_with_retry("Fri, 31 Dec 1999 23:59:59 GMT"),

    Reply = gcm_api:push([<<"regids">>], [{<<"data">>, [{<<"message">>, <<"a message">>}]}], "apikey"),
    ?_assertMatch({error, {retry, I}} when is_integer(I), Reply).

mock_httpc_with_retry(Retry) ->
    Headers = case Retry of
                  "" -> [];
                  RetryAfter ->
                      [{"retry-after", RetryAfter}]
              end,

    meck:expect(httpc, request,
		fun(post, {_BaseURL, _AuthHeader, "application/json", _JSON}, [], []) ->
			{ok, {{"", 503, ""}, Headers, <<"">>}}
		end).
