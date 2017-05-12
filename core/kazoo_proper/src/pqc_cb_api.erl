-module(pqc_cb_api).

-export([authenticate/0
        ,v2_base_url/0
        ,auth_account_id/1

        ,request_headers/1
        ,create_envelope/1, create_envelope/2
        ,make_request/4, make_request/5
        ]).

-include("kazoo_proper.hrl").

-type state() :: #{'auth_token' => ne_binary()
                  ,'account_id' => ne_binary()
                  ,'request_id' => ne_binary()
                  }.

-export_type([state/0
             ,response/0
             ]).

-define(API_BASE, "http://localhost:8000/v2").
-define(API_KEY, <<"df52943da7119a3fc901dcadba313c0689d6d883dd26f5fa84abbc44df6c30d1">>).

-spec authenticate() -> state().
authenticate() ->
    URL = ?API_BASE "/api_auth",
    Data = kz_json:from_list([{<<"api_key">>, ?API_KEY}]),
    Envelope = create_envelope(Data),
    Resp = make_request([201]
                       ,fun kz_http:put/3
                       ,URL
                       ,default_request_headers()
                       ,kz_json:encode(Envelope)
                       ),
    create_api_state(Resp).

-spec create_api_state(binary()) -> map().
create_api_state(RespJSON) ->
    RespEnvelope = kz_json:decode(RespJSON),
    #{'auth_token' => kz_json:get_ne_binary_value(<<"auth_token">>, RespEnvelope)
     ,'account_id' => kz_json:get_ne_binary_value([<<"data">>, <<"account_id">>], RespEnvelope)
     ,'request_id' => kz_binary:rand_hex(8)
     }.

-spec v2_base_url() -> string().
v2_base_url() -> ?API_BASE.

-spec auth_account_id(state()) -> ne_binary().
auth_account_id(#{'account_id' := AccountId}) -> AccountId.

-spec request_headers(state()) -> kz_proplist().
request_headers(#{'auth_token' := AuthToken
                 ,'request_id' := RequestId
                 }) ->
    [{"x-auth-token", kz_term:to_list(AuthToken)}
    ,{"x-request-id", kz_term:to_list(RequestId) ++ pid_to_list(self())}
     | default_request_headers()
    ].

-spec default_request_headers() -> kz_proplist().
default_request_headers() ->
    [{"content-type", "application/json"}
    ,{"accept", "application/json"}
    ].

-type expected_codes() :: [integer()].
-type response() :: binary() |
                    {'error', binary()}.

-spec make_request(expected_codes(), fun(), iolist(), kz_proplist()) -> response().
-spec make_request(expected_codes(), fun(), iolist(), kz_proplist(), binary()) -> response().
make_request(ExpectedCodes, HTTP, URL, RequestHeaders) ->
    handle_response(ExpectedCodes, HTTP(URL, RequestHeaders)).
make_request(ExpectedCodes, HTTP, URL, RequestHeaders, RequestBody) ->
    handle_response(ExpectedCodes, HTTP(URL, RequestHeaders, RequestBody)).

-spec create_envelope(kz_json:object()) ->
                             kz_json:object().
-spec create_envelope(kz_json:object(), kz_json:object()) ->
                             kz_json:object().
create_envelope(Data) ->
    create_envelope(Data, kz_json:new()).
create_envelope(Data, Envelope) ->
    kz_json:set_value(<<"data">>, Data, Envelope).

-spec handle_response(expected_codes(), kz_http:ret()) -> response().
handle_response(ExpectedCode, {'ok', ExpectedCode, _RespHeaders, RespBody}) ->
    RespBody;
handle_response(ExpectedCodes, {'ok', ActualCode, _RespHeaders, RespBody})
  when is_list(ExpectedCodes) ->
    case lists:member(ActualCode, ExpectedCodes) of
        'true' -> RespBody;
        'false' ->
            io:format('user', "failed to get any ~w: ~p: ~s~n"
                     ,[ExpectedCodes, ActualCode, RespBody]
                     ),
            {'error', RespBody}
    end;
handle_response(_ExtectedCode, {'error','socket_closed_remotely'}=E) ->
    io:format("~nwe broke crossbar!~n"),
    throw(E);
handle_response(_ExpectedCode, {'ok', _ActualCode, _RespHeaders, RespBody}) ->
    io:format('user', "failed to get ~w: ~p: ~s~n", [_ExpectedCode, _ActualCode, RespBody]),
    {'error', RespBody}.
