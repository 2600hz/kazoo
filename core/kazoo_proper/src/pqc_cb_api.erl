%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_api).

-export([authenticate/0
        ,v2_base_url/0
        ,auth_account_id/1

        ,request_headers/1, request_headers/2
        ,create_envelope/1, create_envelope/2
        ,make_request/4, make_request/5

        ,cleanup/1

        ,set_log_level/1
        ]).

-include("kazoo_proper.hrl").

-define(TRACE_FORMAT
       ,[{'elapsed', <<"-0">>}, "|"
        ,'request_id', "|"
        ,'module', ":", 'line', " (", 'pid', ")|"
        ,'message', "\n"
        ]
       ).

-type state() :: #{'auth_token' => kz_term:ne_binary()
                  ,'account_id' => kz_term:ne_binary()
                  ,'request_id' => kz_term:ne_binary()
                  ,'trace_file' => kz_data_tracing:trace_ref()
                  ,'start' => kz_time:now()
                  }.

-export_type([state/0
             ,response/0
             ,expectations/0
             ]).

-spec cleanup(state()) -> any().
cleanup(#{'trace_file' := Trace
         ,'start' := Start
         }) ->
    ?INFO("cleanup after ~p ms", [kz_time:elapsed_ms(Start)]),
    kz_data_tracing:stop_trace(Trace).

-define(API_BASE, net_adm:localhost()).

api_base() ->
    Host = kz_network_utils:get_hostname(),
    {Scheme, Port} = case kapps_config:get_integer(<<"crossbar">>, <<"port">>) of
                         'undefined' ->
                             {"https://", kapps_config:get_integer(<<"crossbar">>, <<"ssl_port">>)};
                         P -> {"http://", P}
                     end,
    Scheme ++ Host ++ [$: | integer_to_list(Port)] ++ "/v2".

-spec authenticate() -> state().
authenticate() ->
    URL =  api_base() ++ "/api_auth",
    Data = kz_json:from_list([{<<"api_key">>, api_key()}]),
    Envelope = create_envelope(Data),

    {'ok', Trace} = start_trace(),

    Resp = make_request([201]
                       ,fun kz_http:put/3
                       ,URL
                       ,default_request_headers(kz_util:get_callid())
                       ,kz_json:encode(Envelope)
                       ),
    create_api_state(Resp, Trace).

-spec api_key() -> kz_term:ne_binary().
api_key() ->
    case kapps_util:get_master_account_id() of
        {'ok', MasterAccountId} ->
            api_key(MasterAccountId);
        {'error', _} ->
            ?ERROR("failed to find master account, please create an account first"),
            throw('no_master_account')
    end.

-spec api_key(kz_term:ne_binary()) -> kz_term:ne_binary().
api_key(MasterAccountId) ->
    case kzd_accounts:fetch(MasterAccountId) of
        {'ok', MasterAccount} ->
            APIKey = kzd_accounts:api_key(MasterAccount),
            case is_binary(APIKey) of
                'true' -> APIKey;
                'false' ->
                    ?ERROR("failed to fetch api key for ~s", [MasterAccountId]),
                    throw('missing_api_key')
            end;
        {'error', _E} ->
            ?ERROR("failed to fetch master account ~s: ~p", [MasterAccountId, _E]),
            throw('missing_master_account')
    end.

-spec create_api_state(response(), kz_data_tracing:trace_ref()) -> state().
create_api_state({'error', {'failed_connect', 'econnrefused'}}, _Trace) ->
    lager:warning("failed to connect to Crossbar; is it running?"),
    throw({'error', 'econnrefused'});
create_api_state(<<_/binary>> = RespJSON, Trace) ->
    RespEnvelope = kz_json:decode(RespJSON),
    #{'auth_token' => kz_json:get_ne_binary_value(<<"auth_token">>, RespEnvelope)
     ,'account_id' => kz_json:get_ne_binary_value([<<"data">>, <<"account_id">>], RespEnvelope)
     ,'request_id' => kz_util:get_callid()
     ,'trace_file' => Trace
     ,'start' => get('now')
     }.

-spec v2_base_url() -> string().
v2_base_url() -> api_base().

-spec auth_account_id(state()) -> kz_term:ne_binary().
auth_account_id(#{'account_id' := AccountId}) -> AccountId.

-spec request_headers(state()) -> kz_term:proplist().
request_headers(API) ->
    request_headers(API, []).

-spec request_headers(state(), kz_term:proplist()) -> kz_term:proplist().
request_headers(#{'auth_token' := AuthToken
                 ,'request_id' := RequestId
                 }
               ,RequestHeaders
               ) ->
    lager:md([{'request_id', RequestId}]),
    props:set_values(RequestHeaders
                    ,[{<<"x-auth-token">>, kz_term:to_list(AuthToken)}
                      | default_request_headers(RequestId)
                     ]
                    ).

-spec default_request_headers() -> kz_term:proplist().
default_request_headers() ->
    [{<<"content-type">>, <<"application/json">>}
    ,{<<"accept">>, <<"application/json">>}
    ].

-spec default_request_headers(kz_term:ne_binary()) -> kz_term:proplist().
default_request_headers(RequestId) ->
    NowMS = kz_time:now_ms(),
    APIRequestID = kz_term:to_list(RequestId) ++ "-" ++ integer_to_list(NowMS),
    ?DEBUG("request id ~s", [APIRequestID]),
    [{<<"x-request-id">>, APIRequestID}
     | default_request_headers()
    ].

-type expected_code() :: 200..600.
-type expected_codes() :: [expected_code()].
-type expected_headers() :: [{string(), string()}].
-type expectations() :: #{'response_codes' => expected_codes()
                         ,'response_headers' => expected_headers()
                         }.

-type response() :: binary() |
                    kz_http:ret() |
                    {'error', binary()}.

-type fun_2() :: fun((string(), kz_term:proplist()) -> kz_http:ret()).
-type fun_3() :: fun((string(), kz_term:proplist(), iodata()) -> kz_http:ret()).

-spec make_request(expectations() | expected_code() | expected_codes(), fun_2(), string(), kz_term:proplist()) ->
                          response().
make_request(Code, HTTP, URL, RequestHeaders) when is_integer(Code) ->
    make_request(#{'response_codes' => [Code]}, HTTP, URL, RequestHeaders);
make_request([Code|_]=Codes, HTTP, URL, RequestHeaders) when is_integer(Code) ->
    make_request(#{'response_codes' => Codes}, HTTP, URL, RequestHeaders);
make_request(Expectations, HTTP, URL, RequestHeaders) ->
    ?INFO("~p(~p, ~p)", [HTTP, URL, RequestHeaders]),

    handle_response(Expectations, HTTP(URL, RequestHeaders)).

-spec make_request(expectations() | expected_code() | expected_codes(), fun_3(), string(), kz_term:proplist(), iodata()) ->
                          response().
make_request(Code, HTTP, URL, RequestHeaders, RequestBody) when is_integer(Code) ->
    make_request(#{'response_codes' => [Code]}, HTTP, URL, RequestHeaders, RequestBody);
make_request([Code|_]=Codes, HTTP, URL, RequestHeaders, RequestBody) when is_integer(Code) ->
    make_request(#{'response_codes' => Codes}, HTTP, URL, RequestHeaders, RequestBody);
make_request(Expectations, HTTP, URL, RequestHeaders, RequestBody) ->
    ?INFO("~p: ~s", [HTTP, URL]),
    ?DEBUG("headers: ~p", [RequestHeaders]),
    ?DEBUG("body: ~s", [RequestBody]),
    handle_response(Expectations, HTTP(URL, RequestHeaders, iolist_to_binary(RequestBody))).

-spec create_envelope(kz_json:json_term()) ->
                             kz_json:object().
create_envelope(Data) ->
    create_envelope(Data, kz_json:new()).

-spec create_envelope(kz_json:json_term(), kz_json:object()) ->
                             kz_json:object().
create_envelope(Data, Envelope) ->
    kz_json:set_value(<<"data">>, Data, Envelope).

-spec handle_response(expectations(), kz_http:ret()) -> response().
handle_response(Expectations, {'ok', ActualCode, RespHeaders, RespBody}) ->
    case expectations_met(Expectations, ActualCode, RespHeaders) of
        'true' ->
            ?DEBUG("resp headers: ~p", [RespHeaders]),
            RespBody;
        'false' ->
            {'error', RespBody}
    end;
handle_response(_Expectations, {'error','socket_closed_remotely'}=E) ->
    ?ERROR("~nwe broke crossbar!"),
    throw(E);
handle_response(_ExpectedCode, {'error', _}=E) ->
    ?ERROR("broken req: ~p", [E]),
    E.

expectations_met(Expectations, RespCode, RespHeaders) ->
    response_code_matches(Expectations, RespCode)
        andalso response_headers_match(Expectations, RespHeaders).

response_code_matches(#{'response_codes' := ResponseCodes}, ResponseCode) ->
    case lists:member(ResponseCode, ResponseCodes) of
        'true' -> 'true';
        'false' ->
            ?ERROR("failed expectation: code ~w but expected ~w"
                  ,[ResponseCode, ResponseCodes]
                  ),
            'false'
    end;
response_code_matches(_Expectations, _Code) -> 'true'.

response_headers_match(#{'response_headers' := ExpectedHeaders}, RespHeaders) ->
    lists:all(fun(ExpectedHeader) -> response_header_matches(ExpectedHeader, RespHeaders) end
             ,ExpectedHeaders
             );
response_headers_match(_Expectations, _RespHeaders) -> 'true'.


response_header_matches({ExpectedHeader, ExpectedValue}, RespHeaders) ->
    case props:get_value(ExpectedHeader, RespHeaders) of
        ExpectedValue -> 'true';
        'undefined' ->
            ?ERROR("failed expectation: header ~s missing from response", [ExpectedHeader]),
            'false';
        _ActualValue ->
            ?ERROR("failed expectation: header ~s is not ~p but ~p"
                  ,[ExpectedHeader, ExpectedValue, _ActualValue]
                  ),
            'false'
    end.

-spec start_trace() -> {'ok', kz_data_tracing:trace_ref()}.
start_trace() ->
    RequestId = case kz_util:get_callid() of
                    'undefined' ->
                        RID = kz_binary:rand_hex(5),
                        kz_util:put_callid(RID),
                        RID;
                    RID -> RID
                end,
    lager:md([{'request_id', RequestId}]),
    put('now', kz_time:now()),

    TracePath = trace_path(),

    TraceFile = filename:join(TracePath, kz_term:to_list(RequestId) ++ ".log"),
    lager:info("tracing at ~s", [TraceFile]),

    {'ok', _}=OK = kz_data_tracing:trace_file([glc_ops:eq('request_id', RequestId)]
                                             ,TraceFile
                                             ,?TRACE_FORMAT
                                             ,get_log_level()
                                             ),
    ?INFO("authenticating...~s", [RequestId]),
    OK.

-spec trace_path() -> file:filename_all().
trace_path() ->
    case application:get_env('kazoo_proper', 'trace_path') of
        'undefined' -> "/tmp";
        {'ok', Path} -> Path
    end.

-spec set_log_level(atom()) -> atom().
set_log_level(LogLevel) ->
    put('log_level', LogLevel).

get_log_level() ->
    case get('log_level') of
        'undefined' -> 'debug';
        LogLevel -> LogLevel
    end.
