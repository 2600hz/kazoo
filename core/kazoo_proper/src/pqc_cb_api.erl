%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_api).

-export([authenticate/0
        ,authenticate/3
        ,v2_base_url/0
        ,auth_account_id/1

        ,request_headers/1, request_headers/2
        ,create_envelope/1, create_envelope/2
        ,make_request/4, make_request/5

        ,cleanup/1

        ,set_log_level/1

        ,init_api/2
        ]).

-include("kazoo_proper.hrl").

-define(TRACE_FORMAT
       ,[{'elapsed', <<"-0">>}, "|"
        ,'request_id', "|"
        ,'module', ":", 'line', " (", 'pid', ")|"
        ,'message', "\n"
        ]
       ).

-type response() :: binary() |
                    kz_http:ret() |
                    {'error', binary()}.

-type fun_2() :: fun((string(), kz_term:proplist()) -> kz_http:ret()).
-type fun_3() :: fun((string(), kz_term:proplist(), iodata()) -> kz_http:ret()).

-type state() :: #{'auth_token' => kz_term:ne_binary()
                  ,'account_id' => kz_term:ne_binary()
                  ,'request_id' => kz_term:ne_binary()
                  ,'trace_file' => kz_data_tracing:trace_ref()
                  ,'start' => kz_time:start_time()
                  }.

-export_type([state/0
             ,response/0
             ,expectations/0
             ]).

-spec cleanup(state()) -> any().
cleanup(#{'trace_file' := Trace
         ,'start' := Start
         }) ->
    lager:info("cleanup after ~p ms", [kz_time:elapsed_ms(Start)]),
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

    Resp = make_request([#expectation{response_codes = [201]}]
                       ,fun kz_http:put/3
                       ,URL
                       ,default_request_headers(kz_log:get_callid())
                       ,kz_json:encode(Envelope)
                       ),
    create_api_state(Resp, Trace).

-spec authenticate(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> state().
authenticate(AccountName, Username, Password) ->
    URL =  api_base() ++ "/user_auth",
    Creds = kz_term:to_hex_binary(crypto:hash('md5', <<Username/binary, ":", Password/binary>>)),
    Data = kz_json:from_list([{<<"account_name">>, AccountName}
                             ,{<<"credentials">>, Creds}
                             ]),
    Envelope = create_envelope(Data),

    {'ok', Trace} = start_trace(),

    Resp = make_request([#expectation{response_codes = [201]}]
                       ,fun kz_http:put/3
                       ,URL
                       ,default_request_headers(kz_log:get_callid())
                       ,kz_json:encode(Envelope)
                       ),
    create_api_state(Resp, Trace).

-spec api_key() -> kz_term:ne_binary().
api_key() ->
    case kapps_util:get_master_account_id() of
        {'ok', MasterAccountId} ->
            api_key(MasterAccountId);
        {'error', _} ->
            lager:error("failed to find master account, please create an account first"),
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
                    lager:error("failed to fetch api key for ~s", [MasterAccountId]),
                    throw('missing_api_key')
            end;
        {'error', _E} ->
            lager:error("failed to fetch master account ~s: ~p", [MasterAccountId, _E]),
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
     ,'request_id' => kz_log:get_callid()
     ,'trace_file' => Trace
     ,'start' => get('start_time')
     }.

-spec v2_base_url() -> string().
v2_base_url() -> api_base().

-spec auth_account_id(state()) -> kz_term:ne_binary().
auth_account_id(#{'account_id' := AccountId}) -> AccountId.

-spec request_headers(state()) -> kz_http:headers().
request_headers(API) ->
    request_headers(API, []).

-spec request_headers(state(), request_headers()) -> kz_http:headers().
request_headers(#{'auth_token' := AuthToken
                 ,'request_id' := RequestId
                 }
               ,RequestHeaders
               ) ->
    lager:md([{'request_id', RequestId}]),
    Defaults = [{<<"x-auth-token">>, kz_term:to_list(AuthToken)}
                | default_request_headers(RequestId)
               ],
    [{kz_term:to_list(K), V}
     || {K, V} <- props:unique([{kz_term:to_binary(K), V} || {K, V} <- RequestHeaders ++ Defaults])
    ].

%% Need binary keys to avoid props assuming "foo" is [102, 111, 111] as a nested key
-spec default_request_headers() -> request_headers().
default_request_headers() ->
    [{<<"content-type">>, "application/json"}
    ,{<<"accept">>, "application/json"}
    ].

-spec default_request_headers(kz_term:ne_binary()) -> request_headers().
default_request_headers(RequestId) ->
    NowMS = kz_time:now_ms(),
    APIRequestID = kz_term:to_list(RequestId) ++ "-" ++ integer_to_list(NowMS),
    lager:debug("request id ~s", [APIRequestID]),
    [{<<"x-request-id">>, APIRequestID}
     | default_request_headers()
    ].

-spec make_request(expectations(), fun_2(), string(), request_headers()) ->
                          response().
make_request(Expectations, HTTP, URL, RequestHeaders) ->
    ?INFO("~p(~s, ~p)", [HTTP, URL, RequestHeaders]),
    handle_response(Expectations, HTTP(URL, RequestHeaders)).

-spec make_request(expectations(), fun_3(), string(), request_headers(), iodata()) ->
                          response().
make_request(Expectations, HTTP, URL, RequestHeaders, RequestBody) ->
    ?INFO("~p: ~s", [HTTP, URL]),
    ?DEBUG("headers: ~p", [RequestHeaders]),
    ?DEBUG("body: ~s", [RequestBody]),
    handle_response(Expectations, HTTP(URL, RequestHeaders, iolist_to_binary(RequestBody))).

-spec create_envelope(kz_json:json_term()) -> kz_json:object().
create_envelope(Data) ->
    create_envelope(Data, kz_json:new()).

-spec create_envelope(kz_json:json_term(), kz_json:object()) ->
                             kz_json:object().
create_envelope(Data, Envelope) ->
    kz_json:set_value(<<"data">>, Data, Envelope).

-spec handle_response(expectations(), kz_http:ret()) -> response().
handle_response(Expectations, {'ok', ActualCode, RespHeaders, RespBody}) ->
    ?INFO("checking expectations against ~p: ~p", [ActualCode, RespHeaders]),
    case expectations_met(Expectations, ActualCode, RespHeaders) of
        'true' -> RespBody;
        'false' ->
            lager:info("expectations not met: ~p", [Expectations]),
            lager:info("~p: ~p", [ActualCode, RespHeaders]),
            {'error', RespBody}
    end;
handle_response(_Expectations, {'error','socket_closed_remotely'}=E) ->
    lager:error("we broke crossbar!"),
    throw(E);
handle_response(_ExpectedCode, {'error', _}=E) ->
    lager:error("broken req: ~p", [E]),
    E.

-spec expectations_met(expectations(), response_code(), response_headers()) -> boolean().
expectations_met([], _RespCode, _RespHeaders) -> 'false';
expectations_met([Expectation|Expectations], RespCode, RespHeaders) ->
    case expectation_met(Expectation, RespCode, RespHeaders) of
        'true' -> 'true';
        'false' -> expectations_met(Expectations, RespCode, RespHeaders)
    end.

-spec expectation_met(expectation(), response_code(), response_headers()) -> boolean().
expectation_met(#expectation{response_codes = ExpectedCodes
                            ,response_headers = ExpectedHeaders
                            }
               ,RespCode
               ,RespHeaders
               ) ->
    response_code_matches(ExpectedCodes, RespCode)
        andalso response_headers_match(ExpectedHeaders, RespHeaders).

-spec response_code_matches(expected_codes(), response_code()) -> boolean().
response_code_matches([Code | _], Code) -> 'true';
response_code_matches([_Code | Codes], ResponseCode) -> response_code_matches(Codes, ResponseCode);
response_code_matches([], _ResponseCode) ->
    lager:info("failed expectation: response code ~w didn't match", [_ResponseCode]),
    'false'.

-spec response_headers_match(expected_headers(), response_headers()) -> boolean().
response_headers_match(ExpectedHeaders, RespHeaders) ->
    lists:all(fun(ExpectedHeader) -> response_header_matches(ExpectedHeader, RespHeaders) end
             ,ExpectedHeaders
             ).

-spec response_header_matches(expected_header(), response_headers()) -> boolean().
response_header_matches({ExpectedHeader, {'match', Match}}, RespHeaders) ->
    RespHeaderValue = kz_http_util:get_resp_header(ExpectedHeader, RespHeaders),
    case re:run(RespHeaderValue, Match) of
        {'match', _} -> 'true';
        'nomatch' ->
            lager:info("~p:~p ~/~ ~p", [ExpectedHeader, Match, RespHeaderValue]),
            'false'
    end;
response_header_matches({ExpectedHeader, ExpectedValue}, RespHeaders) ->
    case kz_http_util:get_resp_header(ExpectedHeader, RespHeaders) of
        ExpectedValue -> 'true';
        _RespHeaderValue ->
            lager:info("~p:~p =/= ~p", [ExpectedHeader, ExpectedValue, _RespHeaderValue]),
            'false'
    end.

-spec start_trace() -> {'ok', kz_data_tracing:trace_ref()}.
start_trace() ->
    RequestId = case kz_log:get_callid() of
                    'undefined' ->
                        RID = kz_binary:rand_hex(5),
                        kz_log:put_callid(RID),
                        RID;
                    RID -> RID
                end,
    lager:md([{'request_id', RequestId}]),
    put('start_time', kz_time:start_time()),

    TracePath = trace_path(),

    TraceFile = filename:join(TracePath, kz_term:to_list(RequestId) ++ ".log"),
    lager:info("tracing at ~s", [TraceFile]),

    {'ok', _}=OK = kz_data_tracing:trace_file([glc_ops:eq('request_id', RequestId)]
                                             ,TraceFile
                                             ,?TRACE_FORMAT
                                             ,get_log_level()
                                             ),
    lager:info("authenticating...~s", [RequestId]),
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

-spec get_log_level() -> atom().
get_log_level() ->
    case get('log_level') of
        'undefined' -> 'debug';
        LogLevel -> LogLevel
    end.

-spec init_api([atom()], [module()]) -> state().
init_api(AppsToStart, ModulesToStart) when is_list(AppsToStart)
                                           andalso is_list(ModulesToStart) ->
    Model = initial_state(AppsToStart, ModulesToStart),
    pqc_kazoo_model:api(Model).

-spec initial_state([atom()], [module()]) -> pqc_kazoo_model:model().
initial_state(AppsToStart, ModulesToStart) ->
    _ = init_system(AppsToStart, ModulesToStart),
    API = authenticate(),
    pqc_kazoo_model:new(API).

-spec init_system([atom()], [module()]) -> 'ok'.
init_system(AppsToStart, ModulesToStart) ->
    TestId = kz_binary:rand_hex(5),
    kz_log:put_callid(TestId),

    _ = kz_data_tracing:clear_all_traces(),
    _ = [kapps_controller:start_app(App) ||
            App <- AppsToStart
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ModulesToStart
        ],

    ?INFO("INIT FINISHED").
