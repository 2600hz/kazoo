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

-type state() :: #{'auth_token' => ne_binary()
                  ,'account_id' => ne_binary()
                  ,'request_id' => ne_binary()
                  ,'trace_file' => kz_data_tracing:trace_ref()
                  ,'start' => kz_now()
                  }.

-export_type([state/0
             ,response/0
             ]).

-spec cleanup(state()) -> any().
cleanup(#{'trace_file' := Trace
         ,'start' := Start
         }) ->
    ?INFO("cleanup after ~p ms", [kz_time:elapsed_ms(Start)]),
    kz_data_tracing:stop_trace(Trace).

-define(API_BASE, "http://" ++ net_adm:localhost() ++ ":8000/v2").

-spec authenticate() -> state().
authenticate() ->
    URL =  ?API_BASE ++ "/api_auth",
    Data = kz_json:from_list([{<<"api_key">>, api_key()}]),
    Envelope = create_envelope(Data),

    RequestId = kz_binary:rand_hex(5),

    {'ok', Trace} = start_trace(RequestId),

    Resp = make_request([201]
                       ,fun kz_http:put/3
                       ,URL
                       ,default_request_headers(RequestId)
                       ,kz_json:encode(Envelope)
                       ),
    create_api_state(Resp, RequestId, Trace).

-spec api_key() -> ne_binary().
-spec api_key(ne_binary()) -> ne_binary().
api_key() ->
    case kapps_util:get_master_account_id() of
        {'ok', MasterAccountId} ->
            api_key(MasterAccountId);
        {'error', _} ->
            ?ERROR("failed to find master account, please create an account first"),
            throw('no_master_account')
    end.

api_key(MasterAccountId) ->
    case kz_account:fetch(MasterAccountId) of
        {'ok', MasterAccount} ->
            APIKey = kz_account:api_key(MasterAccount),
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

-spec create_api_state(binary(), binary(), kz_data_tracing:trace_ref()) -> state().
create_api_state(<<_/binary>> = RespJSON, RequestId, Trace) ->
    RespEnvelope = kz_json:decode(RespJSON),
    #{'auth_token' => kz_json:get_ne_binary_value(<<"auth_token">>, RespEnvelope)
     ,'account_id' => kz_json:get_ne_binary_value([<<"data">>, <<"account_id">>], RespEnvelope)
     ,'request_id' => RequestId
     ,'trace_file' => Trace
     ,'start' => get('now')
     }.

-spec v2_base_url() -> string().
v2_base_url() -> ?API_BASE.

-spec auth_account_id(state()) -> ne_binary().
auth_account_id(#{'account_id' := AccountId}) -> AccountId.

-spec request_headers(state()) -> kz_proplist().
-spec request_headers(state(), kz_proplist()) -> kz_proplist().
request_headers(API) ->
    request_headers(API, []).

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

-spec default_request_headers() -> kz_proplist().
-spec default_request_headers(ne_binary()) -> kz_proplist().
default_request_headers() ->
    [{<<"content-type">>, <<"application/json">>}
    ,{<<"accept">>, <<"application/json">>}
    ].

default_request_headers(RequestId) ->
    NowMS = kz_time:now_ms(),
    [{<<"x-request-id">>, kz_term:to_list(RequestId) ++ "-" ++ integer_to_list(NowMS)}
     | default_request_headers()
    ].

-type expected_codes() :: [integer()].
-type response() :: binary() |
                    {'error', binary()}.

-type fun_2() :: fun((string(), kz_proplist()) -> kz_http:ret()).
-type fun_3() :: fun((string(), kz_proplist(), iodata()) -> kz_http:ret()).

-spec make_request(expected_codes(), fun_2(), string(), kz_proplist()) ->
                          response().
-spec make_request(expected_codes(), fun_3(), string(), kz_proplist(), iodata()) ->
                          response().
make_request(ExpectedCodes, HTTP, URL, RequestHeaders) ->
    ?INFO("~p: ~s", [HTTP, URL]),
    ?DEBUG("headers: ~p", [RequestHeaders]),
    handle_response(ExpectedCodes, HTTP(URL, RequestHeaders)).
make_request(ExpectedCodes, HTTP, URL, RequestHeaders, RequestBody) ->
    ?INFO("~p: ~s", [HTTP, URL]),
    ?DEBUG("headers: ~p", [RequestHeaders]),
    ?DEBUG("body: ~s", [RequestBody]),
    handle_response(ExpectedCodes, HTTP(URL, RequestHeaders, iolist_to_binary(RequestBody))).

-spec create_envelope(kz_json:json_term()) ->
                             kz_json:object().
-spec create_envelope(kz_json:json_term(), kz_json:object()) ->
                             kz_json:object().
create_envelope(Data) ->
    create_envelope(Data, kz_json:new()).
create_envelope(Data, Envelope) ->
    kz_json:set_value(<<"data">>, Data, Envelope).

-spec handle_response(expected_codes(), kz_http:ret()) -> response().
handle_response(ExpectedCode, {'ok', ExpectedCode, _RespHeaders, RespBody}) ->
    ?DEBUG("recv expected ~p: ~s", [ExpectedCode, RespBody]),
    RespBody;
handle_response(ExpectedCodes, {'ok', ActualCode, _RespHeaders, RespBody})
  when is_list(ExpectedCodes) ->
    case lists:member(ActualCode, ExpectedCodes) of
        'true' ->
            ?DEBUG("recv expected ~p: ~s", [ActualCode, RespBody]),
            RespBody;
        'false' ->
            ?ERROR("failed to get any ~w: ~p: ~s"
                  ,[ExpectedCodes, ActualCode, RespBody]
                  ),
            {'error', RespBody}
    end;
handle_response(_ExtectedCode, {'error','socket_closed_remotely'}=E) ->
    ?ERROR("~nwe broke crossbar!"),
    throw(E);
handle_response(_ExpectedCode, {'ok', _ActualCode, _RespHeaders, RespBody}) ->
    ?ERROR("failed to get ~w: ~p: ~s", [_ExpectedCode, _ActualCode, RespBody]),
    {'error', RespBody};
handle_response(_ExpectedCode, {'error', _}=E) ->
    ?ERROR("broked req: ~p", [E]),
    E.


-spec start_trace(ne_binary()) -> {'ok', kz_data_tracing:trace_ref()}.
start_trace(RequestId) ->
    lager:md([{'request_id', RequestId}]),
    put('now', kz_time:now()),
    TraceFile = "/tmp/" ++ kz_term:to_list(RequestId) ++ ".log",

    {'ok', _}=OK = kz_data_tracing:trace_file([glc_ops:eq('request_id', RequestId)]
                                             ,TraceFile
                                             ,?TRACE_FORMAT
                                             ,get_log_level()
                                             ),
    ?INFO("authenticating...~s", [RequestId]),
    OK.

-spec set_log_level(atom()) -> atom().
set_log_level(LogLevel) ->
    put('log_level', LogLevel).

get_log_level() ->
    case get('log_level') of
        'undefined' -> 'debug';
        LogLevel -> LogLevel
    end.
