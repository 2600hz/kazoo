%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_websocket).

-export([recv/2
        ,info/2
        ]).

-type cb_ws_return() :: {cowboy_websocket:commands(), cb_context:context(), 'hibernate'}.

-spec recv(cow_ws:frame(), cb_context:context()) -> cb_ws_return().
recv({'text', Data}, State) ->
    lager:info("received request from client IP: ~s", [cb_context:client_ip(State)]),
    maybe_json(Data, State);
recv('ping', State) ->
    {[], State, 'hibernate'};
recv( Other, State) ->
    lager:debug("not handling message : ~p", [Other]),
    {[], State, 'hibernate'}.

-spec info(any(), cb_context:context()) -> cb_ws_return().
info({{_SenderPid, StreamId}, {'response', _HttpCode, #{<<"content-type">> := <<"application/json">>} = RespHeaders, Data}}, State) ->
    lager:info("received responce for request: ~s", [StreamId]),
    DataJObj = kz_json:decode(Data),
    HeadersJObj = response_headers(RespHeaders),
    Response = kz_json:merge_recursive(DataJObj,HeadersJObj),
    Text = kz_json:encode(Response),
    {[{'text', Text}], State, 'hibernate'};
info({{_SenderPid, StreamId}, {'response', _HttpCode, #{<<"content-type">> := _, <<"content-disposition">> := ContentDisposition} = RespHeaders, Data}}, State) ->
    lager:info("converting binary responce using base64 and package to JSON for websocket request: ~s", [StreamId]),
    [<<>>, Tail] = binary:split(ContentDisposition, <<"attachment; filename=\"">>),
    [Filename, <<>>] = binary:split(Tail, <<"\"">>),
    HeadersJObj = kz_json:insert_value(<<"filename">>, Filename, response_headers(RespHeaders)),
    Response = kz_json:insert_value(<<"content">>, base64:encode(Data), HeadersJObj),
    Text = kz_json:encode(Response),
    {[{'text', Text}], State, 'hibernate'};
info(Info, State) ->
    lager:info("unhandled websocket info: ~p", [Info]),
    {[], State, 'hibernate'}.

-spec maybe_json(iodata(), cb_context:context()) -> cb_ws_return().
maybe_json(Data, State) ->
    case decode_json_body(Data) of
        {'error', Msg} ->
            Req = fake_req(kz_json:new()),
            Context0 = api_resource:context_init(Req, []),
            Context1 = cb_context:set_resp_error_code(Context0, 400),
            Context2 = cb_context:add_validation_error(<<"json">>
                                                      ,<<"invalid">>
                                                      ,kz_json:from_list(
                                                         [{<<"message">>, Msg}
                                                         ,{<<"target">>, <<"body">>}
                                                         ]
                                                        )
                                                      , Context1
                                                      ),
            %% We stop processing here. Responce we receive via websocket_info callback and then send to client
            {'stop', _ , _} = api_util:stop(Req, Context2),
            {[], State, 'hibernate'};
        {'ok', JObj} ->
            Req0 = fake_req(JObj),
            {Qs, _} = api_util:get_query_string_data(Req0),
            Context0 = api_resource:context_init(Req0, []),
            %% Crossbar GET requests not have body. To emulate this case we check ".data" object presense
            {Context1, Req1} = api_util:set_request_data_in_context(Context0, Req0, crossbar_jobj(JObj), Qs),
            {'cowboy_rest', Req2, Context2} = api_resource:rest_init(Req1, Context1, []),
            _Rest = cowboy_rest:upgrade(Req2, #{}, 'api_resource', Context2),
            {[], State, 'hibernate'}
    end.

-spec crossbar_jobj(kz_json:object()) -> kz_json:api_object().
crossbar_jobj(JObj) ->
    case kz_json:is_defined(<<"data">>, JObj) of
        'true' -> JObj;
        'false'-> 'undefined'
    end.

-spec decode_json_body(kz_term:ne_binary()) -> {'ok', kz_json:object()} | {'error', kz_term:ne_binary()}.
decode_json_body(Data) ->
    try kz_json:unsafe_decode(Data) of
        JObj ->
            lager:debug("request has a json payload: ~s", [Data]),
            {'ok', api_util:normalize_envelope_keys(JObj)}
    catch
        'throw':{'invalid_json',{'error',{ErrLine, ErrMsg}}, _JSON} ->
            lager:debug("failed to decode json near ~p: ~s", [ErrLine, ErrMsg]),
            {'error', <<(kz_term:to_binary(ErrMsg))/binary, " around ", (kz_term:to_binary(ErrLine))/binary>>};
        _E:_R ->
            lager:debug("unknown catch from json decode ~p : ~p", [_E, _R]),
            throw(_R)
    end.


-spec fake_req(kz_json:object()) -> cowboy_req:req().
fake_req(JObj) ->
    Props = [{<<"content-type">>, <<"application/json">>}
            ,{<<"x-auth-token">>, kz_json:get_first_defined([<<"x-auth-token">>, <<"auth_token">>], JObj)}
            ],
    Headers = kz_json:to_map(kz_json:from_list(Props)),
    RawPath = kz_json:get_ne_binary_value(<<"path">>, JObj, <<"/v2">>),
    {Path, Qs} = split_path_qs(RawPath),

    RespHeaders = #{<<"content-type">> => <<"application/json">>},
    %% Generating fake cowboy request according cowboy_req:req() type (Cowboy 2.6.3 commit d846827b2a70317914621328bd617e4e5d4d13d8)
    %% only mandatory keys
    #{
       %% Public interface.
       method => kz_json:get_ne_binary_value(<<"method">>, JObj, <<"GET">>)
     ,version => 'HTTP/1.0'
     ,scheme => <<"https">> %% most of websocket connection over https
     ,host => <<"localhost">>
     ,port => 443
     ,path => Path
     ,qs => Qs
     ,headers => Headers
     ,peer => {{127,0,0,1}, 443}
     ,sock => {{127,0,0,1}, 443}
     ,cert => 'undefined'

       %% Private interface.
     ,ref => 'undefined'
     ,pid => self()
     ,streamid => get_request_id(JObj)

     ,has_body => 'true'
     ,body_length => 'undefined'
       %% we set application/json content-type by default, if response return other, then headers will be set by crossbar module
     ,resp_headers => RespHeaders
     }.

-spec split_path_qs(kz_term:ne_binary()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
split_path_qs(RawPath) ->
    case binary:split(RawPath, <<"?">>) of
        [Path] -> {Path, <<>>};
        [Path, Qs] -> {Path, Qs}
    end.

-spec get_request_id(kz_json:object()) -> kz_term:ne_binary().
get_request_id(JObj) ->
    ReqId = kz_json:get_ne_binary_value(<<"x-request-id">>, JObj, kz_datamgr:get_uuid()),
    kz_log:put_callid(ReqId),
    ReqId.

-spec response_headers(map()) -> kz_term:object().
response_headers(#{<<"access-control-expose-headers">> := KeysBinary} = Headers) ->
    Keys = lists:sort(binary:split(KeysBinary, <<",">>)),
    response_headers(lists:merge(Keys, well_know_resp_headers()), Headers, []);
response_headers(#{} = Headers) ->
    response_headers(well_know_resp_headers(), Headers, []).

-spec response_headers(kz_term:ne_binaries(), map(), kz_term:proplist()) -> kz_term:object().
response_headers([Key|Rest], #{} = Headers, Acc) ->
    case maps:is_key(Key, Headers) of
        'false' -> response_headers(Rest, Headers, Acc);
        'true' ->
            Value = maps:get(Key, Headers),
            Property = {Key, Value},
            response_headers(Rest, Headers, [Property | Acc])
    end;
response_headers([], #{}, Acc) ->
    kz_json:from_list(Acc).

-spec well_know_resp_headers() -> kz_term:ne_binaries().
well_know_resp_headers() ->
    lists:sort([<<"content-language">>
               ,<<"content-length">>
               ,<<"content-type">>
               ]).
