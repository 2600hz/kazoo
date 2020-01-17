%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Crossbar REST-related functions.
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @author Jon Blanton
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(api_util).

-export([is_cors_preflight/1
        ,is_cors_request/1
        ,add_cors_headers/2
        ,add_req_cors_headers/2
        ,allow_methods/3
        ,path_tokens/1
        ,parse_path_tokens/2
        ,get_req_data/2
        ,get_http_verb/2
        ,get_auth_token/2
        ,get_pretty_print/2
        ,get_content_type/1
        ,is_authentic/2, is_early_authentic/2
        ,is_permitted/2
        ,is_known_content_type/2
        ,does_resource_exist/1
        ,validate/1
        ,succeeded/1
        ,execute_request/2
        ,finish_request/2
        ,create_push_response/2, create_push_response/3
        ,set_resp_headers/2

         %% Content
        ,create_resp_content/2
        ,create_resp_body/2
        ,create_resp_file/2
        ,create_csv_resp_content/2
        ,create_binary_resp_content/2
        ,create_xml_resp_content/2

        ,create_pull_response/2, create_pull_response/3

        ,init_chunk_stream/3
        ,close_chunk_json_envelope/2
        ,create_json_chunk_response/2, create_csv_chunk_response/2

        ,stop/2
        ,content_type_matches/2
        ,ensure_content_type/1
        ,create_event_name/2

        ,encode_start_key/1, decode_start_key/1

        ,exec_req/2
        ,get_request_body/1
        ]).

-include_lib("kernel/include/file.hrl").
-include("crossbar.hrl").

-define(MAX_UPLOAD_SIZE, kapps_config:get_integer(?CONFIG_CAT, <<"max_upload_size">>, 8000000)). %% limit the whole payload/file size
-define(READ_BODY_LENGTH, 8000000). %% read body in 8MB chunks

-define(DATA_SCHEMA
       ,kz_json:from_list([{<<"type">>, [<<"object">>, <<"array">>]}
                          ,{<<"description">>, <<"The request data to be processed">>}
                          ,{<<"required">>, 'true'}
                          ])
       ).

-define(ENVELOPE_SCHEMA
       ,kz_json:from_list([{<<"properties">>
                           ,kz_json:from_list([{<<"data">>, ?DATA_SCHEMA}])
                           }
                          ])
       ).

-define(DEFAULT_JSON_ERROR_MSG, <<"All JSON must be valid">>).

-define(DEFAULT_CSV_FILE_NAME, <<"result.csv">>).

-type stop_return() :: {'stop', cowboy_req:req(), cb_context:context()}.
-type resp_file() :: {'sendfile',  non_neg_integer(),  non_neg_integer(), file:name_all()}.
-type resp_content_return() :: {kz_term:ne_binary() | iolist() | resp_file(), cowboy_req:req()}.
-type resp_content_fun() :: fun((cowboy_req:req(), cb_context:context()) ->  resp_content_return()).

-type req_fun_1() :: fun((cowboy_req:req()) -> cowboy_req:req()).
-type req_fun_2() :: fun((any(), cowboy_req:req()) -> cowboy_req:req()).
-type req_fun_3() :: fun((any(), any(), cowboy_req:req()) -> cowboy_req:req()).
-type req_fun() :: req_fun_1() | req_fun_2() | req_fun_3().
-export_type([req_fun/0
             ,req_fun_1/0
             ,req_fun_2/0
             ,req_fun_3/0
             ,req_funs/0
             ]).

-type req_kv() :: req_fun_1() |
                  {req_fun_2(), any()} |
                  {req_fun_3(), any(), any()}.
-type req_funs() :: [req_kv()].

%%------------------------------------------------------------------------------
%% @doc Attempts to determine if this is a cross origin resource preflight request
%% @end
%%------------------------------------------------------------------------------
-spec is_cors_preflight(cowboy_req:req()) -> boolean().
is_cors_preflight(Req) ->
    is_cors_request(Req)
        andalso ?HTTP_OPTIONS =:= cowboy_req:method(Req).

%%------------------------------------------------------------------------------
%% @doc Attempts to determine if this is a cross origin resource sharing request
%% @end
%%------------------------------------------------------------------------------
-spec is_cors_request(cowboy_req:req()) -> boolean().
is_cors_request(Req) ->
    ReqHdrs = [<<"origin">>, <<"access-control-request-method">>, <<"access-control-request-headers">>],
    is_cors_request(Req, ReqHdrs).

-spec is_cors_request(cowboy_req:req(), kz_term:ne_binaries()) -> boolean().
is_cors_request(_Req, []) -> 'false';
is_cors_request(Req, [ReqHdr|ReqHdrs]) ->
    'undefined' =/= cowboy_req:header(ReqHdr, Req)
        orelse is_cors_request(Req, ReqHdrs).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_req_cors_headers(cb_context:context(), cowboy_req:req()) ->
          cowboy_req:req().
add_req_cors_headers(Context, Req) ->
    add_cors_headers(Req, Context).

-spec add_cors_headers(cowboy_req:req(), cb_context:context()) ->
          cowboy_req:req().
add_cors_headers(Req, Context) ->
    ReqMethod = cowboy_req:header(<<"access-control-request-method">>, Req),

    Methods = [?HTTP_OPTIONS | cb_context:allow_methods(Context)],
    Allow = case kz_term:is_empty(ReqMethod)
                orelse lists:member(ReqMethod, Methods)
            of
                'false' -> [ReqMethod|Methods];
                'true' -> Methods
            end,

    lists:foldl(fun({H, V}, ReqAcc) ->
                        cowboy_req:set_resp_header(H, V, ReqAcc)
                end
               ,Req
               ,get_cors_headers(Allow)
               ).

-spec get_cors_headers(kz_term:ne_binaries()) -> kz_term:proplist().
get_cors_headers(Allow) ->
    [{<<"access-control-allow-origin">>, <<"*">>}
    ,{<<"access-control-allow-methods">>, kz_binary:join(Allow, <<", ">>)}
    ,{<<"access-control-allow-headers">>, <<"content-type, depth, user-agent, x-http-method-override, x-file-size, x-requested-with, if-modified-since, x-file-name, cache-control, x-auth-token, x-kazoo-cluster-id, if-match">>}
    ,{<<"access-control-expose-headers">>, <<"content-type, x-auth-token, x-request-id, x-kazoo-cluster-id, location, etag, etag">>}
    ,{<<"access-control-max-age">>, kz_term:to_binary(?SECONDS_IN_DAY)}
    ].

-spec get_req_data(cb_context:context(), cowboy_req:req()) ->
          {cb_context:context(), cowboy_req:req()} |
          stop_return().
get_req_data(Context, Req0) ->
    maybe_get_req_data(Context, Req0, cb_context:api_version(Context)).

-spec maybe_get_req_data(cb_context:context(), cowboy_req:req(), kz_term:ne_binary()) ->
          {cb_context:context(), cowboy_req:req()} |
          stop_return().
maybe_get_req_data(Context, Req, ?VERSION_1) ->
    lager:debug("unsupported version 1 request, stopping here..."),
    Message = <<"API version 1 is not supported. Please upgrade your code to use version 2.">>,
    Context1 = cb_context:add_system_error(410, 'gone', Message, Context),
    ?MODULE:stop(Req, Context1);
maybe_get_req_data(Context, Req0, _Version) ->
    {QS, Req1} = get_query_string_data(Req0),
    get_req_data(Context, Req1, get_content_type(Req1), QS).

-spec get_query_string_data(cowboy_req:req()) ->
          {kz_json:object(), cowboy_req:req()}.
get_query_string_data(Req) ->
    QS = cowboy_req:parse_qs(Req),
    get_query_string_data(QS, Req).

-spec get_query_string_data(kz_term:proplist(), cowboy_req:req()) ->
          {kz_json:object(), cowboy_req:req()}.
get_query_string_data([], Req) ->
    {kz_json:new(), Req};
get_query_string_data(QS0, Req) ->
    QS = kz_json:from_list(QS0),
    lager:debug("query string: ~s", [kz_json:encode(QS, ['pretty'])]),
    {QS, Req}.

-spec get_content_type(cowboy_req:req()) -> kz_term:api_ne_binary().
get_content_type(Req) ->
    case cowboy_req:parse_header(<<"content-type">>, Req) of
        'undefined' -> 'undefined';
        {Main, Sub, _Opts} -> <<Main/binary, "/", Sub/binary>>
    end.

-spec get_req_data(cb_context:context(), cowboy_req:req(), cowboy_content_type(), kz_json:object()) ->
          {cb_context:context(), cowboy_req:req()} |
          stop_return().
get_req_data(Context, Req0, 'undefined', QS) ->
    lager:debug("undefined content type when getting req data, assuming application/json"),
    case get_request_body(Req0) of
        {'ok', Body, Req1} ->
            Ctx = cb_context:set_req_header(Context, <<"content-type">>, ?DEFAULT_CONTENT_TYPE),
            try_json(Body, QS, Ctx, Req1);
        {'error', 'max_size', Req1} ->
            handle_max_filesize_exceeded(Context, Req1)
    end;
get_req_data(Context, Req, <<"multipart/form-data">>, QS) ->
    lager:debug("multipart/form-data content type when getting req data"),
    maybe_extract_multipart(cb_context:set_query_string(Context, QS), Req, QS);

%% cURL defaults to this content-type, so check it for JSON if parsing fails
get_req_data(Context, Req1, <<"application/x-www-form-urlencoded">>, QS) ->
    lager:debug("application/x-www-form-urlencoded content type when getting req data"),
    handle_failed_multipart(cb_context:set_query_string(Context, QS), Req1, QS);

get_req_data(Context, Req0, <<"application/json">>, QS) ->
    lager:debug("application/json content type when getting req data"),
    case get_request_body(Req0) of
        {'ok', Body, Req1} ->
            try_json(Body, QS, Context, Req1);
        {'error', 'max_size', Req1} ->
            handle_max_filesize_exceeded(Context, Req1)
    end;
get_req_data(Context, Req0, <<"application/x-json">>, QS) ->
    lager:debug("application/x-json content type when getting req data"),
    case get_request_body(Req0) of
        {'ok', Body, Req1} ->
            try_json(Body, QS, Context, Req1);
        {'error', 'max_size', Req1} ->
            handle_max_filesize_exceeded(Context, Req1)
    end;
get_req_data(Context, Req1, <<"application/base64">>, QS) ->
    lager:debug("application/base64 content type when getting req data"),
    decode_base64(cb_context:set_query_string(Context, QS), <<"application/base64">>, Req1);
get_req_data(Context, Req1, <<"application/x-base64">>, QS) ->
    lager:debug("application/x-base64 content type when getting req data"),
    decode_base64(cb_context:set_query_string(Context, QS), <<"application/base64">>, Req1);
get_req_data(Context, Req, <<"multipart/", C/binary>>, QS) ->
    lager:debug("multipart ~s content type when getting req data", [C]),
    maybe_extract_multipart(cb_context:set_query_string(Context, QS), Req, QS);
get_req_data(Context, Req, ContentType, QS) ->
    [{Mod, Params} | _] = cb_context:req_nouns(Context),
    Method = kz_term:to_lower_binary(cb_context:method(Context)),
    Event = create_event_name(Context, [<<"parse_body">>, Method, Mod]),
    Payload = [Req | Params],
    try crossbar_bindings:map(Event, Payload) of
        [{'ok', Body}] ->
            set_request_data_in_context(Context, Req, Body, QS);
        [{'error', Error}] ->
            {'stop', Req, cb_context:add_system_error(Error, Context)};
        _Else ->
            lager:debug("file's content-type: ~p", [ContentType]),
            extract_file(cb_context:set_query_string(Context, QS), ContentType, Req)
    catch
        _:_:_ -> extract_file(cb_context:set_query_string(Context, QS), ContentType, Req)
    end.

-spec maybe_extract_multipart(cb_context:context(), cowboy_req:req(), kz_json:object()) ->
          {cb_context:context(), cowboy_req:req()} |
          stop_return().
maybe_extract_multipart(Context, Req0, QS) ->
    try extract_multipart(Context, Req0, QS)
    catch
        ?STACKTRACE(_E, _R, ST)
        lager:debug("failed to extract multipart ~s: ~p", [_E, _R]),
        kz_log:log_stacktrace(ST),
        handle_failed_multipart(Context, Req0, QS)
        end.

-spec handle_failed_multipart(cb_context:context(), cowboy_req:req(), kz_json:object()) ->
          {cb_context:context(), cowboy_req:req()} |
          stop_return().
handle_failed_multipart(Context, Req0, QS) ->
    case get_request_body(Req0) of
        {'ok', ReqBody, Req1} ->
            try handle_url_encoded_body(Context, Req1, QS, ReqBody, get_url_encoded_body(ReqBody))
            catch
                _E:_R ->
                    lager:debug("failed to extract url-encoded request body: ~s: ~p", [_E, _R]),
                    try_json(ReqBody, QS, Context, Req1)
            end;
        {'error', 'max_size', Req1} ->
            handle_max_filesize_exceeded(Context, Req1)
    end.

-spec handle_url_encoded_body(cb_context:context(), cowboy_req:req(), kz_json:object(), binary(), kz_json:object()) ->
          {cb_context:context(), cowboy_req:req()} |
          stop_return().
handle_url_encoded_body(Context, Req, QS, ReqBody, JObj) ->
    case kz_json:get_values(JObj) of
        {['true'], [_JSON]} ->
            lager:debug("failed to parse url-encoded request body, but we'll give json a go on ~p", [_JSON]),
            try_json(ReqBody, QS, Context, Req);
        _Vs ->
            lager:debug("was able to parse request body as url-encoded to json: ~p", [JObj]),
            set_request_data_in_context(Context, Req, JObj, QS)
    end.

-spec set_request_data_in_context(cb_context:context(), cowboy_req:req(), kz_term:api_object(), kz_json:object()) ->
          {cb_context:context(), cowboy_req:req()} |
          stop_return().
set_request_data_in_context(Context, Req, 'undefined', QS) ->
    lager:debug("request json is empty"),
    {set_valid_data_in_context(Context, kz_json:new(), QS), Req};
set_request_data_in_context(Context, Req, JObj, QS) ->
    case is_valid_request_envelope(JObj, Context) of
        'true' ->
            lager:debug("request json is valid"),
            {set_valid_data_in_context(Context, JObj, QS), Req};
        Errors ->
            lager:info("failed to validate json request, invalid request"),
            ?MODULE:stop(Req
                        ,cb_context:failed(cb_context:set_resp_error_msg(Context, <<"invalid request envelope">>)
                                          ,Errors
                                          )
                        )
    end.

-spec set_valid_data_in_context(cb_context:context(), kz_json:object(), kz_json:object()) ->
          cb_context:context().
set_valid_data_in_context(Context, JObj, QS) ->
    Setters = [{fun cb_context:set_req_json/2, JObj}
              ,{fun cb_context:set_req_data/2, kz_json:get_value(<<"data">>, JObj, kz_json:new())}
              ,{fun cb_context:set_query_string/2, QS}
              ],
    cb_context:setters(Context, Setters).

-spec try_json(kz_term:ne_binary(), kz_json:object(), cb_context:context(), cowboy_req:req()) ->
          {cb_context:context(), cowboy_req:req()} |
          stop_return().
try_json(ReqBody, QS, Context, Req) ->
    try get_json_body(ReqBody, Req) of
        {{'malformed', Msg}, Req1} ->
            stop_on_invalid_envelope(Msg, Req1, Context);
        {JObj, Req1} ->
            set_request_data_in_context(Context, Req1, JObj, QS)
    catch
        _E:Reason ->
            lager:warning("failed to get json body: ~s: ~p", [_E, Reason]),
            stop_on_invalid_envelope(Req, Context)
    end.

-spec stop_on_invalid_envelope(kz_term:ne_binary(), cowboy_req:req(), cb_context:context()) ->
          stop_return().
stop_on_invalid_envelope(Msg, Req, Context) ->
    ?MODULE:stop(Req
                ,cb_context:add_validation_error(<<"json">>
                                                ,<<"invalid">>
                                                ,kz_json:from_list(
                                                   [{<<"message">>, Msg}
                                                   ,{<<"target">>, <<"body">>}
                                                   ]
                                                  )
                                                ,cb_context:set_resp_error_code(Context, 400)
                                                )
                ).

-spec stop_on_invalid_envelope(cowboy_req:req(), cb_context:context()) ->
          stop_return().
stop_on_invalid_envelope(Req, Context) ->
    stop_on_invalid_envelope(?DEFAULT_JSON_ERROR_MSG, Req, Context).

-spec get_url_encoded_body(kz_term:ne_binary()) -> kz_json:object().
get_url_encoded_body(ReqBody) ->
    kz_json:from_list(cow_qs:parse_qs(ReqBody)).

-type cowboy_multipart_response() :: {'ok', cow_multipart:headers(), cowboy_req:req()} |
                                     {'done', cowboy_req:req()} |
                                     cowboy_req:req().

-spec extract_multipart(cb_context:context(), cowboy_multipart_response(), kz_json:object()) ->
          {cb_context:context(), cowboy_req:req()} |
          stop_return().
extract_multipart(Context, {'done', Req}, _QS) ->
    {cb_context:store(Context, <<"multipart_headers">>, 'undefined'), Req};
extract_multipart(Context0, {'ok', Headers, Req0}, QS) ->
    Context = cb_context:store(Context0, <<"multipart_headers">>, Headers),
    %% TODO: maybe check all files and req_data to see they reached to maximum upload size.
    case get_req_data(Context, Req0, maps:get(<<"content-type">>, Headers, 'undefined'), QS) of
        {Context1, Req1} ->
            extract_multipart(Context1 ,cowboy_req:read_part(Req1), QS);
        {'stop', _, _}=Stop ->
            Stop
    end;
extract_multipart(Context, Req, QS) ->
    extract_multipart(Context
                     ,cowboy_req:read_part(Req)
                     ,QS
                     ).

-spec extract_file(cb_context:context(), kz_term:ne_binary(), cowboy_req:req()) ->
          {cb_context:context(), cowboy_req:req()} |
          stop_return().
extract_file(Context, ContentType, Req0) ->
    case get_request_body(Req0) of
        {'ok', FileContents, Req1} ->
            handle_file_contents(Context, ContentType, Req1, FileContents);
        {'error', 'max_size', Req1} ->
            handle_max_filesize_exceeded(Context, Req1)
    end.

-spec handle_file_contents(cb_context:context(), kz_term:ne_binary(), cowboy_req:req(), binary()) ->
          {cb_context:context(), cowboy_req:req()} |
          stop_return().
handle_file_contents(Context, ContentType, Req, FileContents) ->
    MultiPartHeaders = cb_context:fetch(Context, <<"multipart_headers">>, #{}),
    %% http://tools.ietf.org/html/rfc2045#page-17
    TransferKey = <<"content-transfer-encoding">>,

    case maps:get(TransferKey, MultiPartHeaders, cowboy_req:header(TransferKey, Req)) of
        <<"base64">> ->
            lager:debug("base64 encoded request coming in"),
            decode_base64(Context, ContentType, Req, FileContents);
        _Else ->
            lager:debug("unexpected transfer encoding: '~s'", [_Else]),
            ContentLength = maps:get(<<"content-length">>, MultiPartHeaders, cowboy_req:header(<<"content-length">>, Req)),
            FileHeaders = kz_json:from_list([{<<"content_type">>, ContentType}
                                            ,{<<"content_length">>, ContentLength}
                                            ]),
            FileJObj = kz_json:from_list([{<<"headers">>, FileHeaders}
                                         ,{<<"contents">>, FileContents}
                                         ]),
            lager:debug("request is a file upload of type: ~s", [ContentType]),

            Filename = uploaded_filename(Context),
            {cb_context:add_req_file(Context, {Filename, FileJObj}), Req}
    end.

-spec uploaded_filename(cb_context:context()) -> kz_term:ne_binary().
uploaded_filename(Context) ->
    case cb_context:req_value(Context, <<"filename">>) of
        'undefined' -> default_filename();
        Filename ->
            lager:debug("found filename on request: ~s", [Filename]),
            Filename
    end.

-spec default_filename() -> kz_term:ne_binary().
default_filename() ->
    <<"uploaded_file_", (kz_term:to_binary(kz_time:now_s()))/binary>>.

-spec decode_base64(cb_context:context(), kz_term:ne_binary(), cowboy_req:req()) ->
          {cb_context:context(), cowboy_req:req()} |
          stop_return().
decode_base64(Context, CT, Req0) ->
    case get_request_body(Req0) of
        {'ok', Base64Data, Req1} ->
            decode_base64(Context, CT, Req1, Base64Data);
        {'error', 'max_size', Req1} ->
            handle_max_filesize_exceeded(Context, Req1)
    end.

-spec decode_base64(cb_context:context(), kz_term:ne_binary(), cowboy_req:req(), binary()) ->
          {cb_context:context(), cowboy_req:req()} |
          stop_return().
decode_base64(Context, CT, Req, Base64Data) ->
    try kz_attachment:decode_base64(Base64Data) of
        {EncodedType, FileContents} ->
            ContentType = case EncodedType of
                              'undefined' -> CT;
                              {'error', 'badarg'} -> CT;
                              <<"application/base64">> -> <<"application/octet-stream">>;
                              Else -> Else
                          end,
            Headers = kz_json:from_list([{<<"content_type">>, ContentType}
                                        ,{<<"content_length">>, byte_size(FileContents)}
                                        ]),
            FileJObj = kz_json:from_list([{<<"headers">>, Headers}
                                         ,{<<"contents">>, FileContents}
                                         ]),
            lager:debug("request is a base64 file upload of type: ~s", [ContentType]),
            {cb_context:add_req_file(Context, {default_filename(), FileJObj}), Req}
    catch
        _T:_E ->
            lager:debug("failed to decode base64 data: ~p:~p", [_T,_E]),
            JObj = kz_json:from_list([{<<"message">>, <<"failed to decode base64 data">>}]),
            Context1 = cb_context:add_validation_error(<<"file">>
                                                      ,<<"encoding">>
                                                      ,JObj
                                                      ,cb_context:set_resp_error_code(Context, 413)
                                                      ),

            ?MODULE:stop(Req, Context1)
    end.

-spec get_request_body(cowboy_req:req()) ->
          {'ok', binary(), cowboy_req:req()} |
          {'error', 'max_size', cowboy_req:req()}.
get_request_body(Req) ->
    ReadLength = ?READ_BODY_LENGTH,
    MaxSize = ?MAX_UPLOAD_SIZE,

    Params = #{read_options => #{'length' => ReadLength}
              ,max_size => MaxSize
              },

    try get_request_body(Req, Params#{read_fun => fun cowboy_req:read_part_body/2})
    catch
        'error':{'badmatch', _} ->
            get_request_body(Req, Params#{read_fun => fun cowboy_req:read_body/2})
    end.

-type body_return() :: {'more', binary(), cowboy_req:req()} |
                       {'ok', binary(), cowboy_req:req()}.

-spec get_request_body(cowboy_req:req(), map()) ->
          {'ok', binary(), cowboy_req:req()} |
          {'error', 'max_size', cowboy_req:req()}.
get_request_body(Req, #{read_fun := ReadFun
                       ,read_options := Opts
                       }=Params) ->
    get_request_body(Params, <<>>, ReadFun(Req, Opts)).

-spec get_request_body(map(), binary(), body_return()) ->
          {'ok', binary(), cowboy_req:req()} |
          {'error', 'max_size', cowboy_req:req()}.
get_request_body(_, <<>>, {'ok', <<>>, Req1}) ->
    lager:debug("request body had no payload"),
    {'ok', <<>>, Req1};
get_request_body(#{max_size := MaxSize}, Body, {'more', Data, Req1})
  when size(Body) + size(Data) > MaxSize ->
    {'error', 'max_size', Req1};
get_request_body(#{read_fun := ReadFun, read_options := Opts}=Params, Body, {'more', Data, Req1}) ->
    get_request_body(Params, iolist_to_binary([Body, Data]), ReadFun(Req1, Opts));
get_request_body(_, Acc, {'ok', Data, Req1}) ->
    Body = iolist_to_binary([Acc, Data]),
    lager:debug("received request body payload (size: ~b bytes)", [size(Body)]),
    {'ok', Body, Req1}.

-type get_json_return() :: {kz_term:api_object(), cowboy_req:req()} |
                           {{'malformed', kz_term:ne_binary()}, cowboy_req:req()}.
-spec get_json_body(binary(), cowboy_req:req()) -> get_json_return().

get_json_body(<<>>, Req) -> {'undefined', Req};
get_json_body(ReqBody, Req) -> decode_json_body(ReqBody, Req).

-spec handle_max_filesize_exceeded(cb_context:context(), cowboy_req:req()) -> stop_return().
handle_max_filesize_exceeded(Context, Req1) ->
    Maximum = ?MAX_UPLOAD_SIZE,
    MaxSize = kz_term:to_binary(Maximum),

    lager:error("file size exceeded, max is ~p", [MaxSize]),

    Message = <<"Payload or Files must not be more than ", MaxSize/binary, " bytes">>,
    JObj = kz_json:from_list([{<<"message">>, Message}, {<<"target">>, Maximum}]),
    Context1 = cb_context:add_validation_error(<<"file">>
                                              ,<<"maxSize">>
                                              ,JObj
                                              ,cb_context:set_resp_error_code(Context, 413)
                                              ),

    ?MODULE:stop(Req1, Context1).

-spec decode_json_body(binary(), cowboy_req:req()) -> get_json_return().
decode_json_body(ReqBody, Req) ->
    try kz_json:unsafe_decode(ReqBody) of
        JObj ->
            lager:debug("request has a json payload: ~s", [ReqBody]),
            {normalize_envelope_keys(JObj), Req}
    catch
        'throw':{'invalid_json',{'error',{ErrLine, ErrMsg}}, _JSON} ->
            lager:debug("failed to decode json near ~p: ~s", [ErrLine, ErrMsg]),
            {{'malformed', <<(kz_term:to_binary(ErrMsg))/binary, " around ", (kz_term:to_binary(ErrLine))/binary>>}, Req};
        _E:_R ->
            lager:debug("unknown catch from json decode ~p : ~p", [_E, _R]),
            throw(_R)
    end.

%%------------------------------------------------------------------------------
%% @doc Normalizes envelope keys, sets envelope keys to lowercase.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_envelope_keys(kz_json:object()) -> kz_json:object().
normalize_envelope_keys(JObj) ->
    kz_json:foldl(fun normalize_envelope_keys_foldl/3, kz_json:new(), JObj).

-spec normalize_envelope_keys_foldl(kz_json:path(), kz_json:json_term(), kz_json:object()) -> kz_json:object().
normalize_envelope_keys_foldl(_K, 'undefined', JObj) -> JObj;
normalize_envelope_keys_foldl(K, V, JObj) -> kz_json:set_value(kz_json:normalize_key(K), V, JObj).

%%------------------------------------------------------------------------------
%% @doc Determines if the request envelope is valid.
%% @end
%%------------------------------------------------------------------------------
-spec is_valid_request_envelope(kz_json:object(), cb_context:context()) -> 'true' | validation_errors().
is_valid_request_envelope(Envelope, Context) ->
    case requires_envelope(Context) of
        'true' -> validate_request_envelope(Envelope);
        'false' -> 'true'
    end.

-spec requires_envelope(cb_context:context()) -> boolean().
requires_envelope(Context) ->
    Routines = [fun api_version_requires_envelope/1
               ,fun content_type_requires_envelope/1
               ,fun req_noun_requires_envelope/1
               ],
    lists:all(fun(F) -> F(Context) end, Routines).

-spec api_version_requires_envelope(cb_context:context()) -> boolean().
api_version_requires_envelope(Context) ->
    not lists:member(cb_context:api_version(Context), ?NO_ENVELOPE_VERSIONS).

-spec content_type_requires_envelope(cb_context:context()) -> boolean().
content_type_requires_envelope(Context) ->
    not lists:member(cb_context:req_header(Context, <<"content-type">>), ?NO_ENVELOPE_CONTENT_TYPES).

-spec req_noun_requires_envelope(cb_context:context()) -> boolean().
req_noun_requires_envelope(Context) ->
    req_noun_requires_envelope(Context, cb_context:req_nouns(Context)).

-spec req_noun_requires_envelope(cb_context:context(), req_nouns()) -> boolean().
req_noun_requires_envelope(_Context, []) -> 'true';
req_noun_requires_envelope(Context, [{Mod, Params} | _]) ->
    Event = create_event_name(Context, <<"requires_envelope.", Mod/binary>>),
    Payload = [Context | Params],
    try crossbar_bindings:pmap(Event, Payload) of
        [Value | _] when is_boolean(Value) -> Value;
        _Else -> 'true'
    catch
        _:_:_ -> 'true'
    end.

-spec validate_request_envelope(kz_json:object()) -> 'true' | validation_errors().
validate_request_envelope(Envelope) ->
    case kz_json_schema:validate(?ENVELOPE_SCHEMA, Envelope) of
        {'ok', _} -> 'true';
        {'error', Errors} -> Errors
    end.

-spec get_http_verb(http_method(), cb_context:context()) -> kz_term:ne_binary().
get_http_verb(Method, Context) ->
    case cb_context:req_value(Context, <<"verb">>) of
        'undefined' -> Method;
        Verb ->
            lager:debug("found verb ~s on request, using instead of ~s", [Verb, Method]),
            kz_term:to_upper_binary(Verb)
    end.

%%------------------------------------------------------------------------------
%% @doc This function will loop over the Tokens in the request path and return
%% a proplist with keys being the module and values a list of parameters
%% supplied to that module.  If the token order is improper a empty list
%% is returned.
%% @end
%%------------------------------------------------------------------------------
-spec path_tokens(cb_context:context()) -> kz_term:ne_binaries().
path_tokens(Context) ->
    Api = cb_context:api_version(Context),
    case cb_context:path_tokens(Context) of
        [<<>>, Api | Tokens] -> Tokens;
        [Api | Tokens] -> Tokens
    end.

-type cb_mod_with_tokens() :: {kz_term:ne_binary(), path_tokens()}.
-type cb_mods_with_tokens() :: [cb_mod_with_tokens()].

-spec parse_path_tokens(cb_context:context(), path_tokens()) ->
          cb_mods_with_tokens() |
          {'stop', cb_context:context()}.
parse_path_tokens(Context, Tokens) ->
    parse_path_tokens(Context, Tokens, []).

-spec parse_path_tokens(cb_context:context(), kz_json:path(), cb_mods_with_tokens()) ->
          cb_mods_with_tokens() |
          {'stop', cb_context:context()}.
parse_path_tokens(_Context, [], Events) -> Events;
parse_path_tokens(_Context, [<<>>], Events) -> Events;
parse_path_tokens(_Context, [<<"schemas">>=Mod|T], Events) ->
    [{Mod, T} | Events];
parse_path_tokens(_Context, [<<"braintree">>=Mod|T], Events) ->
    [{Mod, T} | Events];
parse_path_tokens(_Context, [<<"system_configs">>=Mod|T], Events) ->
    [{Mod, T} | Events];
parse_path_tokens(_Context, [<<"configs">>=Mod|T], Events) ->
    [{Mod, T} | Events];
parse_path_tokens(_Context, [<<"sup">>=Mod|T], Events) ->
    [{Mod, cb_sup:format_path_tokens(T)} | Events];
parse_path_tokens(Context, [<<"account">>|T], Events) ->
    case cb_context:auth_account_id(Context) of
        'undefined' ->
            lager:info("/account alias not available, request fails"),
            {'stop', crossbar_util:response_401(Context)};
        AuthAccountId ->
            lager:info("aliasing /account to /accounts/~s", [AuthAccountId]),
            parse_path_tokens(Context, [<<"accounts">>, AuthAccountId | T], Events)
    end;
parse_path_tokens(Context, [Mod|T], Events) ->
    case is_cb_module(Context, Mod) of
        'false' -> [];
        'true' ->
            {Params, List2} = lists:splitwith(fun(Elem) -> not is_cb_module(Context, Elem) end, T),
            parse_path_tokens(Context, List2, [{Mod, Params} | Events])
    end.

-spec is_cb_module(cb_context:context(), kz_term:ne_binary()) -> boolean().
is_cb_module(Context, Elem) ->
    ApiVersion = cb_context:api_version(Context),

    Modules = [<<"cb_", Elem/binary>>
              ,<<"cb_", Elem/binary, "_", ApiVersion/binary>>
              ],

    is_cb_module(Modules).

-spec is_cb_module(kz_term:ne_binaries()) -> boolean().
is_cb_module([]) -> 'false';
is_cb_module([Module|Modules]) ->
    'true' =:= kz_module:is_exported(Module, 'init', 0)
        orelse is_cb_module(Modules).

%%------------------------------------------------------------------------------
%% @doc This function will find the intersection of the allowed methods
%% among event responses.  The responses can only veto the list of
%% methods, they can not add.
%%
%% If a client passes a `?verb=(PUT|DELETE)' on a `POST' request, `ReqVerb' will
%% be `?HTTP_PUT' or `?HTTP_DELETE', while `HttpVerb' is `POST'. If the allowed
%% methods do not include `POST', we need to add it if allowed methods include
%% the verb in `ReqVerb'.
%% So, POSTing a `?HTTP_PUT', and the allowed methods include `PUT', insert `POST'
%% as well.
%% POSTing a `?HTTP_DELETE', and `DELETE' is NOT in the allowed methods, remove
%% `POST' from the allowed methods.
%% @end
%%------------------------------------------------------------------------------
-spec allow_methods([http_methods()], kz_term:ne_binary(), http_method()) -> http_methods().
allow_methods(Responses, ReqVerb, HttpVerb) ->
    case crossbar_bindings:succeeded(Responses) of
        [] -> [];
        Succeeded ->
            AllowedSet = lists:foldr(fun allow_methods_fold/2
                                    ,sets:new()
                                    ,Succeeded
                                    ),
            maybe_add_post_method(ReqVerb, HttpVerb, sets:to_list(AllowedSet))
    end.

-spec allow_methods_fold(http_methods(), sets:set()) -> sets:set().
allow_methods_fold(Response, Acc) ->
    sets:union(Acc, sets:from_list(uppercase_all(Response))).

-spec uppercase_all(kz_term:ne_binaries() | kz_term:atoms()) -> kz_term:ne_binaries().
uppercase_all(L) when is_list(L) ->
    [kz_term:to_upper_binary(kz_term:to_binary(I)) || I <- L].

%%------------------------------------------------------------------------------
%% @doc Insert `POST' if Verb is in allowed, otherwise remove `POST'.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_post_method(kz_term:ne_binary(), http_method(), http_methods()) -> http_methods().
maybe_add_post_method(?HTTP_POST, ?HTTP_POST, Allowed) ->
    Allowed;
maybe_add_post_method(Verb, ?HTTP_POST, Allowed) ->
    BigVerb = kz_term:to_upper_binary(Verb),
    case lists:member(BigVerb, Allowed) of
        'true' -> [?HTTP_POST | Allowed];
        'false' -> lists:delete(?HTTP_POST, Allowed)
    end;
maybe_add_post_method(_, _, Allowed) ->
    Allowed.

%%------------------------------------------------------------------------------
%% @doc This function will use event bindings to determine if the client has
%% provided a valid authentication token.
%% @end
%%------------------------------------------------------------------------------
-spec is_early_authentic(cowboy_req:req(), cb_context:context()) ->
          {'true', cowboy_req:req(), cb_context:context()} |
          stop_return().
is_early_authentic(Req, Context) ->
    Event = create_event_name(Context, <<"early_authenticate">>),
    case crossbar_bindings:succeeded(crossbar_bindings:pmap(Event, Context)) of
        [] ->
            {'true', Req, Context};
        ['true'|T] ->
            prefer_new_context(T, Req, Context, 'true');
        [{'true', Context1}|_T] ->
            lager:debug("one true context: ~p", [_T]),
            {'true', Req, Context1};
        [{'stop', Context1}|_] ->
            lager:debug("pre-authn stopped"),
            ?MODULE:stop(Req, Context1)
    end.

-spec is_authentic(cowboy_req:req(), cb_context:context()) ->
          {{'false', <<>>} | 'true', cowboy_req:req(), cb_context:context()} |
          stop_return().
is_authentic(Req, Context) ->
    is_authentic(Req, Context, cb_context:req_verb(Context)).

-spec is_authentic(cowboy_req:req(), cb_context:context(), http_method()) ->
          {boolean(), cowboy_req:req(), cb_context:context()} |
          stop_return().
is_authentic(Req, Context, ?HTTP_OPTIONS) ->
    %% all OPTIONS, they are harmless (I hope) and required for CORS preflight
    {'true', Req, Context};
is_authentic(Req, Context0, _ReqVerb) ->
    Event = create_event_name(Context0, <<"authenticate">>),
    case cb_context:auth_doc(Context0) =:= 'undefined'
        andalso crossbar_bindings:succeeded(crossbar_bindings:pmap(Event, Context0))
    of
        'false' ->
            {'true', Req, Context0};
        [] ->
            is_authentic(Req, Context0, _ReqVerb, cb_context:req_nouns(Context0));
        ['true'|T] ->
            prefer_new_context(T, Req, Context0);
        [{'true', Context1}|_] ->
            {'true', Req, Context1};
        [{'stop', Context1}|_] ->
            lager:debug("authn stopped"),
            ?MODULE:stop(Req, Context1)
    end.

-spec is_authentic(cowboy_req:req(), cb_context:context(), http_method(), list()) ->
          {{'false', <<>>} | 'true', cowboy_req:req(), cb_context:context()} |
          stop_return().

is_authentic(Req, Context, _ReqVerb, []) ->
    lager:debug("failed to authenticate"),
    ?MODULE:stop(Req, cb_context:add_system_error('invalid_credentials', Context));
is_authentic(Req, Context, _ReqVerb, [{Mod, Params} | _ReqNouns]) ->
    Event = create_event_name(Context, <<"authenticate.", Mod/binary>>),
    Payload = [Context | Params],
    case crossbar_bindings:succeeded(crossbar_bindings:pmap(Event, Payload)) of
        [] ->
            lager:debug("failed to authenticate : ~p", [Mod]),
            ?MODULE:stop(Req, cb_context:add_system_error('invalid_credentials', Context));
        ['true'|T] ->
            prefer_new_context(T, Req, Context);
        [{'true', Context2}|_] ->
            {'true', Req, Context2};
        [{'stop', Context2}|_] ->
            lager:debug("authn stopped"),
            ?MODULE:stop(Req, Context2)
    end.

-spec prefer_new_context(kz_term:proplist(), cowboy_req:req(), cb_context:context()) ->
          {'true', cowboy_req:req(), cb_context:context()} |
          stop_return().
prefer_new_context(Results, Req, Context) ->
    prefer_new_context(Results, Req, Context, 'undefined').

prefer_new_context([], Req, Context, 'false') ->
    {'false', Req, Context};
prefer_new_context([], Req, Context, _Return) ->
    {'true', Req, Context};

prefer_new_context([{'true', Context1}|_], Req, _Context, _Return) ->
    {'true', Req, Context1};
prefer_new_context(['true'|T], Req, Context, _Return) ->
    prefer_new_context(T, Req, Context, 'true');

prefer_new_context(['false'|T], Req, Context, 'undefined') ->
    prefer_new_context(T, Req, Context, 'false');
prefer_new_context(['false'|T], Req, Context, 'false') ->
    prefer_new_context(T, Req, Context, 'false');

prefer_new_context([{'stop', Context1}|_], Req, _Context, _Return) ->
    lager:debug("authn stopped"),
    ?MODULE:stop(Req, Context1).

-spec get_auth_token(cowboy_req:req(), cb_context:context()) -> cb_cowboy_payload().
get_auth_token(Req, Context) ->
    case cowboy_req:header(<<"x-auth-token">>, Req) of
        'undefined' ->
            case cb_context:req_value(Context, <<"auth_token">>) of
                'undefined' -> get_authorization_token(Req, Context);
                Token ->
                    lager:debug("using auth token found"),
                    {Req, set_auth_context(Context, Token, 'x-auth-token')}
            end;
        Token ->
            lager:debug("using auth token from header"),
            {Req, set_auth_context(Context, Token, 'x-auth-token')}
    end.

-spec get_authorization_token(cowboy_req:req(), cb_context:context()) -> cb_cowboy_payload().
get_authorization_token(Req, Context) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        'undefined' ->
            case cb_context:req_value(Context, <<"authorization">>) of
                'undefined' ->
                    lager:debug("no auth token found"),
                    {Req, Context};
                Authorization ->
                    lager:debug("using token ~s from url", [Authorization]),
                    {Req, set_auth_context(Context, Authorization)}
            end;
        Authorization ->
            lager:debug("using token ~s from header", [Authorization]),
            {Req, set_auth_context(Context, Authorization)}
    end.

-spec set_auth_context(cb_context:context(), kz_term:ne_binary() | {kz_term:ne_binary(), atom()}) ->
          cb_context:context().
set_auth_context(Context, {Token, TokenType}) ->
    set_auth_context(Context, Token, TokenType);
set_auth_context(Context, Authorization) ->
    set_auth_context(Context, get_authorization_token_type(Authorization)).

-spec set_auth_context(cb_context:context(), kz_term:ne_binary(), atom()) ->
          cb_context:context().
set_auth_context(Context, Token, TokenType) ->
    cb_context:setters(Context, [{fun cb_context:set_auth_token/2, Token}
                                ,{fun cb_context:set_auth_token_type/2, TokenType}
                                ]
                      ).

-spec get_authorization_token_type(kz_term:ne_binary()) -> {kz_term:ne_binary(), atom()}.
get_authorization_token_type(<<"Basic ", Token/binary>>) -> {Token, 'basic'};
get_authorization_token_type(<<"Bearer ", Token/binary>>) -> {Token, 'oauth'};
get_authorization_token_type(Token) -> {Token, 'unknown'}.

%%------------------------------------------------------------------------------
%% @doc This function will try to find pretty print option inside the headers
%% if not found will look into the request to find the options
%% otherwise will result in false.
%% @end
%%------------------------------------------------------------------------------
-spec get_pretty_print(cowboy_req:req(), cb_context:context()) -> cb_cowboy_payload().
get_pretty_print(Req, Context) ->
    case cowboy_req:header(<<"x-pretty-print">>, Req) of
        'undefined' ->
            case cb_context:req_value(Context, <<"pretty_print">>) of
                'undefined' -> {Req, cb_context:set_pretty_print(Context, 'false')};
                Value ->
                    lager:debug("using pretty print value from inside the request"),
                    {Req, cb_context:set_pretty_print(Context, kz_term:is_true(Value))}
            end;
        Value ->
            lager:debug("found pretty print options inside header"),
            {Req, cb_context:set_pretty_print(Context, kz_term:is_true(Value))}
    end.

%%------------------------------------------------------------------------------
%% @doc This function will use event bindings to determine if the client is
%% authorized for this request.
%% @end
%%------------------------------------------------------------------------------
-spec is_permitted(cowboy_req:req(), cb_context:context()) ->
          {'true', cowboy_req:req(), cb_context:context()} |
          stop_return().
is_permitted(Req, Context) ->
    is_permitted_verb(Req, Context, cb_context:req_verb(Context)).

-spec is_permitted_verb(cowboy_req:req(), cb_context:context(), http_method()) ->
          {'true', cowboy_req:req(), cb_context:context()} |
          stop_return().
is_permitted_verb(Req, Context, ?HTTP_OPTIONS) ->
    lager:debug("options requests are permitted by default"),
    %% all all OPTIONS, they are harmless (I hope) and required for CORS preflight
    {'true', Req, Context};
is_permitted_verb(Req, Context0, _ReqVerb) ->
    Event = create_event_name(Context0, <<"authorize">>),
    case crossbar_bindings:succeeded(crossbar_bindings:pmap(Event, Context0)) of
        [] ->
            is_permitted_nouns(Req, Context0, _ReqVerb,cb_context:req_nouns(Context0));
        ['true'|_] ->
            is_permitted_verb_on_module(Req, Context0, _ReqVerb,cb_context:req_nouns(Context0));
        [{'true', Context1}|_] ->
            is_permitted_verb_on_module(Req, Context1, _ReqVerb,cb_context:req_nouns(Context1));
        [{'stop', Context1}|_] ->
            lager:debug("authz stopped"),
            ?MODULE:stop(Req, Context1)
    end.

-spec is_permitted_verb_on_module(cowboy_req:req(), cb_context:context(), http_method(), list()) ->
          {'true', cowboy_req:req(), cb_context:context()} |
          stop_return().
is_permitted_verb_on_module(Req, Context0, _ReqVerb, [{Mod, Params} | _ReqNouns]) ->
    Event = create_event_name(Context0, <<"authorize.", Mod/binary>>),
    Payload = [Context0 | Params],
    case crossbar_bindings:succeeded(crossbar_bindings:pmap(Event, Payload)) of
        [{'stop', Context1}|_] ->
            lager:debug("authz stopped"),
            ?MODULE:stop(Req, Context1);
        _Other -> {'true', Req, Context0}
    end.

-spec is_permitted_nouns(cowboy_req:req(), cb_context:context(), http_method(), list()) ->
          {'true', cowboy_req:req(), cb_context:context()} |
          stop_return().
is_permitted_nouns(Req, Context, _ReqVerb, [{<<"404">>, []}]) ->
    ?MODULE:stop(Req, cb_context:add_system_error('not_found', Context));
is_permitted_nouns(Req, Context, _ReqVerb, []) ->
    lager:debug("no one authz'd the request"),
    ?MODULE:stop(Req, cb_context:add_system_error('forbidden', Context));
is_permitted_nouns(Req, Context0, _ReqVerb, [{Mod, Params} | _ReqNouns]) ->
    Event = create_event_name(Context0, <<"authorize.", Mod/binary>>),
    Payload = [Context0 | Params],
    case crossbar_bindings:succeeded(crossbar_bindings:pmap(Event, Payload)) of
        [] ->
            lager:debug("failed to authorize : ~p", [Mod]),
            ?MODULE:stop(Req, cb_context:add_system_error('forbidden', Context0));
        ['true'|_] ->
            {'true', Req, Context0};
        [{'true', Context1}|_] ->
            {'true', Req, Context1};
        [{'stop', Context1}|_] ->
            lager:debug("authz stopped"),
            ?MODULE:stop(Req, Context1)
    end.

-spec is_known_content_type(cowboy_req:req(), cb_context:context()) ->
          {boolean(), cowboy_req:req(), cb_context:context()}.
is_known_content_type(Req, Context) ->
    is_known_content_type(Req, Context, cb_context:req_verb(Context)).

is_known_content_type(Req, Context, ?HTTP_OPTIONS) ->
    lager:debug("ignore content type for options"),
    {'true', Req, Context};
is_known_content_type(Req, Context, ?HTTP_GET) ->
    lager:debug("ignore content type for get"),
    {'true', Req, Context};
is_known_content_type(Req, Context, ?HTTP_DELETE) ->
    lager:debug("ignore content type for delete"),
    {'true', Req, Context};
is_known_content_type(Req0, Context0, _ReqVerb) ->
    Context1 =
        lists:foldr(fun({Mod, Params}, ContextAcc) ->
                            Event = create_event_name(Context0, <<"content_types_accepted.", Mod/binary>>),
                            Payload = [ContextAcc | Params],
                            crossbar_bindings:fold(Event, Payload)
                    end
                   ,Context0
                   ,cb_context:req_nouns(Context0)
                   ),

    CT = get_content_type(Req0),

    is_known_content_type(Req0, Context1, ensure_content_type(CT), cb_context:content_types_accepted(Context1)).

-spec is_known_content_type(cowboy_req:req(), cb_context:context(), cowboy_content_type(), crossbar_content_handlers()) ->
          {boolean(), cowboy_req:req(), cb_context:context()}.
is_known_content_type(Req, Context, CT, []) ->
    is_known_content_type(Req, Context, CT, ?CONTENT_ACCEPTED);
is_known_content_type(Req, Context, CT, CTAs) ->
    IsAcceptable = is_acceptable_content_type(CT, CTAs),
    lager:debug("is ~p acceptable content type: ~s", [CT, IsAcceptable]),
    {IsAcceptable, Req, cb_context:set_content_types_accepted(Context, CTAs)}.

-spec is_acceptable_content_type(cowboy_content_type(), crossbar_content_handlers()) -> boolean().
is_acceptable_content_type(CTA, ContentHandlers) ->
    lists:any(fun({_Fun, ModCTAs}) ->
                      content_type_matches(CTA, ModCTAs)
              end
             ,ContentHandlers
             ).

%% (ClientContentType, ModuleContentType)
-spec content_type_matches(cowboy_content_type(), [cowboy_content_type()] | cowboy_content_type()) -> boolean().
content_type_matches(CT, ModCTs) when is_list(ModCTs) ->
    lists:any(fun(ModCT) -> content_type_matches(CT, ModCT) end, ModCTs);
content_type_matches({Type, _, _}, {Type, <<"*">>, '*'}) ->
    'true';
content_type_matches({Type, SubType, _}, {Type, SubType, '*'}) ->
    'true';
content_type_matches({Type, SubType, Opts}, {Type, SubType, ModOpts}) ->
    lists:all(fun({K, V}) -> props:get_value(K, Opts) =:= V end
             ,ModOpts
             );
content_type_matches(<<CTA/binary>>, {CT, SubCT, _}) ->
    CTA =:= <<CT/binary, "/", SubCT/binary>>;
content_type_matches(<<CT/binary>>, <<CT/binary>>) -> 'true';
content_type_matches(_CTA, _CTAs) -> 'false'.

-spec ensure_content_type(cowboy_content_type() | 'undefined') -> cowboy_content_type().
ensure_content_type('undefined') -> ?CROSSBAR_DEFAULT_CONTENT_TYPE;
ensure_content_type(<<CT/binary>>) -> CT;
ensure_content_type({_Type, _SubType, _Options}=CT) -> CT.

%%------------------------------------------------------------------------------
%% @doc This function will use event bindings to determine if the target noun
%% (the final module in the chain) accepts this verb parameter pair.
%% @end
%%------------------------------------------------------------------------------
-spec does_resource_exist(cb_context:context()) -> boolean().
does_resource_exist(Context) ->
    does_resource_exist(Context, cb_context:req_nouns(Context)).

-spec does_resource_exist(cb_context:context(), list()) -> boolean().
does_resource_exist(Context, [{Mod, Params}|_]) ->
    Event = create_event_name(Context, <<"resource_exists.", Mod/binary>>),
    Responses = crossbar_bindings:map(Event, Params),
    crossbar_bindings:any(Responses) and 'true';
does_resource_exist(_Context, _ReqNouns) ->
    'false'.

%%------------------------------------------------------------------------------
%% @doc This function gives each `{Mod, Params}' pair a chance to determine if
%% it is valid and returns the status, and any errors.
%%
%% validate_resource for each `{Mod, Params}' pair
%% validate for LAST `{Mod, Params}' pair
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate(Context, cb_context:req_nouns(Context)).

-spec validate(cb_context:context(), list()) -> cb_context:context().
validate(Context, ReqNouns) ->
    Context1 = validate_resources(Context, ReqNouns),
    case succeeded(Context1) of
        'true' ->
            Context2 = validate_data(Context1, ReqNouns),
            case succeeded(Context2) of
                'true' -> process_billing(Context2);
                'false' ->
                    lager:debug("validating data failed"),
                    Context2
            end;
        'false' ->
            lager:debug("validating resources failed"),
            Context1
    end.

-spec validate_data(cb_context:context(), list()) -> cb_context:context().
validate_data(Context, [{Mod, Params}|_]) ->
    Event = create_event_name(Context, <<"validate.", Mod/binary>>),
    Payload = [cb_context:set_resp_status(Context, 'fatal') | Params],
    cb_context:import_errors(crossbar_bindings:fold(Event, Payload)).

-spec validate_resources(cb_context:context(), list()) -> cb_context:context().
validate_resources(Context, ReqNouns) ->
    cb_context:import_errors(
      lists:foldr(fun validate_resources_fold/2
                 ,cb_context:set_resp_status(Context, 'success')
                 ,ReqNouns
                 )).

-spec validate_resources_fold(req_noun(), cb_context:context()) -> cb_context:context().
validate_resources_fold({Mod, Params}, ContextAcc) ->
    Event = create_event_name(ContextAcc, <<"validate_resource.", Mod/binary>>),
    Payload = [ContextAcc | Params],
    crossbar_bindings:fold(Event, Payload).

%%------------------------------------------------------------------------------
%% @doc This function will use event bindings to determine if the client is
%% authorized for this request.
%% @end
%%------------------------------------------------------------------------------
-spec process_billing(cb_context:context()) -> cb_context:context().
process_billing(Context)->
    Event = create_event_name(Context, <<"billing">>),
    process_billing_response(Context, crossbar_bindings:fold(Event, Context)).

process_billing_response(Context, NewContext) ->
    case cb_context:is_context(NewContext) of
        'true' ->
            lager:debug("billing returned ~p", [cb_context:resp_status(NewContext)]),
            NewContext;
        'false' ->
            lager:debug("billing failed: ~p", [NewContext]),
            Context
    end.

%%------------------------------------------------------------------------------
%% @doc This function determines if the response is of type success.
%% @end
%%------------------------------------------------------------------------------
-spec succeeded(cb_context:context()) -> boolean().
succeeded(Context) ->
    is_successful_status(cb_context:resp_status(Context)).

-spec is_successful_status(crossbar_status()) -> boolean().
is_successful_status('success') -> 'true';
is_successful_status('accepted') -> 'true';
is_successful_status(_Status) -> 'false'.

-spec execute_request(cowboy_req:req(), cb_context:context()) ->
          {boolean() | 'stop', cowboy_req:req(), cb_context:context()}.
execute_request(Req, Context) ->
    case cb_context:req_nouns(Context) of
        [{Mod, Params}|_] ->
            execute_request(Req, Context, Mod, Params, cb_context:req_verb(Context));
        _ReqNouns ->
            {'false', Req, Context}
    end.

-spec execute_request(cowboy_req:req(), cb_context:context(), kz_term:ne_binary(), kz_term:ne_binaries(), http_method()) ->
          {boolean() | 'stop', cowboy_req:req(), cb_context:context()}.
execute_request(Req, Context, Mod, Params, Verb) ->
    Event = create_event_name(Context, [<<"execute">>
                                       ,kz_term:to_lower_binary(Verb)
                                       ,Mod
                                       ]),
    Payload = [Context | Params],
    Context2 = crossbar_bindings:fold(Event, Payload),

    case cb_context:is_context(Context2) of
        'true' ->
            execute_request_results(Req, Context2, cb_context:resp_status(Context2));
        'false' ->
            execute_request_failure(Req, Context, Context2)
    end.

-spec execute_request_failure(cowboy_req:req(), cb_context:context(), any()) ->
          {'false', cowboy_req:req(), cb_context:context()}.
execute_request_failure(Req, Context, {'error', _E}) ->
    lager:debug("error executing request: ~p", [_E]),
    {'false', Req, Context};
execute_request_failure(Req, Context, _E) ->
    lager:debug("unexpected return from the fold: ~p", [_E]),
    {'false', Req, Context}.

-spec execute_request_results(cowboy_req:req(), cb_context:context(), crossbar_status()) ->
          {'true' | 'stop', cowboy_req:req(), cb_context:context()}.
execute_request_results(Req, Context, 'success') ->
    {'true', Req, Context};
execute_request_results(Req, Context, 'accepted') ->
    {'true', Req, Context};
execute_request_results(Req, Context, _RespStatus) ->
    ?MODULE:stop(Req, Context).

%%------------------------------------------------------------------------------
%% @doc This function runs the request terminated bindings at the conclusion
%% of all requests.
%% @end
%%------------------------------------------------------------------------------
-spec finish_request(cowboy_req:req(), cb_context:context()) -> 'ok'.
finish_request(_Req, Context) ->
    [{Mod, _}|_] = cb_context:req_nouns(Context),
    Verb = cb_context:req_verb(Context),
    Event = create_event_name(Context, [<<"finish_request">>, Verb, Mod]),
    _ = kz_process:spawn(fun crossbar_bindings:pmap/2, [Event, Context]),
    maybe_cleanup_file(cb_context:resp_file(Context)).

-spec maybe_cleanup_file(binary()) -> 'ok'.
maybe_cleanup_file(<<>>) -> 'ok';
maybe_cleanup_file(File) ->
    _P = spawn(fun() -> cleanup_file(File) end),
    lager:debug("deleting ~s in ~p", [File, _P]).

-spec cleanup_file(file:filename_all()) -> 'ok'.
cleanup_file(File) ->
    'ok' = file:delete(File),
    lager:debug("deleted file ~s", [File]).

-spec create_resp_body(cb_context:context(), cowboy_req:req()) -> cowboy_req:req().
create_resp_body(Context, Req0) ->
    {Content, Req} = create_resp_content(Req0, Context),
    cowboy_req:set_resp_body(Content, Req).

%%------------------------------------------------------------------------------
%% @doc This function will create the content for the response body.
%% @end
%%------------------------------------------------------------------------------
-spec create_resp_content(cowboy_req:req(), cb_context:context()) ->
          {kz_term:ne_binary() | iolist(), cowboy_req:req()}.
create_resp_content(Req0, Context) ->
    Resp = create_resp_envelope(Context),
    Options = get_encode_options(Context),
    try kz_json:encode(Resp, Options) of
        JSON ->
            case cb_context:req_value(Context, <<"jsonp">>) of
                'undefined' ->
                    {JSON
                    ,Req0
                    };
                JsonFun when is_binary(JsonFun) ->
                    lager:debug("jsonp wrapping in ~s: ~p", [JsonFun, JSON]),
                    {[JsonFun, <<"(">>, JSON, <<");">>]
                    ,cowboy_req:set_resp_header(<<"content-type">>, ?JSONP_CONTENT_TYPE, Req0)
                    }
            end
    catch
        'throw':{'json_encode', {'bad_term', _Term}} ->
            lager:debug("json encoding failed on ~p : ~p", [_Term, Resp]),
            {<<"encoding response failed: bad term">>, Req0};
        _E:_R ->
            lager:debug("failed to encode response: ~s: ~p : ~p", [_E, _R, Resp]),
            {<<"failure in request, contact support">>, Req0}
    end.

-spec create_binary_resp_content(cowboy_req:req(), cb_context:context()) ->
          {iodata(), cowboy_req:req()}.
create_binary_resp_content(Req, Context) ->
    case cb_context:response(Context) of
        {'ok', RespData} -> {RespData, Req};
        _Else -> {<<>>, Req}
    end.

-spec create_xml_resp_content(cowboy_req:req(), cb_context:context()) ->
          {kz_term:ne_binary() | iolist(), cowboy_req:req()}.
create_xml_resp_content(Req0, Context) ->
    Req1 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/xml">>, Req0),

    case cb_context:response(Context) of
        {'ok', RespData} ->
            {RespData, Req1};
        {'error', {_ErrorCode, ErrorMsg, _RespData}} ->
            MsgEl = #xmlElement{name='message'
                               ,content=[#xmlText{value=ErrorMsg}]
                               },
            ErrorEl = #xmlElement{name='error'
                                 ,content=[MsgEl]
                                 },
            Xml = iolist_to_binary(xmerl:export([ErrorEl], 'xmerl_xml')),
            {Xml, Req1}
    end.

-spec get_encode_options(cb_context:context()) -> kz_json:encode_options().
get_encode_options(Context) ->
    case cb_context:pretty_print(Context) of
        'true' ->  ['pretty'];
        'false' -> []
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type csv_resp_data() :: kz_term:api_binary() |
                         kz_term:binaries() |
                         kz_json:object() |
                         kz_json:objects().

-spec create_csv_resp_content(cowboy_req:req(), cb_context:context()) ->
          {{'file', file:filename_all()} | 'stop', cowboy_req:req(), cb_context:context()}.
create_csv_resp_content(Req, Context) ->
    check_csv_resp_content(Req, Context, cb_context:resp_data(Context)).

-spec check_csv_resp_content(cowboy_req:req(), cb_context:context(), csv_resp_data()) ->
          {{'file', file:filename_all()} | 'stop', cowboy_req:req(), cb_context:context()}.
check_csv_resp_content(Req, Context, 'undefined') ->
    maybe_create_empty_csv_resp(Req, Context);
check_csv_resp_content(Req, Context, []) ->
    maybe_create_empty_csv_resp(Req, Context);
check_csv_resp_content(Req, Context, <<>>) ->
    maybe_create_empty_csv_resp(Req, Context);
check_csv_resp_content(Req, Context, Content) when is_list(Content) ->
    case final_csv_resp_type(Context, should_convert_csv(Content)) of
        'binary' ->
            lager:debug("returning csv content"),
            {Content, Req, Context};
        'is_chunked' ->
            IsStarted = cb_context:fetch(Context, 'chunking_started', 'false'),
            _ = create_csv_chunk_response(Req, Context, Content, IsStarted),

            lager:debug("(chunked) finishing csv stream"),

            'ok' = cowboy_req:stream_body(<<>>, 'fin', Req),
            {'stop', Req, Context};
        'to_csv' ->
            create_csv_resp_content_from_jobjs(Req, Context, Content)
    end;
check_csv_resp_content(Req, Context, Content) ->
    check_csv_resp_content(Req, Context, [Content]).

-spec final_csv_resp_type(cb_context:context(), boolean()) -> 'binary' | 'is_chunked' | 'to_csv'.
final_csv_resp_type(_, 'true') ->
    'to_csv';
final_csv_resp_type(Context, 'false') ->
    case cb_context:fetch(Context, 'is_chunked', 'false')
        andalso not cb_context:fetch(Context, 'chunk_is_file', 'false')
    of
        'true' -> 'is_chunked';
        'false' -> 'binary'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_csv_resp_content_from_jobjs(cowboy_req:req(), cb_context:context(), kz_json:objects()) ->
          {{'file', file:filename_all()} | 'stop', cowboy_req:req(), cb_context:context()}.
create_csv_resp_content_from_jobjs(Req, Context, JObjs) ->
    Context1 = csv_body(Context, JObjs),
    create_csv_resp_content_from_csv_acc(Req, Context1, cb_context:fetch(Context1, 'csv_acc')).

create_csv_resp_content_from_csv_acc(Req, Context, 'undefined') ->
    create_empty_csv_resp(Req, Context);
create_csv_resp_content_from_csv_acc(Req, Context, {_File, _}=CSVAcc) ->
    lager:debug("finishing CSV file ~s", [_File]),
    {File, _} = kz_csv:write_header_to_file(CSVAcc, ?CSV_HEADER_MAP),
    lager:debug("wrote header to '~s'", [File]),

    ContextHeaders = cb_context:resp_headers(Context),
    FileName = csv_file_name(Context, File),
    lager:debug("csv filename ~s", [FileName]),
    ContentDisposition = <<"attachment; filename=\"", (filename:basename(FileName))/binary, "\"">>,
    Headers = #{<<"content-type">> => maps:get(<<"content-type">>, ContextHeaders, <<"text/csv">>)
               ,<<"content-disposition">> => maps:get(<<"content-disposition">>, ContextHeaders, ContentDisposition)
               },
    {{'file', File}
    ,maps:fold(fun(H, V, R) -> cowboy_req:set_resp_header(H, V, R) end, Req, Headers)
    ,cb_context:set_resp_file(Context, File)
    }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
maybe_create_empty_csv_resp(Req, Context) ->
    case cb_context:fetch(Context, 'csv_acc') of
        'undefined' ->
            RespType = final_csv_resp_type(Context, 'false'),
            maybe_create_empty_csv_resp(Req, Context, RespType);
        Acc ->
            create_csv_resp_content_from_csv_acc(Req, Context, Acc)
    end.

-spec maybe_create_empty_csv_resp(cowboy_req:req(), cb_context:context(), 'binary' | 'is_chunked' | 'to_csv') ->
          {'stop', cowboy_req:req(), cb_context:context()}.
maybe_create_empty_csv_resp(Req, Context, 'is_chunked') ->
    case cb_context:fetch(Context, 'chunking_started', 'false') of
        'true' ->
            lager:debug("(chunked) finishing csv stream"),
            'ok' = cowboy_req:stream_body(<<>>, 'fin', Req),
            {'stop', Req, Context};
        'false' ->
            create_empty_csv_resp(Req, Context)
    end;
maybe_create_empty_csv_resp(Req, Context, _) ->
    create_empty_csv_resp(Req, Context).

create_empty_csv_resp(Req, Context) ->
    ContextHeaders = cb_context:resp_headers(Context),
    ContentType = maps:get(<<"content-type">>, ContextHeaders, <<"text/csv">>),
    lager:info("sending empty CSV for ~s", [ContentType]),
    {'stop', Req, Context}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_resp_file(cowboy_req:req(), cb_context:context()) ->
          {resp_file(), cowboy_req:req()}.
create_resp_file(Req, Context) ->
    File = cb_context:resp_file(Context),
    Len = filelib:file_size(File),
    {{'sendfile', 0, Len, File}, Req}.

%%------------------------------------------------------------------------------
%% @doc Encodes the `JObj' and send it as a chunk. Starts chunk response if is
%% not started yet. This is usually called by `api_resource:to_chunk/3'.
%% @end
%%------------------------------------------------------------------------------
-spec create_json_chunk_response(cowboy_req:req(), cb_context:context()) ->
          {boolean(), cowboy_req:req(), cb_context:context()}.
create_json_chunk_response(Req, Context) ->
    JObjs = cb_context:resp_data(Context),
    create_json_chunk_response(Req, Context, JObjs, cb_context:fetch(Context, 'chunking_started', 'false')).

-spec create_json_chunk_response(cowboy_req:req(), cb_context:context(), kz_term:api_objects(), boolean()) ->
          {boolean(), cowboy_req:req(), cb_context:context()}.
create_json_chunk_response(Req, Context, 'undefined', StartedChunk) ->
    {StartedChunk, Req, Context};
create_json_chunk_response(Req, Context, [], StartedChunk) ->
    {StartedChunk, Req, Context};
create_json_chunk_response(Req, Context, JObjs, StartedChunk) ->
    try do_encode_to_json(JObjs) of
        JSON when StartedChunk ->
            'ok' = cowboy_req:stream_body(<<",", JSON/binary>>, 'nofin', Req),
            {StartedChunk, Req, Context};
        JSON ->
            Req1 = init_chunk_stream(Req, Context, <<"to_json">>),
            'ok' = cowboy_req:stream_body(JSON, 'nofin', Req1),
            {'true', Req1, Context}
    catch
        'throw':{'json_encode', {'bad_term', _Term}} ->
            lager:info("json encoding failed on ~p", [_Term]),
            {StartedChunk, Req, Context};
        _E:_R ->
            lager:info("failed to encode response: ~s: ~p", [_E, _R]),
            {StartedChunk, Req, Context}
    end.

-spec do_encode_to_json(kz_json:objects()) -> binary().
do_encode_to_json(JObjs) ->
    Encoded = kz_json:encode(JObjs),
    %% remove first "[" and last "]" from json
    binary:part(Encoded, 1, size(Encoded) - 2).

%%------------------------------------------------------------------------------
%% @doc Convert JSON and save to CSV file if resut is JSON boject, otherwise
%% send already converted CSV binary chunk.
%% @end
%%------------------------------------------------------------------------------
-spec create_csv_chunk_response(cowboy_req:req(), cb_context:context()) ->
          {boolean(), cowboy_req:req(), cb_context:context()}.
create_csv_chunk_response(Req, Context) ->
    create_csv_chunk_response(Req, Context
                             ,cb_context:resp_data(Context)
                             ,cb_context:fetch(Context, 'chunking_started', 'false')
                             ).

-spec create_csv_chunk_response(cowboy_req:req(), cb_context:context(), resp_data(), boolean()) ->
          {boolean(), cowboy_req:req(), cb_context:context()}.
create_csv_chunk_response(Req, Context, 'undefined', IsStarted) ->
    {IsStarted, Req, Context};
create_csv_chunk_response(Req, Context, <<>>, IsStarted) ->
    {IsStarted, Req, Context};
create_csv_chunk_response(Req, Context, [], IsStarted) ->
    {IsStarted, Req, Context};

create_csv_chunk_response(Req, Context, Content, 'true') when is_list(Content) ->
    stream_or_create_csv_chunk(Req, Context, Content, should_convert_csv(Content));
create_csv_chunk_response(Req, Context, Content, 'false') when is_list(Content) ->
    ShouldConvert = should_convert_csv(Content),
    Req1 = maybe_init_csv_chunk_stream(Req, Context, ShouldConvert),
    stream_or_create_csv_chunk(Req1, Context, Content, ShouldConvert);

create_csv_chunk_response(Req, Context, Thing, IsStarted) ->
    create_csv_chunk_response(Req, Context, [Thing], IsStarted).

-spec stream_or_create_csv_chunk(cowboy_req:req(), cb_context:context(), kz_json:objects() | kz_term:ne_binaries(), boolean()) ->
          {boolean(), cowboy_req:req(), cb_context:context()}.
stream_or_create_csv_chunk(Req, Context, JObjs, 'true') ->
    lager:debug("(chunked) continuing conversion of CSV chunks"),
    {'true', Req, csv_body(Context, JObjs)};
stream_or_create_csv_chunk(Req, Context, CSVs, 'false') ->
    lager:debug("(chunked) continuing stream of CSV chunks"),
    'ok' = cowboy_req:stream_body(CSVs, 'nofin', Req),
    {'true', Req, Context}.

-spec should_convert_csv(kz_json:objects() | kz_term:ne_binaries()) -> boolean().
should_convert_csv([First|_]) ->
    kz_json:is_json_object(First).

-spec maybe_init_csv_chunk_stream(cowboy_req:req(), cb_context:context(), boolean()) -> cowboy_req:req().
maybe_init_csv_chunk_stream(Req, Context, 'true') ->
    FileName = csv_file_name(Context, ?DEFAULT_CSV_FILE_NAME),
    lager:debug("(chunked) starting converting JSON to CSV to file ~s", [FileName]),
    Req;
maybe_init_csv_chunk_stream(Req, Context, 'false') ->
    lager:debug("(chunked) starting chunk stream"),
    init_chunk_stream(Req, Context, <<"to_csv">>).

%%------------------------------------------------------------------------------
%% @doc Returns the `x-file-name' from the request header if available.
%% If its not then check URL params for `file_name'. If neither are present
%% then return the default defined value.
%% @end
%%------------------------------------------------------------------------------
-spec csv_file_name(cb_context:context(), kz_term:api_ne_binary()) -> kz_term:api_ne_binary().
csv_file_name(Context, Default) ->
    case cb_context:req_header(Context, <<"x-file-name">>) of
        'undefined' -> cb_context:req_value(Context, <<"file_name">>, Default);
        FileName -> FileName
    end.

-spec init_chunk_stream(cowboy_req:req(), cb_context:context(), kz_term:ne_binary()) -> cowboy_req:req().
init_chunk_stream(Req, _, <<"to_json">>) ->
    Headers = cowboy_req:resp_headers(Req),
    lager:debug("starting to_json stream"),
    Req1 = cowboy_req:stream_reply(200, Headers, Req),
    'ok' = cowboy_req:stream_body("{\"data\":[", 'nofin', Req1),
    Req1;
init_chunk_stream(Req, Context, <<"to_csv">>) ->
    ContextHeaders = cb_context:resp_headers(Context),
    FileName = filename:basename(csv_file_name(Context, ?DEFAULT_CSV_FILE_NAME)),
    DefaultDisposition = <<"attachment; filename=\"", FileName/binary, "\"">>,
    Headers = #{<<"content-type">> => maps:get(<<"content-type">>, ContextHeaders, <<"text/csv">>)
               ,<<"content-disposition">> => maps:get(<<"content-disposition">>, ContextHeaders, DefaultDisposition)
               },
    cowboy_req:stream_reply(200, maps:merge(cowboy_req:resp_headers(Req), Headers), Req).

-spec csv_body(cb_context:context(), kz_json:objects() | kz_term:ne_binaries()) ->
          cb_context:context().
csv_body(Context, JObjs) when is_list(JObjs) ->
    Acc1 = case cb_context:fetch(Context, 'csv_acc') of
               'undefined' -> kz_csv:jobjs_to_file(JObjs);
               Acc0 -> kz_csv:jobjs_to_file(JObjs, Acc0)
           end,
    lager:debug("csv'd to ~p", [Acc1]),
    Setters = [{fun cb_context:store/3, 'csv_acc', Acc1}
              ,{fun cb_context:store/3, 'chunk_is_file', 'true'}
              ],
    cb_context:setters(Context, Setters).

%%------------------------------------------------------------------------------
%% @doc This function will create response expected for a request that
%% is pushing data (like PUT).
%% @end
%%------------------------------------------------------------------------------
-spec create_push_response(cowboy_req:req(), cb_context:context()) ->
          {boolean(), cowboy_req:req(), cb_context:context()}.
create_push_response(Req0, Context) ->
    create_push_response(Req0, Context, fun create_resp_content/2).

-spec create_push_response(cowboy_req:req(), cb_context:context(), resp_content_fun()) ->
          {boolean(), cowboy_req:req(), cb_context:context()}.
create_push_response(Req0, Context, Fun) ->
    {Content, Req1} = Fun(Req0, Context),
    Req2 = set_resp_headers(Req1, Context),
    {succeeded(Context), cowboy_req:set_resp_body(Content, Req2), Context}.

-type pull_response() :: kz_term:text() | resp_file().

%%------------------------------------------------------------------------------
%% @doc This function will create response expected for a request that
%% is pulling data (like GET).
%% @end
%%------------------------------------------------------------------------------
-spec create_pull_response(cowboy_req:req(), cb_context:context()) ->
          {pull_response(), cowboy_req:req(), cb_context:context()} |
          stop_return().
create_pull_response(Req0, Context) ->
    create_pull_response(Req0, Context, fun create_resp_content/2).

-spec create_pull_response(cowboy_req:req(), cb_context:context(), resp_content_fun()) ->
          {pull_response(), cowboy_req:req(), cb_context:context()} |
          stop_return().
create_pull_response(Req0, Context0, Fun) ->
    {Content, Req1, Context1} =
        case Fun(Req0, Context0) of
            {Data, Req01} -> {Data, Req01, Context0};
            {Data, Req01, Context01} -> {Data, Req01, Context01}
        end,
    Req2 = set_resp_headers(Req1, Context1),
    case succeeded(Context1) of
        'false' ->
            lager:info("failed to process pull response in ~p", [Fun]),
            ?MODULE:stop(Req2, Context1);
        'true' ->
            maybe_set_pull_response_stream(Content, Req2, Context1)
    end.

-spec maybe_set_pull_response_stream(kz_term:text() | resp_file(), cowboy_req:req(), cb_context:context()) ->
          {kz_term:text()
          ,cowboy_req:req()
          ,cb_context:context()
          }.
maybe_set_pull_response_stream({'file', File}, Req, Context) ->
    {'ok', #file_info{size=Size}} = file:read_file_info(File),
    lager:debug("sending file ~s(~p)", [File, Size]),
    Req1 = cowboy_req:reply(200, #{}, {'sendfile', 0, Size, File}, Req),
    {'stop', Req1, cb_context:set_resp_status(Context, 'stop')};
maybe_set_pull_response_stream(Other, Req, Context) ->
    {Other, Req, Context}.

-spec get_token_obj(cb_context:context()) -> kz_json:object().
get_token_obj(Context) ->
    kz_json:from_list([{<<"consumed">>, cb_modules_util:token_cost(Context)}
                      ,{<<"remaining">>, cb_modules_util:tokens_remaining(Context)}
                      ]).
%%------------------------------------------------------------------------------
%% @doc This function extracts the response fields and puts them in a proplist.
%% @end
%%------------------------------------------------------------------------------
-spec create_resp_envelope(cb_context:context()) -> kz_json:object().
create_resp_envelope(Context) ->
    do_create_resp_envelope(cb_context:import_errors(Context)).

-spec do_create_resp_envelope(cb_context:context()) -> kz_json:object().
do_create_resp_envelope(Context) ->
    [{Mod, Params} | _] = cb_context:req_nouns(Context),
    Verb = kz_term:to_lower_binary(cb_context:req_verb(Context)),
    Event = create_event_name(Context, list_to_binary(["response_envelope.", Verb, ".", Mod])),
    Payload = [Context | Params],
    try crossbar_bindings:pmap(Event, Payload) of
        []  -> create_default_resp_envelope(Context);
        [JObj | _] -> JObj
    catch
        _:_:_ -> create_default_resp_envelope(Context)
    end.

-spec create_default_resp_envelope(cb_context:context()) -> kz_json:object().
create_default_resp_envelope(Context) ->
    Resp = case cb_context:response(Context) of
               {'ok', RespData} ->
                   [{<<"auth_token">>, cb_context:auth_token(Context)}
                   ,{<<"tokens">>, get_token_obj(Context)}
                   ,{<<"status">>, <<"success">>}
                   ,{<<"request_id">>, cb_context:req_id(Context)}
                   ,{<<"node">>, kz_nodes:node_encoded()}
                   ,{<<"version">>, kz_util:kazoo_version()}
                   ,{<<"timestamp">>, kz_time:iso8601(kz_time:now_s())}
                   ,{<<"revision">>, kz_term:to_api_binary(cb_context:resp_etag(Context))}
                   ,{<<"data">>, RespData}
                   ];
               {'error', {ErrorCode, ErrorMsg, RespData}} ->
                   lager:debug("generating error ~b ~s response", [ErrorCode, ErrorMsg]),
                   [{<<"auth_token">>, kz_term:to_binary(cb_context:auth_token(Context))}
                   ,{<<"tokens">>, get_token_obj(Context)}
                   ,{<<"request_id">>, cb_context:req_id(Context)}
                   ,{<<"node">>, kz_nodes:node_encoded()}
                   ,{<<"version">>, kz_util:kazoo_version()}
                   ,{<<"timestamp">>, kz_time:iso8601(kz_time:now_s())}
                   ,{<<"status">>, <<"error">>}
                   ,{<<"message">>, ErrorMsg}
                   ,{<<"error">>, kz_term:to_binary(ErrorCode)}
                   ,{<<"data">>, RespData}
                   ]
           end,

    encode_start_keys(kz_json:set_values(Resp, cb_context:resp_envelope(Context))
                     ,cb_context:should_paginate(Context)
                     ).

-spec close_chunk_json_envelope(cowboy_req:req(), cb_context:context()) -> 'ok'.
close_chunk_json_envelope(Req, Context) ->
    Paging = kz_json:to_proplist(encode_start_keys(cb_context:resp_envelope(Context)
                                                  ,cb_context:should_paginate(Context)
                                                  )
                                ),
    Trailer =
        Paging ++
        [{<<"status">>, <<"success">>}
        ,{<<"request_id">>, cb_context:req_id(Context)}
        ,{<<"node">>, kz_nodes:node_encoded()}
        ,{<<"version">>, kz_util:kazoo_version()}
        ,{<<"timestamp">>, kz_time:iso8601(kz_time:now_s())}
        ,{<<"revision">>, kz_term:to_api_binary(cb_context:resp_etag(Context))}
        ,{<<"auth_token">>, cb_context:auth_token(Context)}
        ],
    Encoded = [<<"\"", K/binary, "\":\"", (kz_term:to_binary(V))/binary, "\"">>
                   || {K, V} <- Trailer,
                      kz_term:is_not_empty(V)
              ],
    'ok' = cowboy_req:stream_body(<<"], ", (kz_binary:join(Encoded))/binary, "}">>, 'fin', Req).

-spec encode_start_keys(kz_json:object(), boolean()) -> kz_json:object().
encode_start_keys(Resp, 'false') ->
    lager:debug("pagination disabled, removing resp envelope keys"),
    kz_json:delete_keys([<<"start_key">>, <<"page_size">>, <<"next_start_key">>], Resp);
encode_start_keys(Resp, 'true') ->
    lists:foldl(fun(Key, JObj) ->
                        case kz_json:get_value(Key, JObj) of
                            'undefined' -> JObj;
                            Value ->
                                kz_json:set_value(Key, encode_start_key(Value), JObj)
                        end
                end
               ,Resp
               ,[<<"start_key">>, <<"next_start_key">>]
               ).

-spec encode_start_key(kz_json:path()) -> kz_term:ne_binary().
encode_start_key(StartKey) ->
    kz_base64url:encode(erlang:term_to_binary(StartKey)).

-spec decode_start_key(kz_term:ne_binary()) -> kz_json:path().
decode_start_key(Encoded) ->
    try erlang:binary_to_term(kz_base64url:decode(Encoded))
    catch _:_ -> Encoded
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Iterate through {@link cb_context:resp_headers/1}, setting the headers specified.
%% @end
%%------------------------------------------------------------------------------
-spec set_resp_headers(cowboy_req:req(), cb_context:context() | cowboy:http_headers()) ->
          cowboy_req:req().
set_resp_headers(Req0, #{}=Headers) ->
    maps:fold(fun(Header, Value, ReqAcc) ->
                      {H, V} = fix_header(Header, Value, ReqAcc),
                      lager:debug("adding resp header ~s: ~p", [H, V]),
                      cowboy_req:set_resp_header(H, V, ReqAcc)
              end
             ,Req0
             ,Headers
             );
set_resp_headers(Req0, Context) ->
    set_resp_headers(Req0, cb_context:resp_headers(Context)).

-spec fix_header(kz_term:text(), kz_term:text(), cowboy_req:req()) ->
          {binary(), binary()}.
fix_header(<<"Location">>, Path, Req) ->
    {<<"location">>, crossbar_util:get_path(Req, Path)};
fix_header(<<"location">> = H, Path, Req) ->
    {H, crossbar_util:get_path(Req, Path)};
fix_header(H, V, _) ->
    {kz_term:to_lower_binary(H), kz_term:to_binary(V)}.

-spec stop(cowboy_req:req(), cb_context:context()) ->
          stop_return().
stop(Req0, Context) ->
    [{Mod, Params} | _] = cb_context:req_nouns(Context),
    Verb = kz_term:to_lower_binary(cb_context:req_verb(Context)),
    Event = create_event_name(Context, [<<"error">>, Verb, Mod]),
    Payload = [{Req0, Context} | Params],
    try crossbar_bindings:pmap(Event, Payload) of
        []  -> default_stop(Req0, Context);
        [{Req, Ctx} | _] -> {'stop', Req, Ctx}
    catch
        _:_:_ -> default_stop(Req0, Context)
    end.

-spec default_stop(cowboy_req:req(), cb_context:context()) ->
          stop_return().
default_stop(Req0, Context) ->
    StatusCode = cb_context:resp_error_code(Context),
    lager:info("stopping execution here with status code ~p", [StatusCode]),

    {Content, Req1} = create_resp_content(Req0, Context),
    lager:debug("setting resp body: ~s", [Content]),
    Req2 = cowboy_req:set_resp_body(Content, Req1),

    Req3 = add_cors_headers(Req2, Context),
    lager:debug("ensured CORS headers are on the response"),

    Req4 = cowboy_req:set_resp_header(<<"x-request-id">>, cb_context:req_id(Context), Req3),

    lager:debug("replying with ~p", [StatusCode]),
    Req5 = cowboy_req:reply(StatusCode, Req4),
    {'stop', Req5, cb_context:set_resp_status(Context, 'stop')}.

-spec create_event_name(cb_context:context(), kz_term:ne_binary() | kz_term:ne_binaries()) -> kz_term:ne_binary().
create_event_name(Context, Segments) when is_list(Segments) ->
    create_event_name(Context, kz_binary:join(Segments, <<".">>));
create_event_name(Context, Name) ->
    ApiVersion = cb_context:api_version(Context),
    <<ApiVersion/binary, "_resource.", Name/binary>>.


-spec exec_req(cowboy_req:req(), req_funs()) -> cowboy_req:req().
exec_req(Req, []) -> Req;
exec_req(Req, [_|_]=Funs) ->
    lists:foldl(fun exec_req_fold/2, Req, Funs).

-spec exec_req_fold(req_kv(), cowboy_req:req()) -> cowboy_req:req().
exec_req_fold({F, V}, Req) -> F(V, Req);
exec_req_fold({F, K, V}, Req) -> F(K, V, Req);
exec_req_fold(F, Req) when is_function(F, 1) -> F(Req).
