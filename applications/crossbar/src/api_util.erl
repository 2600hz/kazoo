%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz
%%% @doc
%%% Moved util functions out of v1_resource so only REST-related calls
%%% are in there.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Jon Blanton
%%%-------------------------------------------------------------------
-module(api_util).

-export([is_cors_preflight/1
        ,is_cors_request/1
        ,add_cors_headers/2
        ,allow_methods/3
        ,path_tokens/1
        ,parse_path_tokens/2
        ,get_req_data/2
        ,get_http_verb/2
        ,get_auth_token/2
        ,get_pretty_print/2
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
        ,create_resp_content/2, create_resp_file/2, create_csv_resp_content/2
        ,create_pull_response/2, create_pull_response/3

        ,init_chunk_stream/2
        ,close_chunk_json_envelope/2
        ,create_json_chunk_response/2, create_csv_chunk_response/2

        ,stop/2
        ,content_type_matches/2
        ,ensure_content_type/1
        ,create_event_name/2

        ,encode_start_key/1, decode_start_key/1
        ]).

-include("crossbar.hrl").

-define(MAX_UPLOAD_SIZE, kapps_config:get_integer(?CONFIG_CAT, <<"max_upload_size">>, 8000000)).

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

-type stop_return() :: {'stop', cowboy_req:req(), cb_context:context()}.
-type resp_file() :: {integer(), send_file_fun()}.
-type resp_content_return() :: {ne_binary() | iolist() | resp_file(), cowboy_req:req()}.
-type resp_content_fun() :: fun((cowboy_req:req(), cb_context:context()) ->  resp_content_return()).
-type send_file_fun() :: fun((any(), module()) -> ok).
-type pull_file_resp() :: {'stream', integer(), send_file_fun()}.
-type pull_file_response_return() :: {pull_file_resp(), cowboy_req:req(), cb_context:context()} |
                                     stop_return().

-export_type([pull_file_response_return/0
             ,stop_return/0
             ]).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to determine if this is a cross origin resource preflight
%% request
%% @end
%%--------------------------------------------------------------------
-spec is_cors_preflight(cowboy_req:req()) -> boolean().
is_cors_preflight(Req) ->
    is_cors_request(Req)
        andalso ?HTTP_OPTIONS =:= cowboy_req:method(Req).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to determine if this is a cross origin resource sharing
%% request
%% @end
%%--------------------------------------------------------------------
-spec is_cors_request(cowboy_req:req()) -> boolean().
-spec is_cors_request(cowboy_req:req(), ne_binaries()) -> boolean().
is_cors_request(Req) ->
    ReqHdrs = [<<"origin">>, <<"access-control-request-method">>, <<"access-control-request-headers">>],
    is_cors_request(Req, ReqHdrs).

is_cors_request(_Req, []) -> 'false';
is_cors_request(Req, [ReqHdr|ReqHdrs]) ->
    'undefined' =/= cowboy_req:header(ReqHdr, Req)
        orelse is_cors_request(Req, ReqHdrs).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_cors_headers(ne_binaries()) -> kz_proplist().
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
-spec get_req_data(cb_context:context(), cowboy_req:req(), content_type(), kz_json:object()) ->
                          {cb_context:context(), cowboy_req:req()} |
                          stop_return().
get_req_data(Context, Req0) ->
    {QS, Req1} = get_query_string_data(Req0),
    get_req_data(Context, Req1, get_content_type(Req1), QS).

-spec get_query_string_data(cowboy_req:req()) ->
                                   {kz_json:object(), cowboy_req:req()}.
-spec get_query_string_data(kz_proplist(), cowboy_req:req()) ->
                                   {kz_json:object(), cowboy_req:req()}.
get_query_string_data(Req) ->
    QS = cowboy_req:parse_qs(Req),
    get_query_string_data(QS, Req).

get_query_string_data([], Req) ->
    {kz_json:new(), Req};
get_query_string_data(QS0, Req) ->
    QS = kz_json:from_list(QS0),
    lager:debug("query string: ~p", [kz_json:encode(QS)]),
    {QS, Req}.

-spec get_content_type(cowboy_req:req()) -> api_ne_binary().
get_content_type(Req) ->
    case cowboy_req:parse_header(<<"content-type">>, Req) of
        'undefined' -> 'undefined';
        {Main, Sub, _Opts} ->
            <<Main/binary, "/", Sub/binary>>
    end.

get_req_data(Context, Req0, 'undefined', QS) ->
    lager:debug("undefined content type when getting req data, assuming application/json"),
    {Body, Req1} = get_request_body(Req0),
    Ctx = cb_context:set_req_header(Context, <<"content-type">>, ?DEFAULT_CONTENT_TYPE),
    try_json(Body, QS, Ctx, Req1);
get_req_data(Context, Req, <<"multipart/form-data">>, QS) ->
    lager:debug("multipart/form-data content type when getting req data"),
    maybe_extract_multipart(cb_context:set_query_string(Context, QS), Req, QS);

%% cURL defaults to this content-type, so check it for JSON if parsing fails
get_req_data(Context, Req1, <<"application/x-www-form-urlencoded">>, QS) ->
    lager:debug("application/x-www-form-urlencoded content type when getting req data"),
    handle_failed_multipart(cb_context:set_query_string(Context, QS), Req1, QS);

get_req_data(Context, Req0, <<"application/json">>, QS) ->
    lager:debug("application/json content type when getting req data"),
    {Body, Req1} = get_request_body(Req0),
    try_json(Body, QS, Context, Req1);
get_req_data(Context, Req0, <<"application/x-json">>, QS) ->
    lager:debug("application/x-json content type when getting req data"),
    {Body, Req1} = get_request_body(Req0),
    try_json(Body, QS, Context, Req1);
get_req_data(Context, Req1, <<"application/base64">>, QS) ->
    lager:debug("application/base64 content type when getting req data"),
    decode_base64(cb_context:set_query_string(Context, QS), <<"application/base64">>, Req1);
get_req_data(Context, Req1, <<"application/x-base64">>, QS) ->
    lager:debug("application/x-base64 content type when getting req data"),
    decode_base64(cb_context:set_query_string(Context, QS), <<"application/base64">>, Req1);
get_req_data(Context, Req, <<"multipart/", C/binary>>, QS) ->
    lager:debug("multipart ~s content type when getting req data", [C]),
    maybe_extract_multipart(cb_context:set_query_string(Context, QS), Req, QS);
get_req_data(Context, Req1, ContentType, QS) ->
    lager:debug("file's content-type: ~p", [ContentType]),
    extract_file(cb_context:set_query_string(Context, QS), ContentType, Req1).

-spec maybe_extract_multipart(cb_context:context(), cowboy_req:req(), kz_json:object()) ->
                                     {cb_context:context(), cowboy_req:req()} |
                                     stop_return().
maybe_extract_multipart(Context, Req0, QS) ->
    try extract_multipart(Context, Req0, QS)
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug("failed to extract multipart ~s: ~p", [_E, _R]),
            kz_util:log_stacktrace(ST),
            handle_failed_multipart(Context, Req0, QS)
    end.

-spec handle_failed_multipart(cb_context:context(), cowboy_req:req(), kz_json:object()) ->
                                     {cb_context:context(), cowboy_req:req()} |
                                     stop_return().
handle_failed_multipart(Context, Req0, QS) ->
    {ReqBody, Req1} = get_request_body(Req0),

    try get_url_encoded_body(ReqBody) of
        JObj ->
            handle_url_encoded_body(Context, Req1, QS, ReqBody, JObj)
    catch
        _E:_R ->
            lager:debug("failed to extract url-encoded request body: ~s: ~p", [_E, _R]),
            try_json(ReqBody, QS, Context, Req1)
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

-spec set_request_data_in_context(cb_context:context(), cowboy_req:req(), api_object(), kz_json:object()) ->
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

-spec try_json(ne_binary(), kz_json:object(), cb_context:context(), cowboy_req:req()) ->
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

-spec stop_on_invalid_envelope(cowboy_req:req(), cb_context:context()) ->
                                      stop_return().
-spec stop_on_invalid_envelope(ne_binary(), cowboy_req:req(), cb_context:context()) ->
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

stop_on_invalid_envelope(Req, Context) ->
    stop_on_invalid_envelope(?DEFAULT_JSON_ERROR_MSG, Req, Context).

-spec get_url_encoded_body(ne_binary()) -> kz_json:object().
get_url_encoded_body(ReqBody) ->
    kz_json:from_list(cow_qs:parse_qs(ReqBody)).

-type cowboy_multipart_response() :: {'ok', cow_multipart:headers(), cowboy_req:req()} |
                                     {'done', cowboy_req:req()} |
                                     cowboy_req:req().

-spec extract_multipart(cb_context:context(), cowboy_multipart_response(), kz_json:object()) ->
                               {cb_context:context(), cowboy_req:req()}.
extract_multipart(Context, {'done', Req}, _QS) ->
    {Context, Req};
extract_multipart(Context, {'ok', Headers, Req}, QS) ->
    {Ctx, R} = get_req_data(Context, Req, maps:get(<<"content-type">>, Headers, 'undefined'), QS),
    extract_multipart(Ctx
                     ,cowboy_req:read_part(R)
                     ,QS
                     );
extract_multipart(Context, Req, QS) ->
    extract_multipart(Context
                     ,cowboy_req:read_part(Req)
                     ,QS
                     ).

-spec extract_file(cb_context:context(), ne_binary(), cowboy_req:req()) ->
                          {cb_context:context(), cowboy_req:req()} |
                          stop_return().
extract_file(Context, ContentType, Req0) ->
    try extract_file_part_body(Context, ContentType, Req0)
    catch
        _E:_R ->
            extract_file_body(Context, ContentType, Req0)
    end.

-spec extract_file_part_body(cb_context:context(), ne_binary(), cowboy_req:req()) ->
                                    {cb_context:context(), cowboy_req:req()} |
                                    stop_return().
extract_file_part_body(Context, ContentType, Req0) ->
    case cowboy_req:read_part_body(Req0, #{'length'=>?MAX_UPLOAD_SIZE}) of
        {'more', _, Req1} ->
            handle_max_filesize_exceeded(Context, Req1);
        {'ok', FileContents, Req1} ->
            handle_file_contents(Context, ContentType, Req1, FileContents)
    end.

-spec extract_file_body(cb_context:context(), ne_binary(), cowboy_req:req()) ->
                               {cb_context:context(), cowboy_req:req()} |
                               stop_return().
extract_file_body(Context, ContentType, Req0) ->
    case cowboy_req:read_body(Req0, #{'length'=>?MAX_UPLOAD_SIZE}) of
        {'more', _, Req1} ->
            handle_max_filesize_exceeded(Context, Req1);
        {'ok', FileContents, Req1} ->
            handle_file_contents(Context, ContentType, Req1, FileContents)
    end.

-spec handle_max_filesize_exceeded(cb_context:context(), cowboy_req:req()) ->
                                          stop_return().
handle_max_filesize_exceeded(Context, Req1) ->
    Maximum = ?MAX_UPLOAD_SIZE,
    MaxSize = kz_term:to_binary(Maximum),

    lager:error("file size exceeded, max is ~p", [Maximum]),

    Message = <<"Files must not be more than ", MaxSize/binary, " bytes">>,
    JObj = kz_json:from_list([{<<"message">>, Message}, {<<"target">>, Maximum}]),
    Context1 = cb_context:add_validation_error(<<"file">>
                                              ,<<"maxSize">>
                                              ,JObj
                                              ,cb_context:set_resp_error_code(Context, 413)
                                              ),

    ?MODULE:stop(Req1, Context1).

-spec handle_file_contents(cb_context:context(), ne_binary(), cowboy_req:req(), binary()) ->
                                  {cb_context:context(), cowboy_req:req()} |
                                  stop_return().
handle_file_contents(Context, ContentType, Req, FileContents) ->
    %% http://tools.ietf.org/html/rfc2045#page-17
    case cowboy_req:header(<<"content-transfer-encoding">>, Req) of
        <<"base64">> ->
            lager:debug("base64 encoded request coming in"),
            decode_base64(Context, ContentType, Req);
        _Else ->
            lager:debug("unexpected transfer encoding: '~s'", [_Else]),
            ContentLength = cowboy_req:header(<<"content-length">>, Req),
            Headers = kz_json:from_list([{<<"content_type">>, ContentType}
                                        ,{<<"content_length">>, ContentLength}
                                        ]),
            FileJObj = kz_json:from_list([{<<"headers">>, Headers}
                                         ,{<<"contents">>, FileContents}
                                         ]),
            lager:debug("request is a file upload of type: ~s", [ContentType]),

            Filename = uploaded_filename(Context),
            {cb_context:set_req_files(Context, [{Filename, FileJObj}]), Req}
    end.

-spec uploaded_filename(cb_context:context()) -> ne_binary().
uploaded_filename(Context) ->
    case cb_context:req_value(Context, <<"filename">>) of
        'undefined' -> default_filename();
        Filename ->
            lager:debug("found filename on request: ~s", [Filename]),
            Filename
    end.

-spec default_filename() -> ne_binary().
default_filename() ->
    <<"uploaded_file_", (kz_term:to_binary(kz_time:now_s()))/binary>>.

-spec decode_base64(cb_context:context(), ne_binary(), cowboy_req:req()) ->
                           {cb_context:context(), cowboy_req:req()} |
                           stop_return().
decode_base64(Context, CT, Req0) ->
    decode_base64(Context, CT, Req0, []).

decode_base64(Context, CT, Req0, Body) ->
    case cowboy_req:read_body(Req0, #{'length'=>?MAX_UPLOAD_SIZE}) of
        {'error', 'badlength'} ->
            lager:debug("the request body was most likely too big"),
            handle_max_filesize_exceeded(Context, Req0);
        {'error', E} ->
            lager:debug("error getting request body: ~p", [E]),
            ?MODULE:stop(Req0
                        ,cb_context:set_resp_status(cb_context:set_resp_data(Context, E)
                                                   ,'fatal'
                                                   )
                        );
        {'more', _, Req1} ->
            handle_max_filesize_exceeded(Context, Req1);
        {'ok', Base64Data, Req1} ->
            Data = iolist_to_binary(lists:reverse([Base64Data | Body])),

            {EncodedType, FileContents} = kz_attachment:decode_base64(Data),
            ContentType = case EncodedType of
                              'undefined' -> CT;
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
            {cb_context:set_req_files(Context, [{default_filename(), FileJObj}]), Req1}
    end.

-spec get_request_body(cowboy_req:req()) ->
                              {binary(), cowboy_req:req()}.
-spec get_request_body(cowboy_req:req(), iolist()) ->
                              {binary(), cowboy_req:req()}.
get_request_body(Req) ->
    get_request_body(Req, []).
get_request_body(Req0, Body) ->
    try get_request_body(Req0, Body, cowboy_req:read_part_body(Req0))
    catch
        'error':{'badmatch', _} ->
            get_request_body(Req0, Body, cowboy_req:read_body(Req0))
    end.

-type body_return() :: {'more', binary(), cowboy_req:req()} |
                       {'error', atom()} |
                       {'ok', binary(), cowboy_req:req()}.

-spec get_request_body(cowboy_req:req(), iolist(), body_return()) ->
                              {binary(), cowboy_req:req()}.
get_request_body(Req0, _Body, {'error', _E}) ->
    lager:debug("request body had no payload: ~p", [_E]),
    {<<>>, Req0};
get_request_body(_Req0, _Body, {'more', _, Req1}) ->
    lager:error("file size exceeded, max is ~p", [?MAX_UPLOAD_SIZE]),
    {<<>>, Req1};
get_request_body(_Req0, Body, {'ok', Data, Req1}) ->
    {iolist_to_binary([Body, Data]), Req1}.

-type get_json_return() :: {api_object(), cowboy_req:req()} |
                           {{'malformed', ne_binary()}, cowboy_req:req()}.
-spec get_json_body(binary(), cowboy_req:req()) -> get_json_return().

get_json_body(<<>>, Req) -> {'undefined', Req};
get_json_body(ReqBody, Req) -> decode_json_body(ReqBody, Req).

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% normalizes envelope keys
%%   sets envelope keys to lowercase
%% @end
%%--------------------------------------------------------------------
-spec normalize_envelope_keys(kz_json:object()) -> kz_json:object().
normalize_envelope_keys(JObj) ->
    kz_json:foldl(fun normalize_envelope_keys_foldl/3, kz_json:new(), JObj).

-spec normalize_envelope_keys_foldl(kz_json:path(), kz_json:json_term(), kz_json:object()) -> kz_json:object().
normalize_envelope_keys_foldl(_K, 'undefined', JObj) -> JObj;
normalize_envelope_keys_foldl(K, V, JObj) -> kz_json:set_value(kz_json:normalize_key(K), V, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determines if the request envelope is valid
%% @end
%%--------------------------------------------------------------------
-spec is_valid_request_envelope(kz_json:object(), cb_context:context()) -> 'true' | validation_errors().
is_valid_request_envelope(Envelope, Context) ->
    case requires_envelope(Context) of
        'true' -> validate_request_envelope(Envelope);
        'false' -> 'true'
    end.

-spec requires_envelope(cb_context:context()) -> boolean().
requires_envelope(Context) ->
    not lists:member(cb_context:api_version(Context), ?NO_ENVELOPE_VERSIONS).

-spec validate_request_envelope(kz_json:object()) -> 'true' | validation_errors().
validate_request_envelope(Envelope) ->
    case kz_json_schema:validate(?ENVELOPE_SCHEMA, Envelope) of
        {'ok', _} -> 'true';
        {'error', Errors} -> Errors
    end.

-spec get_http_verb(http_method(), cb_context:context()) -> ne_binary().
get_http_verb(Method, Context) ->
    case cb_context:req_value(Context, <<"verb">>) of
        'undefined' -> Method;
        Verb ->
            lager:debug("found verb ~s on request, using instead of ~s", [Verb, Method]),
            kz_term:to_upper_binary(Verb)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will loop over the Tokens in the request path and return
%% a proplist with keys being the module and values a list of parameters
%% supplied to that module.  If the token order is improper a empty list
%% is returned.
%% @end
%%--------------------------------------------------------------------

-spec path_tokens(cb_context:context()) -> ne_binaries().
path_tokens(Context) ->
    Api = cb_context:api_version(Context),
    case cb_context:path_tokens(Context) of
        [<<>>, Api | Tokens] -> Tokens;
        [Api | Tokens] -> Tokens
    end.

-type cb_mod_with_tokens() :: {ne_binary(), path_tokens()}.
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

-spec is_cb_module(cb_context:context(), ne_binary()) -> boolean().
is_cb_module(Context, Elem) ->
    try (kz_term:to_atom(<<"cb_", Elem/binary>>)):module_info('exports') of
        _ -> 'true'
    catch
        'error':'badarg' -> 'false'; %% atom didn't exist already
        _E:_R -> is_cb_module_version(Context, Elem)
    end.

-spec is_cb_module_version(cb_context:context(), ne_binary()) -> boolean().
is_cb_module_version(Context, Elem) ->
    case cb_context:is_context(Context) of
        'false' -> 'false';
        'true'  ->
            ApiVersion = cb_context:api_version(Context),
            ModuleName = <<"cb_", Elem/binary, "_", ApiVersion/binary>>,
            try (kz_term:to_atom(ModuleName)):module_info('exports') of
                _ -> 'true'
            catch
                'error':'badarg' -> 'false'; %% atom didn't exist already
                _E:_R -> 'false'
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will find the intersection of the allowed methods
%% among event respsonses.  The responses can only veto the list of
%% methods, they can not add.
%%
%% If a client passes a ?verb=(PUT|DELETE) on a POST request, ReqVerb will
%% be ?HTTP_PUT or ?HTTP_DELETE, while HttpVerb is 'POST'. If the allowed
%% methods do not include 'POST', we need to add it if allowed methods include
%% the verb in ReqVerb.
%% So, POSTing a ?HTTP_PUT, and the allowed methods include 'PUT', insert POST
%% as well.
%% POSTing a ?HTTP_DELETE, and 'DELETE' is NOT in the allowed methods, remove
%% 'POST' from the allowed methods.
%% @end
%%--------------------------------------------------------------------
-spec allow_methods([http_methods(),...], ne_binary(), http_method()) -> http_methods().
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

-spec uppercase_all(ne_binaries() | atoms()) -> ne_binaries().
uppercase_all(L) when is_list(L) ->
    [kz_term:to_upper_binary(kz_term:to_binary(I)) || I <- L].

%% insert 'POST' if Verb is in Allowed; otherwise remove 'POST'.
-spec maybe_add_post_method(ne_binary(), http_method(), http_methods()) -> http_methods().
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the client has
%% provided a valid authentication token
%% @end
%%--------------------------------------------------------------------
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
    case crossbar_bindings:succeeded(crossbar_bindings:pmap(Event, Context0)) of
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

-spec prefer_new_context(kz_proplist(), cowboy_req:req(), cb_context:context()) ->
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

-spec set_auth_context(cb_context:context(), ne_binary() | {ne_binary(), atom()}) ->
                              cb_context:context().
-spec set_auth_context(cb_context:context(), ne_binary(), atom()) ->
                              cb_context:context().
set_auth_context(Context, {Token, TokenType}) ->
    set_auth_context(Context, Token, TokenType);
set_auth_context(Context, Authorization) ->
    set_auth_context(Context, get_authorization_token_type(Authorization)).

set_auth_context(Context, Token, TokenType) ->
    cb_context:setters(Context, [{fun cb_context:set_auth_token/2, Token}
                                ,{fun cb_context:set_auth_token_type/2, TokenType}
                                ]
                      ).

-spec get_authorization_token_type(ne_binary()) -> {ne_binary(), atom()}.
get_authorization_token_type(<<"Basic ", Token/binary>>) -> {Token, 'basic'};
get_authorization_token_type(<<"Bearer ", Token/binary>>) -> {Token, 'oauth'};
get_authorization_token_type(Token) -> {Token, 'unknown'}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will try to find pretty print option inside the headers
%% if not found will look into the request to find the options
%% otherwise will result in false
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the client is
%% authorized for this request
%% @end
%%--------------------------------------------------------------------
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
                    end, Context0, cb_context:req_nouns(Context0)),

    CT = get_content_type(Req0),

    is_known_content_type(Req0, Context1, ensure_content_type(CT), cb_context:content_types_accepted(Context1)).

-spec is_known_content_type(cowboy_req:req(), cb_context:context(), content_type(), list()) ->
                                   {boolean(), cowboy_req:req(), cb_context:context()}.
is_known_content_type(Req, Context, CT, []) ->
    is_known_content_type(Req, Context, CT, ?CONTENT_ACCEPTED);
is_known_content_type(Req, Context, CT, CTAs) ->
    CTA = lists:foldr(fun({_Fun, L}, Acc) ->
                              lists:foldl(fun fold_in_content_type/2, Acc, L);
                         (L, Acc) ->
                              lists:foldl(fun fold_in_content_type/2, Acc, L)
                      end, [], CTAs),

    IsAcceptable = is_acceptable_content_type(CT, CTA),
    lager:debug("is ~p acceptable content type: ~s", [CT, IsAcceptable]),
    {IsAcceptable, Req, cb_context:set_content_types_accepted(Context, CTAs)}.

-spec fold_in_content_type({ne_binary(), ne_binary()}, list()) -> list().
fold_in_content_type({Type, Sub}, Acc) ->
    [{Type, Sub, []} | Acc].

-spec is_acceptable_content_type(content_type(), [content_type()]) -> boolean().
is_acceptable_content_type(CTA, CTAs) ->
    ['true' || ModCTA <- CTAs, content_type_matches(CTA, ModCTA)] =/= [].

%% (ReqContentType, ModuleContentType)
-spec content_type_matches(content_type(), content_type()) -> boolean().
content_type_matches({Type, _, _}, {Type, <<"*">>, '*'}) ->
    'true';
content_type_matches({Type, SubType, _}, {Type, SubType, '*'}) ->
    'true';
content_type_matches({Type, SubType, Opts}, {Type, SubType, ModOpts}) ->
    lists:all(fun({K, V}) -> props:get_value(K, Opts) =:= V end
             ,ModOpts
             );
content_type_matches(CTA, {CT, SubCT, _}) when is_binary(CTA) ->
    CTA =:= <<CT/binary, "/", SubCT/binary>>;
content_type_matches(CTA, CT) when is_binary(CTA), is_binary(CT) ->
    CTA =:= CT;
content_type_matches(_CTA, _CTAs) ->
    'false'.

-spec ensure_content_type(content_type() | 'undefined') -> content_type().
ensure_content_type('undefined') -> ?CROSSBAR_DEFAULT_CONTENT_TYPE;
ensure_content_type(CT) -> CT.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the target noun
%% (the final module in the chain) accepts this verb parameter pair.
%% @end
%%--------------------------------------------------------------------
-spec does_resource_exist(cb_context:context()) -> boolean().
-spec does_resource_exist(cb_context:context(), list()) -> boolean().
does_resource_exist(Context) ->
    does_resource_exist(Context, cb_context:req_nouns(Context)).

does_resource_exist(Context, [{Mod, Params}|_]) ->
    Event = create_event_name(Context, <<"resource_exists.", Mod/binary>>),
    Responses = crossbar_bindings:map(Event, Params),
    crossbar_bindings:any(Responses) and 'true';
does_resource_exist(_Context, _ReqNouns) ->
    'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function gives each {Mod, Params} pair a chance to determine if
%% it is valid and returns the status, and any errors
%%
%% validate_resource for each {Mod, Params} pair
%% validate for LAST {Mod, Params} pair
%%
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), list()) -> cb_context:context().
validate(Context) ->
    validate(Context, cb_context:req_nouns(Context)).

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the client is
%% authorized for this request
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the response is of type success
%% @end
%%--------------------------------------------------------------------
-spec succeeded(cb_context:context()) -> boolean().
succeeded(Context) -> cb_context:resp_status(Context) =:= 'success'.

-spec execute_request(cowboy_req:req(), cb_context:context()) ->
                             {boolean() | 'stop', cowboy_req:req(), cb_context:context()}.
-spec execute_request(cowboy_req:req(), cb_context:context(), ne_binary(), ne_binaries(), http_method()) ->
                             {boolean() | 'stop', cowboy_req:req(), cb_context:context()}.
execute_request(Req, Context) ->
    case cb_context:req_nouns(Context) of
        [{Mod, Params}|_] ->
            execute_request(Req, Context, Mod, Params, cb_context:req_verb(Context));
        _ReqNouns ->
            {'false', Req, Context}
    end.

execute_request(Req, Context, Mod, Params, Verb) ->
    Event = create_event_name(Context, [<<"execute">>
                                       ,kz_term:to_lower_binary(Verb)
                                       ,Mod
                                       ]),
    Payload = [Context | Params],
    Context1 = crossbar_bindings:fold(Event, Payload),

    case cb_context:is_context(Context1) of
        'true' ->
            execute_request_results(Req, Context1, cb_context:resp_status(Context1));
        'false' ->
            execute_request_failure(Req, Context, Context1)
    end.

-spec execute_request_failure(cowboy_req:req(), cb_context:context(), any()) ->
                                     {'false', cowboy_req:req(), cb_context:context()}.
execute_request_failure(Req, Context, {'error', _E}) ->
    lager:debug("error executing request: ~p", [_E]),
    {'false', Req, Context};
execute_request_failure(Req, Context, _E) ->
    lager:debug("unexpected return from the fold: ~p", [_E]),
    {'false', Req, Context}.

-spec execute_request_results(cowboy_req:req(), cb_context:context()) ->
                                     {'true' | 'stop', cowboy_req:req(), cb_context:context()}.
-spec execute_request_results(cowboy_req:req(), cb_context:context(), crossbar_status()) ->
                                     {'true' | 'stop', cowboy_req:req(), cb_context:context()}.
execute_request_results(Req, Context) ->
    case succeeded(Context) of
        'false' -> ?MODULE:stop(Req, Context);
        'true' -> {'true', Req, Context}
    end.

execute_request_results(Req, Context, 'success') ->
    execute_request_results(Req, Context);
execute_request_results(Req, Context, _RespStatus) ->
    ?MODULE:stop(Req, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function runs the request terminated bindings at the conclusion
%% of all requests
%% @end
%%--------------------------------------------------------------------
-spec finish_request(cowboy_req:req(), cb_context:context()) -> 'ok'.
finish_request(_Req, Context) ->
    [{Mod, _}|_] = cb_context:req_nouns(Context),
    Verb = cb_context:req_verb(Context),
    Event = create_event_name(Context, [<<"finish_request">>, Verb, Mod]),
    _ = kz_util:spawn(fun crossbar_bindings:pmap/2, [Event, Context]),
    case kz_json:get_value(<<"billing">>, cb_context:doc(Context)) of
        'undefined' -> 'ok';
        _Else -> crossbar_services:reconcile(Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create the content for the response body
%% @end
%%--------------------------------------------------------------------
-spec create_resp_content(cowboy_req:req(), cb_context:context()) ->
                                 {ne_binary() | iolist(), cowboy_req:req()}.
create_resp_content(Req0, Context) ->
    Resp = create_resp_envelope(Context),
    Options = get_encode_options(Context),
    try kz_json:encode(Resp, Options) of
        JSON ->
            case cb_context:req_value(Context, <<"jsonp">>) of
                'undefined' ->
                    {JSON
                    ,cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req0)
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

-spec get_encode_options(cb_context:context()) -> kz_json:encode_options().
get_encode_options(Context) ->
    case cb_context:pretty_print(Context) of
        'true' ->  ['pretty'];
        'false' -> []
    end.

-spec create_csv_resp_content(cowboy_req:req(), cb_context:context()) ->
                                     {ne_binary() | iolist(), cowboy_req:req()}.
create_csv_resp_content(Req, Context) ->
    Content = csv_body(cb_context:resp_data(Context)),
    ContextHeaders = cb_context:resp_headers(Context),
    Headers = #{<<"content-type">> => maps:get(<<"content-type">>, ContextHeaders, <<"text/csv">>)
               ,<<"content-disposition">> => maps:get(<<"content-disposition">>, ContextHeaders, <<"attachment; filename=\"data.csv\"">>)
               },
    {Content, maps:fold(fun(H, V, R) -> cowboy_req:set_resp_header(H, V, R) end, Req, Headers)}.

-spec create_resp_file(cowboy_req:req(), cb_context:context()) ->
                              {resp_file(), cowboy_req:req()}.
create_resp_file(Req, Context) ->
    File = cb_context:resp_file(Context),
    Len = filelib:file_size(File),
    Fun = fun(Socket, Transport) ->
                  lager:debug("sending file ~s", [File]),
                  Res = Transport:sendfile(Socket, kz_term:to_list(File)),
                  _ = file:delete(File),
                  Res
          end,
    {{Len, Fun}, Req}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Encode the JObj and send it as a chunk. Start chunk response if is
%% not started yet.
%% @end
%%--------------------------------------------------------------------
-spec create_json_chunk_response(cowboy_req:req(), cb_context:context()) ->
                                        {boolean(), cowboy_req:req()}.
create_json_chunk_response(Req, Context) ->
    JObjs = cb_context:resp_data(Context),
    create_json_chunk_response(Req, JObjs, cb_context:fetch(Context, 'chunking_started', 'false')).

-spec create_json_chunk_response(cowboy_req:req(), kz_json:objects(), boolean()) ->
                                        {boolean(), cowboy_req:req()}.
create_json_chunk_response(Req, [], StartedChunk) ->
    {StartedChunk, Req};
create_json_chunk_response(Req, JObjs, StartedChunk) ->
    try do_encode_to_json(JObjs) of
        JSON when StartedChunk ->
            'ok' = cowboy_req:stream_body(<<",", JSON/binary>>, nofin, Req),
            {StartedChunk, Req};
        JSON ->
            Req1 = init_chunk_stream(Req, <<"to_json">>),
            'ok' = cowboy_req:stream_body(JSON, 'fin', Req1),
            {'true', Req1}
    catch
        'throw':{'json_encode', {'bad_term', _Term}} ->
            lager:debug("json encoding failed on ~p", [_Term]),
            {StartedChunk, Req};
        _E:_R ->
            lager:debug("failed to encode response: ~s: ~p", [_E, _R]),
            {StartedChunk, Req}
    end.

%% @private
-spec do_encode_to_json(kz_json:objects()) -> binary().
do_encode_to_json(JObjs) ->
    Encoded = kz_json:encode(JObjs),
    %% remove first "[" and last "]" from json
    binary:part(Encoded, 1, size(Encoded) - 2).

-spec create_csv_chunk_response(cowboy_req:req(), cb_context:context()) ->
                                       {boolean(), cowboy_req:req()}.
create_csv_chunk_response(Req, Context) ->
    CSVs = cb_context:resp_data(Context),
    case cb_context:fetch(Context, 'chunking_started', 'false') of
        'true' ->
            'ok' = cowboy_req:stream_body(CSVs, 'fin', Req),
            {'true', Req};
        'false' ->
            Req1 = init_chunk_stream(Req, <<"to_csv">>),
            'ok' = cowboy_req:stream_body(CSVs, 'fin', Req1),
            {'true', Req1}
    end.

%% @public
-spec init_chunk_stream(cowboy_req:req(), ne_binary()) -> cowboy_req:req().
init_chunk_stream(Req, <<"to_json">>) ->
    Headers = cowboy_req:resp_headers(Req),
    Req1 = cowboy_req:stream_reply(200, Headers, Req),
    'ok' = cowboy_req:stream_body("{\"data\":[", 'nofin', Req1),
    Req1;
init_chunk_stream(Req, <<"to_csv">>) ->
    Headers0 = #{<<"content-type">> => <<"text/csv">>
                ,<<"content-disposition">> => <<"attachment; filename=\"result.csv\"">>
                },
    Headers = maps:merge(Headers0, cowboy_req:resp_headers(Req)),
    cowboy_req:stream_reply(200, Headers, Req).

-spec csv_body(ne_binary() | kz_json:object() | kz_json:objects()) -> iolist().
csv_body(Body=?NE_BINARY) -> Body;
csv_body(JObjs) when is_list(JObjs) ->
    FlattenJObjs = [kz_json:flatten(JObj, 'binary_join') || JObj <- JObjs],
    CsvOptions = [{'transform_fun', fun map_empty_json_value_to_binary/2}
                 ,{'header_map', ?CSV_HEADER_MAP}
                 ],
    kz_csv:from_jobjs(FlattenJObjs, CsvOptions);
csv_body(JObj) ->
    csv_body([JObj]).

-spec map_empty_json_value_to_binary(kz_json:key(), kz_json:term()) -> {kz_json:key(), kz_json:term()}.
map_empty_json_value_to_binary(Key, Value) ->
    case kz_json:is_json_object(Value) of
        'true' -> {Key, <<>>};
        'false' -> {Key, Value}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create response expected for a request that
%% is pushing data (like PUT)
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create response expected for a request that
%% is pulling data (like GET)
%% @end
%%--------------------------------------------------------------------
-type pull_response() :: text() | resp_file().

-spec create_pull_response(cowboy_req:req(), cb_context:context()) ->
                                  {pull_response(), cowboy_req:req(), cb_context:context()} |
                                  stop_return().
create_pull_response(Req0, Context) ->
    create_pull_response(Req0, Context, fun create_resp_content/2).

-spec create_pull_response(cowboy_req:req(), cb_context:context(), resp_content_fun()) ->
                                  {pull_response(), cowboy_req:req(), cb_context:context()} |
                                  stop_return().
create_pull_response(Req0, Context, Fun) ->
    {Content, Req1} = Fun(Req0, Context),
    Req2 = set_resp_headers(Req1, Context),
    case succeeded(Context) of
        'false' -> ?MODULE:stop(Req2, Context);
        'true' -> {maybe_set_pull_response_stream(Content), Req2, Context}
    end.

-spec maybe_set_pull_response_stream(text() | resp_file()) -> text() | pull_file_resp().
maybe_set_pull_response_stream({FileLength, TransportFun})
  when is_integer(FileLength)
       andalso is_function(TransportFun, 2) ->
    {'stream', FileLength, TransportFun};
maybe_set_pull_response_stream(Other) ->
    Other.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function extracts the response fields and puts them in a proplist
%% @end
%%--------------------------------------------------------------------
-spec create_resp_envelope(cb_context:context()) -> kz_json:object().
-spec do_create_resp_envelope(cb_context:context()) -> kz_json:object().
create_resp_envelope(Context) ->
    do_create_resp_envelope(cb_context:import_errors(Context)).

do_create_resp_envelope(Context) ->
    Resp = case cb_context:response(Context) of
               {'ok', RespData} ->
                   [{<<"auth_token">>, cb_context:auth_token(Context)}
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

    encode_start_keys(kz_json:set_values(props:filter_undefined(Resp), cb_context:resp_envelope(Context))
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
        props:filter_undefined(
          [{<<"status">>, <<"success">>}
          ,{<<"request_id">>, cb_context:req_id(Context)}
          ,{<<"node">>, kz_nodes:node_encoded()}
          ,{<<"version">>, kz_util:kazoo_version()}
          ,{<<"timestamp">>, kz_time:iso8601(kz_time:now_s())}
          ,{<<"revision">>, kz_term:to_api_binary(cb_context:resp_etag(Context))}
          ,{<<"auth_token">>, cb_context:auth_token(Context)}
          ]),
    Encoded = [<<"\"", K/binary, "\":\"", (kz_term:to_binary(V))/binary, "\"">>
                   || {K, V} <- Trailer,
                      kz_term:is_not_empty(V)
              ],
    'ok' = cowboy_req:stream_body(<<"], ", (kz_binary:join(Encoded))/binary, "}">>, fin, Req).

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

-spec encode_start_key(kz_json:path()) -> ne_binary().
encode_start_key(StartKey) ->
    kz_base64url:encode(erlang:term_to_binary(StartKey)).

-spec decode_start_key(ne_binary()) -> kz_json:path().
decode_start_key(Encoded) ->
    try erlang:binary_to_term(kz_base64url:decode(Encoded))
    catch _:_ -> Encoded
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Iterate through cb_context:resp_headers/1, setting the headers specified
%% @end
%%--------------------------------------------------------------------
-spec set_resp_headers(cowboy_req:req(), cb_context:context() | cowboy:http_headers()) ->
                              cowboy_req:req().
set_resp_headers(Req0, #{}=Headers) ->
    maps:fold(fun(Header, Value, ReqAcc) ->
                      {H, V} = fix_header(Header, Value, ReqAcc),
                      cowboy_req:set_resp_header(H, V, ReqAcc)
              end
             ,Req0
             ,Headers
             );
set_resp_headers(Req0, Context) ->
    set_resp_headers(Req0, cb_context:resp_headers(Context)).

-spec fix_header(text(), text(), cowboy_req:req()) ->
                        {binary(), binary()}.
fix_header(<<"Location">>, Path, Req) ->
    {<<"location">>, crossbar_util:get_path(Req, Path)};
fix_header(<<"ocation">> = H, Path, Req) ->
    {H, crossbar_util:get_path(Req, Path)};
fix_header(H, V, _) ->
    {kz_term:to_lower_binary(H), kz_term:to_binary(V)}.

-spec stop(cowboy_req:req(), cb_context:context()) ->
                  stop_return().
stop(Req0, Context) ->
    StatusCode = cb_context:resp_error_code(Context),
    lager:info("stopping execution here with status code ~p", [StatusCode]),

    {Content, Req1} = create_resp_content(Req0, Context),
    lager:debug("setting resp body: ~s", [Content]),
    Req2 = cowboy_req:set_resp_body(Content, Req1),

    Req3 = add_cors_headers(Req2, Context),
    lager:debug("ensured CORS headers are on the response"),

    Req4 = cowboy_req:set_resp_header(<<"x-request-id">>, cb_context:req_id(Context), Req3),

    Req5 = cowboy_req:reply(StatusCode, Req4),
    {'stop', Req5, cb_context:set_resp_status(Context, 'stop')}.

-spec create_event_name(cb_context:context(), ne_binary() | ne_binaries()) -> ne_binary().
create_event_name(Context, Segments) when is_list(Segments) ->
    create_event_name(Context, kz_binary:join(Segments, <<".">>));
create_event_name(Context, Name) ->
    ApiVersion = cb_context:api_version(Context),
    <<ApiVersion/binary, "_resource.", Name/binary>>.
