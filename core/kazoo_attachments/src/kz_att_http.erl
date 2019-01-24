%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc Simple URL Storage for attachments.
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_att_http).
-behaviour(gen_attachment).

-include("kz_att.hrl").

%% `gen_attachment' behaviour callbacks (API)
-export([put_attachment/6]).
-export([fetch_attachment/4]).

-define(MAX_REDIRECTS, 10).

%%%=============================================================================
%%% gen_attachment behaviour callbacks (API)
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec put_attachment(gen_attachment:settings()
                    ,gen_attachment:db_name()
                    ,gen_attachment:doc_id()
                    ,gen_attachment:att_name()
                    ,gen_attachment:contents()
                    ,gen_attachment:options()
                    ) -> gen_attachment:put_response().
put_attachment(Settings, DbName, DocId, AName, Contents, Options) ->
    #{url := BaseUrlParam
     ,verb := Verb
     } = Settings,

    BaseUrl = kz_binary:strip_right(BaseUrlParam, $/),
    Url = list_to_binary([BaseUrl, base_separator(BaseUrl), kz_att_util:format_url(Settings, {DbName, DocId, AName})]),

    DefaultContentType = props:get_value('content_type', Options, kz_mime:from_filename(AName)),

    {ContentType, Body} = build_req_body(Settings, DbName, DocId, Contents, DefaultContentType),
    Headers = [{'content_type', ContentType}],

    case send_request(Url, format_verb(Verb), Headers, Body) of
        {'ok', NewUrl, _Body, _Debug} ->
            DocUrlField = maps:get('document_url_field', Settings, 'undefined'),
            {'ok', url_fields(DocUrlField, NewUrl)};
        {'error', ErrorUrl, Resp} ->
            Routines = [{fun kz_att_error:set_req_url/2, ErrorUrl}
                        | kz_att_error:put_routines(Settings, DbName, DocId, AName, Contents, Options)
                       ],
            handle_http_error_response(Resp, Routines)
    end.

build_req_body(#{'send_multipart' := SendMultipart}=Options, DbName, DocId, Contents, DefaultContentType) ->
    case kz_term:is_true(SendMultipart) of
        'true' -> build_multipart_body(Options, DbName, DocId, Contents, DefaultContentType);
        'false' -> {DefaultContentType, Contents}
    end;
build_req_body(_Settings, _DbName, _DocId, Contents, DefaultContentType) ->
    {DefaultContentType, Contents}.

build_multipart_body(Options, DbName, DocId, Contents, DefaultContentType) ->
    {'ok', Doc} = kz_datamgr:open_cache_doc(DbName, DocId),
    Metadata = kz_doc:public_fields(Doc),

    Parts = [{kz_json:encode(Metadata), [{<<"content-type">>, <<"application/json">>}]}
            ,{maybe_encode_content_part(Options, Contents), [{<<"content-type">>, DefaultContentType}]}
            ],

    Boundary = kz_http_util:create_boundary(),

    {<<"multipart/mixed; boundary=", Boundary/binary>>
    ,kz_http_util:encode_multipart(Parts, Boundary)
    }.

maybe_encode_content_part(#{'base64_encode_data':= ShouldEncode}, Contents) ->
    case kz_term:is_true(ShouldEncode) of
        'true' ->
            lager:debug("base64-encoding contents"),
            base64:encode(Contents);
        'false' ->
            Contents
    end.

-spec format_verb(kz_term:ne_binary()) -> 'put' | 'post'.
format_verb(<<"POST">>) -> 'post';
format_verb(<<"PUT">>) -> 'put';
format_verb(<<"post">>) -> 'post';
format_verb(<<"put">>) -> 'put'.

-spec fetch_attachment(gen_attachment:handler_props()
                      ,gen_attachment:db_name()
                      ,gen_attachment:doc_id()
                      ,gen_attachment:att_name()
                      ) -> gen_attachment:fetch_response().
fetch_attachment(HandlerProps, DbName, DocId, AName) ->
    Routines = kz_att_error:fetch_routines(HandlerProps, DbName, DocId, AName),
    case kz_json:get_value(<<"url">>, HandlerProps) of
        'undefined' -> kz_att_error:new('invalid_data', Routines);
        Url ->
            AttSettings = kz_json:get_value(<<"handler_props">>, HandlerProps),
            handle_fetch_attachment_resp(fetch_attachment(Url), AttSettings, Routines)
    end.

-spec handle_fetch_attachment_resp(gen_attachment:fetch_response(), map(), kz_att_error:update_routines()) ->
                                          gen_attachment:fetch_response().
handle_fetch_attachment_resp({'error', Url, Resp}, _AttSettings, Routines) ->
    handle_http_error_response(Resp, [{fun kz_att_error:set_req_url/2, Url} | Routines]);
handle_fetch_attachment_resp({'ok', <<"\r\n", _/binary>>=Multipart}
                            ,#{'send_multipart' := 'true'}=AttSettings
                            ,Routines
                            ) ->
    lager:debug("recv multipart response, looking for attachment"),
    handle_multipart_fetch(Multipart, AttSettings, Routines);
handle_fetch_attachment_resp({'ok', Body}, #{'base64_encode_data' := 'true'}, _Routines) ->
    {'ok', base64:decode(Body)};
handle_fetch_attachment_resp({'ok', Body}, _AttSettings, _Routines) ->
    {'ok', Body};
handle_fetch_attachment_resp(Else, _AttSettings, _Routines) ->
    Else.

-spec handle_multipart_fetch(kz_term:ne_binary(), map(), kz_att_error:update_routines()) ->
                                    gen_attachment:fetch_response().
handle_multipart_fetch(Multipart, AttSettings, Routines) ->
    [Boundary | Parts] = [Bin || Bin <- binary:split(Multipart, <<"\r\n">>, ['global']),
                                 Bin =/= <<>>
                         ],
    handle_multipart_contents(AttSettings, Boundary, Parts, Routines).

-spec handle_multipart_contents(map(), kz_term:ne_binary(), kz_term:ne_binaries(), kz_att_error:update_routines()) ->
                                       gen_attachment:fetch_response().
handle_multipart_contents(_AttSettings, _Boundary, [], Routines) ->
    lager:error("failed to find content in multipart"),
    handle_http_error_response(<<"invalid multipart response">>, Routines);
handle_multipart_contents(AttSettings
                         ,Boundary
                         ,[<<"content-type: application/json">>, _Metadata
                          ,Boundary, <<"content-type: ", _CT/binary>>, Content
                           | _
                          ]
                         ,Routines
                         ) ->
    lager:debug("found content of type ~s", [_CT]),
    handle_fetch_attachment_resp({'ok', Content}, AttSettings, Routines);
handle_multipart_contents(AttSettings, Boundary, [_Part | Parts], Routines) ->
    lager:debug("skipping part ~s", [_Part]),
    handle_multipart_contents(AttSettings, Boundary, Parts, Routines).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec base_separator(kz_term:ne_binary()) -> binary().
base_separator(Url) ->
    case kz_http_util:urlsplit(Url) of
        {_, _, _, <<>>, _} -> <<"/">>;
        {_, _, _, _, _} -> <<>>
    end.

send_request(Url, Verb, Headers, Contents) ->
    send_request(Url, Verb, Headers, Contents, 0, kz_json:new()).

send_request(Url, _Verb, _Headers, _Contents, Redirects, _)
  when Redirects > ?MAX_REDIRECTS ->
    {'error', Url, 'too_many_redirects'};
send_request(Url, Verb, Headers, Contents, Redirects, Debug) ->
    case kz_http:req(Verb, Url, Headers, Contents) of
        {'ok', Code, ReplyHeaders, Body} when
              is_integer(Code)
              andalso Code >= 200
              andalso Code =< 299 ->
            {'ok', Url, Body, add_debug(Debug, Url, Code, ReplyHeaders)};
        {'ok', Code, ReplyHeaders, _Body} when
              Code =:= 301;
              Code =:= 302 ->
            NewDebug = add_debug(Debug, Url, Code, ReplyHeaders),
            Fun = fun(URL, Tries, Data) -> send_request(URL, Verb, Headers, Contents, Tries, Data) end,
            maybe_redirect(Url, ReplyHeaders, Redirects, NewDebug, Fun);
        Resp -> {'error', Url, Resp}
    end.

maybe_redirect(ToUrl, Headers, Redirects, Debug, Fun) ->
    case props:get_value("location", Headers) of
        'undefined' ->
            lager:debug("request to ~s got redirection but no 'location' header was found", [ToUrl]),
            {'error', ToUrl, 'redirection_with_no_location'};
        Url ->
            maybe_redirect_loop(ToUrl, Url, Redirects, Debug, Fun)
    end.

maybe_redirect_loop(ToUrl, Url, Redirects, Debug, Fun) ->
    case kz_json:get_value(Url, Debug) of
        'undefined' ->
            lager:debug("request to ~s redirected to ~s.",[ToUrl, Url]),
            Fun(Url, Redirects + 1, Debug);
        _ ->
            lager:debug("the request ~s got redirect to ~s but this is already in the list of visited urls",[ToUrl, Url]),
            {'error', ToUrl, 'redirect_loop_detected'}
    end.

add_debug(Debug, Url, Code, Headers) ->
    kz_json:set_values([{[Url, <<"code">>], Code}
                       ,{[Url, <<"headers">>], kz_json:from_list([{kz_term:to_binary(K), kz_term:to_binary(V)}|| {K,V} <- Headers])}
                       ], Debug).


url_fields('undefined', Url) ->
    [{'attachment', [{<<"url">>, Url}]}];
url_fields(DocUrlField, Url) ->
    [{'attachment', [{<<"url">>, Url}]}
    ,{'document', [{DocUrlField, Url}]}
    ].

-spec fetch_attachment(kz_term:ne_binary()) ->
                              gen_attachment:fetch_response() |
                              {'error', kz_term:ne_binary(), atom() | kz_http:ret()}.
fetch_attachment(URL) ->
    fetch_attachment(URL, 0, kz_json:new()).

-spec fetch_attachment(kz_term:ne_binary(), integer(), kz_json:object()) ->
                              gen_attachment:fetch_response() |
                              {'error', kz_term:ne_binary(), atom() | kz_http:ret()}.
fetch_attachment(Url, Redirects, _)
  when Redirects > ?MAX_REDIRECTS ->
    {'error', Url, 'too_many_redirects'};
fetch_attachment(Url, Redirects, Debug) ->
    case kz_http:get(Url) of
        {'ok', 200, _Headers, Body} ->
            {'ok', Body};
        {'ok', Code, Headers, _Body} when
              Code =:= 301;
              Code =:= 302 ->
            NewDebug = add_debug(Debug, Url, Code, Headers),
            maybe_redirect(Url, Headers, Redirects, NewDebug, fun fetch_attachment/3);
        Resp -> {'error', Url, Resp}
    end.

-spec handle_http_error_response(kz_http:req(), kz_att_error:update_routines()) -> kz_att_error:error().
handle_http_error_response({'ok', RespCode, RespHeaders, RespBody} = _E, Routines) ->
    Reason = kz_http_util:http_code_to_status_line(RespCode),
    NewRoutines = [{fun kz_att_error:set_resp_code/2, RespCode}
                  ,{fun kz_att_error:set_resp_headers/2, RespHeaders}
                  ,{fun kz_att_error:set_resp_body/2, RespBody}
                   | Routines
                  ],
    lager:error("http storage error: ~p: ~s", [RespCode, RespBody]),
    kz_att_error:new(Reason, NewRoutines);
handle_http_error_response({'error', {'failed_connect', Reason}} = _E, Routines) ->
    lager:error("http storage failed to connect: ~p", [_E]),
    kz_att_error:new(Reason, Routines);
handle_http_error_response({'error', {Reason, _}} = _E, Routines)
  when is_atom(Reason) ->
    lager:error("http storage request error: ~p", [_E]),
    kz_att_error:new(Reason, Routines);
handle_http_error_response(_E, Routines) ->
    lager:error("http storage request error: ~p", [_E]),
    kz_att_error:new('request_error', Routines).
