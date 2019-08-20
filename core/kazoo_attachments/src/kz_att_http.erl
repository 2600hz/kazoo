%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc Simple URL Storage for attachments.
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
    ClientSegment = kz_att_util:format_url(Settings, {DbName, DocId, AName}, fields(Settings)),
    Separator = base_separator(BaseUrl),

    Url = list_to_binary([BaseUrl, Separator, ClientSegment]),

    DefaultContentType = props:get_value('content_type', Options, kz_mime:from_filename(AName)),

    {ContentType, Body} = build_req_body(Settings, DbName, DocId, AName, Contents, DefaultContentType),
    Headers = [{'content_type', ContentType}],

    case send_request(Url, format_verb(Verb), Headers, Body) of
        {'ok', NewUrl, _Body, _Debug} ->
            DocUrlField = maps:get('document_url_field', Settings, 'undefined'),
            {'ok', url_fields(DocUrlField, NewUrl, Settings)};
        {'error', ErrorUrl, Resp} ->
            Routines = [{fun kz_att_error:set_req_url/2, ErrorUrl}
                        | kz_att_error:put_routines(Settings, DbName, DocId, AName, Contents, Options)
                       ],
            handle_http_error_response(Resp, Routines)
    end.

build_req_body(#{'send_multipart' := SendMultipart}=Options, DbName, DocId, AName, Contents, DefaultContentType) ->
    case kz_term:is_true(SendMultipart) of
        'true' -> build_multipart_body(Options, DbName, DocId, AName, Contents, DefaultContentType);
        'false' -> {DefaultContentType, Contents}
    end;
build_req_body(_Settings, _DbName, _DocId, _AName, Contents, DefaultContentType) ->
    {DefaultContentType, Contents}.

build_multipart_body(Options, DbName, DocId, AName, Contents, DefaultContentType) ->
    {'ok', Doc} = kz_datamgr:open_cache_doc(DbName, DocId),
    Metadata = kz_doc:public_fields(Doc),

    Parts = [{kz_json:encode(Metadata)
             ,[{<<"content-disposition">>, <<"form-data; name=", DocId/binary>>}
              ,{<<"content-type">>, <<"application/json">>}
              ]
             }
            ,{maybe_encode_content_part(Options, Contents)
             ,[{<<"content-disposition">>, <<"form-data; name=", AName/binary>>}
              ,{<<"content-type">>, DefaultContentType}
              ]
             }
            ],

    Boundary = kz_http_util:create_boundary(),

    lager:debug("creating multipart body with boundary ~s", [Boundary]),
    {<<"multipart/form-data; boundary=", Boundary/binary>>
    ,kz_http_util:encode_multipart(Parts, Boundary)
    }.

maybe_encode_content_part(#{'base64_encode_data':= ShouldEncode}, Contents) ->
    case kz_term:is_true(ShouldEncode) of
        'true' ->
            lager:debug("base64-encoding contents"),
            base64:encode(Contents);
        'false' ->
            Contents
    end;
maybe_encode_content_part(_Settings, Contents) -> Contents.

-spec format_verb(kz_term:ne_binary()) -> 'put' | 'post'.
format_verb(<<"POST">>) -> 'post';
format_verb(<<"PUT">>) -> 'put';
format_verb(<<"post">>) -> 'post';
format_verb(<<"put">>) -> 'put'.

-spec fields(map()) -> url_fields().
fields(#{field_list := FieldList}) -> FieldList;
fields(_Settings) -> kz_att_util:default_format_url_fields().

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
            handle_fetch_attachment_resp(fetch_attachment(Url), Routines)
    end.

-spec handle_fetch_attachment_resp(gen_attachment:fetch_response(), kz_att_error:update_routines()) ->
                                          gen_attachment:fetch_response().
handle_fetch_attachment_resp({'error', Url, Resp}, Routines) ->
    handle_http_error_response(Resp, [{fun kz_att_error:set_req_url/2, Url} | Routines]);
handle_fetch_attachment_resp({'ok', Body}, _Routines) ->
    {'ok', Body}.

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


url_fields('undefined', Url, Settings) ->
    [{'attachment', [{<<"url">>, Url} | maybe_add_settings(Settings)]}];
url_fields(DocUrlField, Url, Settings) ->
    [{'attachment', [{<<"url">>, Url} | maybe_add_settings(Settings)]}
    ,{'document', [{DocUrlField, Url}]}
    ].

maybe_add_settings(#{'base64_encode_data' := 'true'}) ->
    [{<<"base64_encode_data">>, 'true'}];
maybe_add_settings(_Settings) -> [].

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
    lager:debug("resp headers: ~p", [RespHeaders]),
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
