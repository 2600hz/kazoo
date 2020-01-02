%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2020, 2600Hz
%%% @doc Google Storage for attachments.
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_att_google_storage).
-behaviour(gen_attachment).

-include("kz_att.hrl").

%% `gen_attachment' behaviour callbacks (API)
-export([put_attachment/6]).
-export([fetch_attachment/4]).

-export([gstorage_default_fields/0]).

-define(DRV_BUCKET_UPLOAD_URL(B), <<"https://www.googleapis.com/upload/storage/v1/b/", B/binary, "/o">>).
-define(DRV_SINGLE_FILE_UPLOAD_URL(B), <<(?DRV_BUCKET_UPLOAD_URL(B))/binary, "?uploadType=media">>).
-define(DRV_MULTIPART_FILE_URL(B), <<(?DRV_BUCKET_UPLOAD_URL(B))/binary, "?uploadType=multipart">>).
-define(DRV_BASE_FETCH_URL(B), <<"https://www.googleapis.com/storage/v1/b/", B/binary, "/o">>).

-define(DRV_SCOPE, <<"https://www.googleapis.com/auth/devstorage.read_write">>).
-define(DRV_SCOPES, [?DRV_SCOPE]).
-define(DRV_TOKEN_OPTIONS, #{'scopes' => ?DRV_SCOPES}).

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
    do_put_attachment(gstorage_token(Settings), Settings, DbName, DocId, AName, Contents, Options).

-spec do_put_attachment(kz_auth_client:token()
                       ,gen_attachment:settings()
                       ,gen_attachment:db_name()
                       ,gen_attachment:doc_id()
                       ,gen_attachment:att_name()
                       ,gen_attachment:contents()
                       ,gen_attachment:options()
                       ) -> gen_attachment:put_response().
do_put_attachment({'ok', #{'token' := #{'authorization' := Authorization}}}
                 ,Settings, DbName, DocId, AName, Contents, Options) ->
    CT = kz_mime:from_filename(AName),
    {Bucket, Name} = resolve_path(Settings, {DbName, DocId, AName}),
    case send_attachment(Authorization, Bucket, Name, CT, Options, Contents) of
        {'ok', ContentUrl, ResponseHeaders} ->
            props:to_log(ResponseHeaders, <<"GSTORAGE2 HEADERS">>),
            Data = base64:encode(term_to_binary({Settings, ContentUrl})),
            {'ok', [{'attachment', [{<<"gstorage">>, Data}
                                   ,{<<"metadata">>, kz_json:from_list(ResponseHeaders)}
                                   ]}
                   ]};
        {'ok', ResponseHeaders} ->
            Data = base64:encode(term_to_binary({Settings, {'name', Name}})),
            {'ok', [{'attachment', [{<<"gstorage">>, Data}
                                   ,{<<"metadata">>, kz_json:from_list(ResponseHeaders)}
                                   ]}
                   ]};
        {'error', Url, Resp} ->
            Routines = [{fun kz_att_error:set_req_url/2, Url}
                        | kz_att_error:put_routines(Settings, DbName, DocId, AName, Contents, Options)
                       ],
            handle_http_error_response(Resp, Routines)
    end;
do_put_attachment({'error', _}, Settings, DbName, DocId, AName, Contents, Options) ->
    Routines = kz_att_error:put_routines(Settings, DbName, DocId, AName, Contents, Options),
    kz_att_error:new('oauth_failure', Routines).

-spec fetch_attachment(gen_attachment:handler_props()
                      ,gen_attachment:db_name()
                      ,gen_attachment:doc_id()
                      ,gen_attachment:att_name()
                      ) -> gen_attachment:fetch_response().
fetch_attachment(HandlerProps, DbName, DocId, AName) ->
    case kz_json:get_value(<<"gstorage">>, HandlerProps) of
        'undefined' ->
            Routines = kz_att_error:fetch_routines(HandlerProps, DbName, DocId, AName),
            kz_att_error:new('invalid_data', Routines);
        GData ->
            {#{'bucket' := Bucket} = Settings, ContentId} = binary_to_term(base64:decode(GData)),
            do_fetch_attachment(gstorage_token(Settings), Bucket, ContentId, HandlerProps, DbName, DocId, AName)
    end.

-spec do_fetch_attachment(kz_auth_client:token()
                         ,kz_term:ne_binary()
                         ,{'name', kz_term:ne_binary()} | kz_term:ne_binary()
                         ,gen_attachment:handler_props()
                         ,gen_attachment:db_name()
                         ,gen_attachment:doc_id()
                         ,gen_attachment:att_name()
                         ) -> gen_attachment:fetch_response().
do_fetch_attachment(Authorization, Bucket, {'name', Name}, HandlerProps, DbName, DocId, AName) ->
    ContentId = <<(?DRV_BASE_FETCH_URL(Bucket))/binary, "/", Name/binary>>,
    do_fetch_attachment(Authorization, Bucket, ContentId, HandlerProps, DbName, DocId, AName);
do_fetch_attachment({'ok', #{'token' := #{'authorization' := Authorization}}}
                   ,_Bucket, ContentId, HandlerProps, DbName, DocId, AName) ->
    Headers = [{<<"Authorization">>, Authorization}],
    case kz_http:get(ContentId, Headers) of
        {'ok', 200, _ResponseHeaders, ResponseBody} ->
            {'ok', ResponseBody};
        Resp ->
            Routines = [{fun kz_att_error:set_req_url/2, ContentId}
                        | kz_att_error:fetch_routines(HandlerProps, DbName, DocId, AName)
                       ],
            handle_http_error_response(Resp, Routines)
    end;
do_fetch_attachment({'error', _}, _, _, HandlerProps, DbName, DocId, AName) ->
    Routines = kz_att_error:fetch_routines(HandlerProps, DbName, DocId, AName),
    kz_att_error:new('oauth_failure', Routines).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec gstorage_default_fields() -> url_fields().
gstorage_default_fields() ->
    [{'group', [{'arg', <<"id">>}
               ,{'const', <<"_">>}
               ,{'arg', <<"attachment">>}
               ]}
    ].

-spec gstorage_format_url(map(), attachment_info()) -> kz_term:ne_binary().
gstorage_format_url(Map, AttInfo) ->
    kz_att_util:format_url(Map, AttInfo, gstorage_default_fields()).

-spec resolve_path(map(), attachment_info()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
resolve_path(#{'bucket' := Bucket} = Settings, AttInfo) ->
    Url = gstorage_format_url(Settings, AttInfo),
    {Bucket, Url}.

-spec gstorage_token(map()) -> kz_auth_client:token().
gstorage_token(#{'oauth_doc_id' := TokenDocId}) ->
    kz_auth_client:token_for_auth_id(TokenDocId, ?DRV_TOKEN_OPTIONS);
gstorage_token(#{'oauth_app_id' := AppId}) ->
    kz_auth_client:token_for_app(AppId, ?DRV_TOKEN_OPTIONS).

-spec send_attachment(binary(), kz_term:ne_binary(), kz_term:ne_binary(), binary(), kz_term:proplist(), binary()) ->
          {'ok', kz_term:proplist()} |
          {'ok', kz_term:ne_binary(), kz_term:proplist()} |
          {'error', kz_term:ne_binary(), kz_http:ret()}.
send_attachment(Authorization, Bucket, AName, CT, Options, Contents) ->
    JObj = kz_json:from_list(
             props:filter_empty(
               [{<<"mimeType">>, CT}
               ,{<<"name">>, AName}
               ,{<<"description">>, props:get_value('description', Options)}
               ,{<<"appProperties">>, kz_json:from_list(props:get_value('metadata', Options, [])) }
               ,{<<"properties">>, kz_json:from_list(props:get_value('metadata', Options, [])) }
               ])),

    JsonPart = {kz_json:encode(JObj)
               ,[{<<"Content-Type">>, <<"application/json">>}]
               },

    FilePart = {base64:encode(Contents)
               ,[{<<"Content-Type">>, CT}
                ,{<<"Content-Transfer-Encoding">>, <<"base64">>}
                ]
               },

    Boundary = <<"------", (kz_binary:rand_hex(16))/binary>>,

    Body = kz_http_util:encode_multipart([JsonPart, FilePart], Boundary),
    ContentType = kz_term:to_list(<<"multipart/related; boundary=", Boundary/binary>>),
    Headers = [{<<"Authorization">>, Authorization}
              ,{<<"Content-Type">>, ContentType}
              ],
    Url = ?DRV_MULTIPART_FILE_URL(Bucket),
    case kz_http:post(Url, Headers, Body) of
        {'ok', 200, ResponseHeaders, ResponseBody} ->
            props:to_log(ResponseHeaders, <<"GSTORAGE HEADERS">>),
            BodyJObj = kz_json:decode(ResponseBody),
            case kz_json:get_value(<<"mediaLink">>, BodyJObj) of
                'undefined' -> {'ok', [{<<"body">>, BodyJObj} | kz_att_util:headers_as_binaries(ResponseHeaders) ]};
                ContentUrl -> {'ok', ContentUrl, [{<<"body">>, BodyJObj} | kz_att_util:headers_as_binaries(ResponseHeaders)]}
            end;
        Resp -> {'error', Url, Resp}
    end.

%% Google Storage REST API errors reference: https://cloud.google.com/storage/docs/json_api/v1/status-codes#error-response-format
-spec handle_http_error_response(kz_http:req(), kz_att_error:update_routines()) -> kz_att_error:error().
handle_http_error_response({'ok', RespCode, RespHeaders, RespBody} = _E, Routines) ->
    Reason = get_reason(RespCode, RespBody),
    NewRoutines = [{fun kz_att_error:set_resp_code/2, RespCode}
                  ,{fun kz_att_error:set_resp_headers/2, RespHeaders}
                  ,{fun kz_att_error:set_resp_body/2, RespBody}
                   | Routines
                  ],
    lager:error("google storage error: ~p (code: ~p)", [_E, RespCode]),
    kz_att_error:new(Reason, NewRoutines);
handle_http_error_response({'error', {'failed_connect', Reason}} = _E, Routines) ->
    lager:error("google storage failed to connect: ~p", [_E]),
    kz_att_error:new(Reason, Routines);
handle_http_error_response({'error', {Reason, _}} = _E, Routines)
  when is_atom(Reason) ->
    lager:error("google storage request error: ~p", [_E]),
    kz_att_error:new(Reason, Routines);
handle_http_error_response(_E, Routines) ->
    lager:error("google storage request error: ~p", [_E]),
    kz_att_error:new('request_error', Routines).

-spec get_reason(atom() | pos_integer(), kz_term:ne_binary()) -> kz_term:ne_binary().
get_reason(RespCode, RespBody) when RespCode >= 400 ->
    %% If the `RespCode' value is >= 400 then the resp_body must contain an error object
    kz_json:get_ne_binary_value([<<"error">>, <<"message">>], kz_json:decode(RespBody));
get_reason(RespCode, _RespBody) ->
    kz_http_util:http_code_to_status_line(RespCode).
