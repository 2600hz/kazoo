%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% Google Storage for attachments
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-----------------------------------------------------------------------------

-module(kz_att_google_storage).
-behaviour(gen_attachment).

-include("kz_att.hrl").

%% `gen_attachment' behaviour callbacks (API)
-export([put_attachment/6]).
-export([fetch_attachment/4]).

-define(DRV_BUCKET_UPLOAD_URL(B), <<"https://www.googleapis.com/upload/storage/v1/b/", B/binary, "/o">>).
-define(DRV_SINGLE_FILE_UPLOAD_URL(B), <<(?DRV_BUCKET_UPLOAD_URL(B))/binary, "?uploadType=media">>).
-define(DRV_MULTIPART_FILE_URL(B), <<(?DRV_BUCKET_UPLOAD_URL(B))/binary, "?uploadType=multipart">>).
-define(DRV_BASE_FETCH_URL(B), <<"https://www.googleapis.com/storage/v1/b/", B/binary, "/o">>).

-define(DRV_SCOPE, <<"https://www.googleapis.com/auth/devstorage.read_write">>).
-define(DRV_SCOPES, [?DRV_SCOPE]).
-define(DRV_TOKEN_OPTIONS, #{scopes => ?DRV_SCOPES}).

%% ====================================================================
%% `gen_attachment' behaviour callbacks (API)
%% ====================================================================
-spec put_attachment(gen_attachment:settings()
                    ,gen_attachment:db_name()
                    ,gen_attachment:doc_id()
                    ,gen_attachment:att_name()
                    ,gen_attachment:contents()
                    ,gen_attachment:options()
                    ) -> gen_attachment:put_response().
put_attachment(Settings, DbName, DocId, AName, Contents, Options) ->
    Authorization = gstorage_token(Settings),
    CT = kz_mime:from_filename(AName),
    {Bucket, Name} = resolve_path(Settings, {DbName, DocId, AName}),
    case send_attachment(Authorization, Bucket, Name, CT, Options, Contents) of
        {ok, ContentUrl, ResponseHeaders} ->
            props:to_log(ResponseHeaders, <<"GSTORAGE2 HEADERS">>),
            Data = base64:encode(term_to_binary({Settings, ContentUrl})),
            {'ok', [{'attachment', [{<<"gstorage">>, Data}
                                   ,{<<"metadata">>, kz_json:from_list(ResponseHeaders)}
                                   ]}
                   ]};
        {ok, ResponseHeaders} ->
            Data = base64:encode(term_to_binary({Settings, {name, Name}})),
            {'ok', [{'attachment', [{<<"gstorage">>, Data}
                                   ,{<<"metadata">>, kz_json:from_list(ResponseHeaders)}
                                   ]}
                   ]};
        Else -> Else
    end.

-spec fetch_attachment(gen_attachment:handler_props()
                      ,gen_attachment:db_name()
                      ,gen_attachment:doc_id()
                      ,gen_attachment:att_name()
                      ) -> gen_attachment:fetch_response().
fetch_attachment(HandlerProps, _DbName, _DocId, _AName) ->
    case kz_json:get_value(<<"gstorage">>, HandlerProps) of
        'undefined' ->
            gen_attachment:error_response(400, 'invalid_data');
        GData ->
            {#{bucket := Bucket} = Settings, ContentId} = binary_to_term(base64:decode(GData)),
            Authorization = gstorage_token(Settings),
            Headers = [{<<"Authorization">>, Authorization}],
            Url = case ContentId of
                      {name, Name} -> <<(?DRV_BASE_FETCH_URL(Bucket))/binary, "/", Name/binary>>;
                      ContentId -> ContentId
                  end,
            case kz_http:get(Url, Headers) of
                {'ok', 200, _ResponseHeaders, ResponseBody} ->
                    {'ok', ResponseBody};
                Resp ->
                    kz_att_util:handle_http_error_response(Resp, "GStorage fetch error", Url)
            end
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
-spec gstorage_default_fields() -> kz_term:proplist().
gstorage_default_fields() ->
    [{group, [{arg, <<"id">>}
             ,<<"_">>
             ,{arg, <<"attachment">>}
             ]}
    ].

-spec gstorage_format_url(map(), attachment_info()) -> kz_term:ne_binary().
gstorage_format_url(Map, AttInfo) ->
    kz_att_util:format_url(Map, AttInfo, gstorage_default_fields()).

-spec resolve_path(map(), attachment_info()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
resolve_path(#{bucket := Bucket} = Settings, AttInfo) ->
    Url = gstorage_format_url(Settings, AttInfo),
    {Bucket, Url}.

-spec gstorage_token(map()) -> kz_term:ne_binary().
gstorage_token(#{oauth_doc_id := TokenDocId}) ->
    {'ok', #{token := #{authorization := Authorization}}} = kz_auth_client:token_for_auth_id(TokenDocId, ?DRV_TOKEN_OPTIONS),
    Authorization;
gstorage_token(#{oauth_app_id := AppId}) ->
    {'ok', #{token := #{authorization := Authorization}}} = kz_auth_client:token_for_app(AppId, ?DRV_TOKEN_OPTIONS),
    Authorization.

-spec send_attachment(binary(), kz_term:ne_binary(), kz_term:ne_binary(), binary(), kz_term:proplist(), binary()) ->
                             {'ok', kz_term:proplist()} |
                             {'ok', kz_term:ne_binary(), kz_term:proplist()} |
                             gen_attachment:error_response().
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

    Body = kz_att_util:encode_multipart([JsonPart, FilePart], Boundary),
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
                undefined -> {ok, [{<<"body">>, BodyJObj} | kz_att_util:headers_as_binaries(ResponseHeaders) ]};
                ContentUrl -> {ok, ContentUrl, [{<<"body">>, BodyJObj} | kz_att_util:headers_as_binaries(ResponseHeaders)]}
            end;
        Resp ->
            kz_att_util:handle_http_error_response(Resp, "GStorage error", Url)
    end.
