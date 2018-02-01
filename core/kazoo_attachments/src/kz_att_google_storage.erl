%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% Google Storage for attachments
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-----------------------------------------------------------------------------

-module(kz_att_google_storage).

-include("kz_att.hrl").

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
%% API functions
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

-spec put_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> any().
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

-spec send_attachment(binary(), kz_term:ne_binary(), kz_term:ne_binary(), binary(), kz_term:proplist(), binary()) ->
                             {ok, kz_term:proplist()} | {'error', 'gstorage_drive_error'}.
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
    case kz_http:post(?DRV_MULTIPART_FILE_URL(Bucket), Headers, Body) of
        {'ok', 200, ResponseHeaders, ResponseBody} ->
            props:to_log(ResponseHeaders, <<"GSTORAGE HEADERS">>),
            BodyJObj = kz_json:decode(ResponseBody),
            case kz_json:get_value(<<"mediaLink">>, BodyJObj) of
                undefined -> {ok, [{<<"body">>, BodyJObj} | kz_att_util:headers_as_binaries(ResponseHeaders) ]};
                ContentUrl -> {ok, ContentUrl, [{<<"body">>, BodyJObj} | kz_att_util:headers_as_binaries(ResponseHeaders)]}
            end;
        _E ->
            lager:error("GSTORAGE ERROR ~p", [_E]),
            {'error', 'google_storage_error'}
    end.

-spec fetch_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                              {'ok', iodata()} |
                              {'error', 'invalid_data' | 'not_found'}.
fetch_attachment(HandlerProps, _DbName, _DocId, _AName) ->
    case kz_json:get_value(<<"gstorage">>, HandlerProps) of
        'undefined' -> {'error', 'invalid_data'};
        GData ->
            {#{bucket := Bucket} = Settings, ContentId} = binary_to_term(base64:decode(GData)),
            Authorization = gstorage_token(Settings),
            Headers = [{<<"Authorization">>, Authorization}],
            Url = case ContentId of
                      {name, Name} -> <<(?DRV_BASE_FETCH_URL(Bucket))/binary, "/", Name/binary>>;
                      ContentId -> ContentId
                  end,
            case kz_http:get(Url, Headers) of
                {'ok', 200, _ResponseHeaders, ResponseBody} -> {'ok', ResponseBody};
                _E ->
                    lager:error("GSTORAGE FETCH ERROR ~p", [_E]),
                    {'error', 'not_found'}
            end
    end.

