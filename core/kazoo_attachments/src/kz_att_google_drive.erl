%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% Google Drive for attachments
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-----------------------------------------------------------------------------

-module(kz_att_google_drive).

-include("kz_att.hrl").

-export([put_attachment/6]).
-export([fetch_attachment/4]).

-define(DRV_BASE_UPLOAD_URL, <<"https://www.googleapis.com/upload/drive/v3">>).
-define(DRV_FILE_UPLOAD_URL, <<(?DRV_BASE_UPLOAD_URL)/binary, "/files">>).
-define(DRV_SINGLE_FILE_UPLOAD_URL, <<(?DRV_FILE_UPLOAD_URL)/binary, "?uploadType=media">>).
-define(DRV_FOLDER_URL, <<"https://www.googleapis.com/drive/v3/files?fields=id">>).
-define(DRV_MULTIPART_FILE_URL, <<(?DRV_FILE_UPLOAD_URL)/binary, "?fields=id&uploadType=multipart">>).
-define(FOLDER_CT, <<"application/vnd.google-apps.folder">>).
-define(DRV_BASE_FETCH_URL2, "https://www.googleapis.com/drive/v2/files/").
-define(DRV_BASE_FETCH_URL, "https://www.googleapis.com/drive/v3/files").
-define(DRV_SCOPE, <<"https://www.googleapis.com/auth/drive.file">>).
-define(DRV_SCOPES, [?DRV_SCOPE]).
-define(DRV_TOKEN_OPTIONS, #{scopes => ?DRV_SCOPES}).
-define(DRV_FOLDER_CT, <<"application/vnd.google-apps.folder">>).

-type gdrive_result() :: {'ok', binary()} | {'error', 'google_drive_error'}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_json_from_url(kz_term:ne_binary(), kz_term:proplist()) -> {'ok', kz_json:object()} | {'error', any()}.
get_json_from_url(Url, ReqHeaders) ->
    case kz_http:get(kz_term:to_list(Url), ReqHeaders, [{ssl, [{versions, ['tlsv1.2']}]}]) of
        {'ok', 200, _RespHeaders, Body} ->
            JObj = kz_json:decode(Body),
            case kz_term:is_empty(JObj) of
                'true' -> {'error', 'empty'};
                'false' -> {'ok', JObj}
            end;
        {'ok', Code, _RespHeaders, Body} ->
            lager:debug("unexpected code ~b downloading json from ~s : ~s", [Code, Url, Body]),
            {'error', 'unexpected_result'};
        Else ->
            lager:debug("error downloading json from ~s : ~p", [Url, Else]),
            {'error', Else}
    end.

-spec create_folder(binary(), binary(), binary()) -> gdrive_result().
create_folder(Name, Parent, Authorization) ->
    Headers = [{<<"Authorization">>, Authorization}
              ,{<<"Content-Type">>, <<"application/json">>}
              ],
    Fields = [{<<"mimeType">>, ?DRV_FOLDER_CT}
             ,{<<"name">>, Name}
             ,{<<"parents">>, [Parent]}
             ],
    Body = kz_json:encode(kz_json:from_list(Fields)),
    case gdrive_post(?DRV_FOLDER_URL, Headers, Body) of
        {ok, FolderId, _} -> {ok, FolderId};
        Else -> Else
    end.

-spec resolve_id(binary(), binary(), binary()) -> {ok, binary()} | {error, not_found}.
resolve_id(Name, Parent, Authorization) ->
    Params = ["\"", Parent, "\" in parents"
             ," and name = \"", Name, "\""
             ," and mimeType = \"application/vnd.google-apps.folder\""
             ],
    Q = kz_http_util:urlencode(list_to_binary(Params)),
    Url = <<?DRV_BASE_FETCH_URL, "?q=", Q/binary>>,
    case get_json_from_url(Url, [{<<"Authorization">>, Authorization}]) of
        {ok, JObj} ->
            case kz_json:get_list_value(<<"files">>, JObj, []) of
                [] -> {error, not_found};
                [FolderJObj | _] -> {ok, kz_json:get_ne_binary_value(<<"id">>, FolderJObj)}
            end;
        _Else -> {error, not_found}
    end.


-spec resolve_ids(kz_term:binaries(), kz_term:binaries(), binary()) -> kz_term:binaries().
resolve_ids([], Acc, _Authorization) ->
    lists:reverse(Acc);
resolve_ids([Id | Ids], [Parent | _]=Acc, Authorization) ->
    case resolve_id(Id, Parent, Authorization) of
        {ok, FolderId} -> resolve_ids(Ids, [FolderId | Acc], Authorization);
        {error, not_found} ->
            case create_folder(Id, Parent, Authorization) of
                {ok, FolderId} -> resolve_ids(Ids, [FolderId | Acc], Authorization);
                _Else -> lists:reverse(Acc)
            end
    end.

-spec resolve_folder(map(), kz_term:ne_binaries(), binary()) -> kz_term:ne_binaries().
resolve_folder(Settings, PathTokens, Authorization) ->
    case maps:get(folder_id, Settings, undefined) of
        undefined ->
            lager:debug("no folder_id defined, looking for folder_path"),
            [lists:last(resolve_ids(PathTokens, [<<"root">>], Authorization))];
        Path -> [Path]
    end.

-spec resolve_path(map(), attachment_info(), binary()) -> {kz_term:ne_binaries(), kz_term:ne_binary()}.
resolve_path(Settings, AttInfo, Authorization) ->
    Url = gdrive_format_url(Settings, AttInfo),
    PathTokens = binary:split(Url, <<"/">>, ['global', 'trim_all']),
    Name = lists:last(PathTokens),
    Folder = resolve_folder(Settings, lists:droplast(PathTokens), Authorization),
    {Folder, Name}.

-spec gdrive_default_fields() -> kz_term:proplist().
gdrive_default_fields() ->
    [{group, [{arg, <<"id">>}
             ,<<"_">>
             ,{arg, <<"attachment">>}
             ]}
    ].

-spec gdrive_format_url(map(), attachment_info()) -> kz_term:ne_binary().
gdrive_format_url(Map, AttInfo) ->
    kz_att_util:format_url(Map, AttInfo, gdrive_default_fields()).

-spec put_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> any().
put_attachment(#{oauth_doc_id := TokenDocId}=Settings, DbName, DocId, AName, Contents, Options) ->
    {'ok', #{token := #{authorization := Authorization}}} = kz_auth_client:token_for_auth_id(TokenDocId, ?DRV_TOKEN_OPTIONS),
    CT = kz_mime:from_filename(AName),
    {Folder, Name} = resolve_path(Settings, {DbName, DocId, AName}, Authorization),
    case send_attachment(Authorization, Folder, TokenDocId, Name, CT, Options, Contents) of
        {error, _E} when Folder /= [<<"root">>] ->
            send_attachment(Authorization, [<<"root">>], TokenDocId, Name, CT, Options, Contents);
        Else -> Else
    end.

-spec gdrive_post(binary(), kz_term:proplist(), binary()) -> gdrive_result().
gdrive_post(Url, Headers, Body) ->
    case kz_http:post(Url, Headers, Body) of
        {'ok', 200, ResponseHeaders, ResponseBody} ->
            BodyJObj = kz_json:decode(ResponseBody),
            case kz_json:get_value(<<"id">>, BodyJObj) of
                undefined -> {error, 'return_id_missing'};
                ContentId -> {ok, ContentId, [{<<"body">>, BodyJObj} | kz_att_util:headers_as_binaries(ResponseHeaders)]}
            end;
        _E ->
            lager:error("GDRIVE ERROR ~p", [_E]),
            {'error', 'google_drive_error'}
    end.


-spec send_attachment(binary(), kz_term:binaries(), binary(), binary(), binary(), kz_term:proplist(), binary()) ->
                             {ok, kz_term:proplist()} | {'error', 'google_drive_error'}.
send_attachment(Authorization, Folder, TokenDocId, AName, CT, Options, Contents) ->
    JObj = kz_json:from_list(
             props:filter_empty(
               [{<<"mimeType">>, CT}
               ,{<<"name">>, AName}
               ,{<<"parents">>, Folder}
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
    case gdrive_post(?DRV_MULTIPART_FILE_URL, Headers, Body) of
        {ok, ContentId, ResponseHeaders} ->
            Data = base64:encode(term_to_binary({TokenDocId, ContentId})),
            {'ok', [{'attachment', [{<<"gdrive">>, Data}
                                   ,{<<"metadata">>, kz_json:from_list(ResponseHeaders)}
                                   ]}
                   ]};
        Else -> Else
    end.

-spec fetch_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                              {'ok', iodata()} |
                              {'error', 'invalid_data' | 'not_found'}.
fetch_attachment(HandlerProps, _DbName, _DocId, _AName) ->
    case kz_json:get_value(<<"gdrive">>, HandlerProps) of
        'undefined' -> {'error', 'invalid_data'};
        GData ->
            {TokenDocId, ContentId} = binary_to_term(base64:decode(GData)),
            {'ok', #{token := #{authorization := Authorization}}} = kz_auth_client:token_for_auth_id(TokenDocId, ?DRV_TOKEN_OPTIONS),
            Headers = [{<<"Authorization">>, Authorization}],
            Url = <<?DRV_BASE_FETCH_URL, "/", ContentId/binary, "?alt=media">>,
            case kz_http:get(Url, Headers) of
                {'ok', 200, _ResponseHeaders, ResponseBody} -> {'ok', ResponseBody};
                _E ->
                    lager:error("GDRIVE FETCH ERROR ~p", [_E]),
                    {'error', 'not_found'}
            end
    end.
