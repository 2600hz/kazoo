%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
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

%% ====================================================================
%% API functions
%% ====================================================================

encode_multipart(Parts, Boundary) ->
    encode_multipart(Parts, Boundary, <<>>).

encode_multipart([], Boundary, Encoded) ->
    Close = <<"\r\n--" , Boundary/binary, "--">>,
    <<Encoded/binary, Close/binary>>;
encode_multipart([{Body, Headers} | Parts], Boundary, Encoded) ->
    Delimiter = <<"\r\n--" ,Boundary/binary, "\r\n">>,
    H = encode_multipart_headers(Headers),
    Acc = <<Encoded/binary, Delimiter/binary, H/binary, Body/binary>>,
    encode_multipart(Parts, Boundary, Acc).

encode_multipart_headers(Headers) ->
    encode_multipart_headers(Headers, <<>>).

encode_multipart_headers([], Encoded) -> <<Encoded/binary, "\r\n">>;
encode_multipart_headers([{K, V} | Headers], Encoded) ->
    Acc = <<Encoded/binary, K/binary, ": ", V/binary, "\r\n">>,
    encode_multipart_headers(Headers, Acc).

-spec get_json_from_url(ne_binary(), kz_proplist()) -> {'ok', kz_json:object()} | {'error', any()}.
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


resolve_ids([], Acc, _Authorization) ->
    lists:reverse(Acc);
resolve_ids([Id | Ids], [Parent | _]=Acc, Authorization) ->
    case resolve_id(Id, Parent, Authorization) of
        {ok, FolderId} -> resolve_ids(Ids, [FolderId | Acc], Authorization);
        {error, not_found} ->
            case create_folder(Id, Parent, Authorization) of
                {ok, FolderId} -> resolve_ids(Ids, [FolderId | Acc], Authorization);
                _Else -> lists:reverse(Acc)
            end;
        _Else -> lists:reverse(Acc)
    end.

resolve_ids(Path, Authorization) ->
    lager:debug("resolving path ~s", [Path]),
    Ids = binary:split(Path, <<"/">>, [global ,trim_all]),
    resolve_ids(Ids, [<<"root">>], Authorization).

resolve_path(Settings, Authorization) ->
    case maps:get(folder_path, Settings, undefined) of
        undefined ->
            lager:debug("no folder_path, saving to root"),
            [<<"root">>];
        Path -> resolve_ids(Path, Authorization)
    end.

resolve_folder(Settings, Authorization) ->
    case maps:get(folder_id, Settings, undefined) of
        undefined ->
            lager:debug("no folder_id defined, looking for folder_path"),
            [lists:last(resolve_path(Settings, Authorization))];
        Path -> [Path]
    end.

-spec put_attachment(kz_data:connection(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), kz_data:options()) -> any().
put_attachment(#{oauth_doc_id := TokenDocId}=Settings, _DbName, _DocId, AName, Contents, Options) ->
    {'ok', #{token := #{authorization := Authorization}}} = kz_auth_client:token_for_auth_id(TokenDocId, ?DRV_TOKEN_OPTIONS),
    CT = kz_mime:from_filename(AName),
    Folder = resolve_folder(Settings, Authorization),
    case send_attachment(Authorization, Folder, TokenDocId, AName, CT, Options, Contents) of
        {error, _E} when Folder /= [<<"root">>] ->
            send_attachment(Authorization, [<<"root">>], TokenDocId, AName, CT, Options, Contents);
        {ok, _}=OK -> OK
    end.

gdrive_post(Url, Headers, Body) ->
    case kz_http:post(Url, Headers, Body) of
        {'ok', 200, ResponseHeaders, ResponseBody} ->
            case kz_json:get_value(<<"id">>, kz_json:decode(ResponseBody)) of
                undefined -> ok;
                ContentId -> {ok, ContentId, ResponseHeaders}
            end;
        _E ->
            lager:debug("GDRIVE ERROR ~p", [_E]),
            {'error', 'google_drive_error'}
    end.


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

    Body = encode_multipart([JsonPart, FilePart], Boundary),
    ContentType = kz_term:to_list(<<"multipart/related; boundary=", Boundary/binary>>),
    Headers = [{<<"Authorization">>, Authorization}
              ,{<<"Content-Type">>, ContentType}
              ],
    case gdrive_post(?DRV_MULTIPART_FILE_URL, Headers, Body) of
        {ok, ContentId, ResponseHeaders} ->
            Data = base64:encode(term_to_binary({TokenDocId, ContentId})),
            Metadata = [ convert_kv(KV) || KV <- ResponseHeaders, filter_kv(KV)],
            {'ok', [{'attachment', [{<<"gdrive">>, Data}
                                   ,{<<"metadata">>, kz_json:from_list(Metadata)}
                                   ]}
                   ]};
        Else -> Else
    end.


filter_kv({"x-guploader-uploadid", _V}) -> 'true';
filter_kv(_KV) -> 'false'.

convert_kv({K, V})
  when is_list(K) ->
    convert_kv({kz_term:to_binary(K), V});
convert_kv({K, V})
  when is_list(V) ->
    convert_kv({K, kz_term:to_binary(V)});
convert_kv(KV) -> KV.

-spec fetch_attachment(kz_data:connection(), ne_binary(), ne_binary(), ne_binary()) ->
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
                    lager:debug("GDRIVE FETCH ERROR ~p", [_E]),
                    {'error', 'not_found'}
            end
    end.
