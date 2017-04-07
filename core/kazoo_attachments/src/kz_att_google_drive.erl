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
-define(DRV_FOLDER_URL, <<"https://content.googleapis.com/drive/v3/files?fields=id">>).
-define(DRV_MULTIPART_FILE_URL, <<(?DRV_FILE_UPLOAD_URL)/binary, "?fields=id&uploadType=multipart">>).
-define(FOLDER_CT, <<"application/vnd.google-apps.folder">>).
-define(DRV_BASE_FETCH_URL, "https://www.googleapis.com/drive/v2/files/").
-define(DRV_SCOPE, <<"https://www.googleapis.com/auth/drive.file">>).
-define(DRV_SCOPES, [?DRV_SCOPE]).
-define(DRV_TOKEN_OPTIONS, #{scopes => ?DRV_SCOPES}).

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

-spec put_attachment(kz_data:connection(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), kz_data:options()) -> any().
put_attachment(#{oauth_doc_id := TokenDocId, folder_id := Folder}, _DbName, _DocId, AName, Contents, Options) ->
    {'ok', #{token := #{authorization := Authorization}}} = kz_auth_client:token_for_auth_id(TokenDocId, ?DRV_TOKEN_OPTIONS),
    CT = kz_mime:from_filename(AName),
    JObj = kz_json:from_list(
             props:filter_empty(
               [{<<"mimeType">>, CT}
               ,{<<"name">>, AName}
               ,{<<"parents">>, [Folder]}
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
%%    Headers = [{<<"Authorization">>, kazoo_oauth_util:authorization_header(Token)}
    Headers = [{<<"Authorization">>, Authorization}
              ,{<<"Content-Type">>, ContentType}
              ],
    case kz_http:post(?DRV_MULTIPART_FILE_URL, Headers, Body) of
        {'ok', 200, ResponseHeaders, ResponseBody} ->
            ContentId = kz_json:get_value(<<"id">>, kz_json:decode(ResponseBody)),
            Data = base64:encode(term_to_binary({TokenDocId, ContentId})),
            Metadata = [ convert_kv(KV) || KV <- ResponseHeaders, filter_kv(KV)],
            {'ok', [{'attachment', [{<<"gdrive">>, Data}
                                   ,{<<"metadata">>, kz_json:from_list(Metadata)}
                                   ]}
                   ]};
        _E ->
            lager:debug("GDRIVE ERROR ~p", [_E]),
            {'error', 'google_drive_error'}
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
            {'ok', Token} = kazoo_oauth_util:token(TokenDocId),
            Headers = [{<<"Authorization">>, kazoo_oauth_util:authorization_header(Token)}],
            Url = <<?DRV_BASE_FETCH_URL, ContentId/binary, "?alt=media">>,
            case kz_http:get(Url, Headers) of
                {'ok', 200, _ResponseHeaders, ResponseBody} -> {'ok', ResponseBody};
                _E ->
                    lager:debug("GDRIVE FETCH ERROR ~p", [_E]),
                    {'error', 'not_found'}
            end
    end.
