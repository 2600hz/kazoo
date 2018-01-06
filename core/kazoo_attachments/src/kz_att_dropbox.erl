%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% Dropbox for attachments
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-----------------------------------------------------------------------------

-module(kz_att_dropbox).

-include("kz_att.hrl").

-export([put_attachment/6]).
-export([fetch_attachment/4]).

-define(DRV_UPLOAD_URL, <<"https://content.dropboxapi.com/2/files/upload">>).
-define(DRV_FETCH_URL, <<"https://content.dropboxapi.com/2/files/download">>).
-define(DRV_DROPBOX_HEADER(I), {<<"Dropbox-API-Arg">>, kz_json:encode(kz_json:from_list([{<<"path">>, I}]))}).

-type dropbox_result() :: {'ok', binary()} | {'error', 'dropbox_error'}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec resolve_path(map(), attachment_info()) -> kz_term:ne_binary().
resolve_path(Settings, AttInfo) ->
    <<"/", (dropbox_format_url(Settings, AttInfo))/binary>>.

-spec dropbox_default_fields() -> kz_term:proplist().
dropbox_default_fields() ->
    [{group, [{arg, <<"id">>}
             ,<<"_">>
             ,{arg, <<"attachment">>}
             ]}
    ].

-spec dropbox_format_url(map(), attachment_info()) -> kz_term:ne_binary().
dropbox_format_url(Map, AttInfo) ->
    kz_att_util:format_url(Map, AttInfo, dropbox_default_fields()).

dropbox_token(#{oauth_doc_id := TokenDocId}) ->
    {'ok', #{token := #{authorization := Authorization}}} = kz_auth_client:token_for_auth_id(TokenDocId),
    Authorization.

-spec put_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> any().
put_attachment(Settings, DbName, DocId, AName, Contents, _Options) ->
    Authorization = dropbox_token(Settings),
    Url = resolve_path(Settings, {DbName, DocId, AName}),
    Headers = [{<<"Authorization">>, Authorization}
              ,{<<"Content-Type">>, <<"application/octet-stream">>}
              ,?DRV_DROPBOX_HEADER(Url)
              ],
    case dropbox_post(?DRV_UPLOAD_URL, Headers, Contents) of
        {ok, ContentId, ResponseHeaders} ->
            Data = base64:encode(term_to_binary({Settings, ContentId})),
            {'ok', [{'attachment', [{<<"dropbox">>, Data}
                                   ,{<<"metadata">>, kz_json:from_list(ResponseHeaders)}
                                   ]}
                   ]};
        Else -> Else
    end.

-spec dropbox_post(binary(), kz_term:proplist(), binary()) -> dropbox_result().
dropbox_post(Url, Headers, Body) ->
    case kz_http:post(Url, Headers, Body) of
        {'ok', 200, ResponseHeaders, ResponseBody} ->
            BodyJObj = kz_json:decode(ResponseBody),
            case kz_json:get_value(<<"id">>, BodyJObj) of
                undefined -> {error, 'return_id_missing'};
                ContentId -> {ok, ContentId, [{<<"body">>, BodyJObj} | kz_att_util:headers_as_binaries(ResponseHeaders)]}
            end;
        _E ->
            lager:error("DROPBOX ERROR ~p", [_E]),
            {'error', 'dropbox_error'}
    end.

-spec fetch_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                              {'ok', iodata()} |
                              {'error', 'invalid_data' | 'not_found'}.
fetch_attachment(HandlerProps, _DbName, _DocId, _AName) ->
    case kz_json:get_value(<<"dropbox">>, HandlerProps) of
        'undefined' -> {'error', 'invalid_data'};
        GData ->
            {Settings, ContentId} = binary_to_term(base64:decode(GData)),
            Authorization = dropbox_token(Settings),

            Headers = [{<<"Authorization">>, Authorization}
                      ,?DRV_DROPBOX_HEADER(ContentId)
                      ],
            case kz_http:get(?DRV_FETCH_URL, Headers) of
                {'ok', 200, _ResponseHeaders, ResponseBody} -> {'ok', ResponseBody};
                _E ->
                    lager:error("DROPBOX FETCH ERROR ~p", [_E]),
                    {'error', 'not_found'}
            end
    end.
