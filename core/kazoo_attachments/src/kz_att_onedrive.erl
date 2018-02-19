%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% Google Drive for attachments
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-----------------------------------------------------------------------------

-module(kz_att_onedrive).
-behaviour(gen_attachment).

-include("kz_att.hrl").

%% `gen_attachment' behaviour callbacks (API)
-export([put_attachment/6]).
-export([fetch_attachment/4]).

-define(GRAPH_HTTP_OPTIONS, [{version, "HTTP/1.1"}, {ssl, [{versions, ['tlsv1.2']}]}]).

-define(GRAPH_API_URL, <<"https://graph.microsoft.com/v1.0">>).

-define(GRAPH_ME, <<"me">>).
-define(GRAPH_USER(U), <<"users/", U/binary>>).
-define(GRAPH_GROUP(G), <<"/groups/", G/binary>>).

-define(GRAPH_DRIVE, <<"drive">>).

-define(GRAPH_ROOT, <<"root:">>).
-define(GRAPH_ITEMS, <<"items">>).

-define(GRAPH_CONTENT, <<"content">>).

-define(GRAPH_ME_UPLOAD_URL(PATH), [?GRAPH_API_URL
                                   ,?GRAPH_ME
                                   ,?GRAPH_DRIVE
                                   ,?GRAPH_ROOT
                                   ,<<PATH/binary, ":">>
                                   ,?GRAPH_CONTENT
                                   ]).

-define(DRV_FETCH_URL(D,I), <<(?GRAPH_API_URL)/binary, "/drives/", D/binary, "/items/", I/binary, "/content">>).

%% TODO review scope
-define(DRV_PUT_SCOPE, <<"Files.ReadWrite.All">>).
-define(DRV_PUT_SCOPES, [?DRV_PUT_SCOPE]).
-define(DRV_PUT_TOKEN_OPTIONS, #{scopes => ?DRV_PUT_SCOPES
                                ,http_options => ?GRAPH_HTTP_OPTIONS
                                }).

-define(DRV_GET_SCOPE, <<"Files.Read.All">>).
-define(DRV_GET_SCOPES, [?DRV_GET_SCOPE]).
-define(DRV_GET_TOKEN_OPTIONS, #{scopes => ?DRV_GET_SCOPES
                                ,http_options => ?GRAPH_HTTP_OPTIONS
                                }).

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
put_attachment(Settings, DbName, DocId, AName, Contents, _Options) ->
    Authorization = onedrive_token(Settings, ?DRV_PUT_TOKEN_OPTIONS),
    Headers = [{<<"Authorization">>, Authorization}
              ,{<<"Content-Type">>, kz_mime:from_filename(AName)}
              ],
    Url = resolve_put_url(Settings, {DbName, DocId, AName}),
    case onedrive_put(Url, Headers, Contents) of
        {ok, ContentId, ResponseHeaders} ->
            Data = base64:encode(term_to_binary({Settings, ContentId})),
            {'ok', [{'attachment', [{<<"onedrive">>, Data}
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
    case kz_json:get_value(<<"onedrive">>, HandlerProps) of
        'undefined' -> gen_attachment:error_response(400, 'invalid_data');
        GData ->
            {Settings, {DriveId, ContentId}} = binary_to_term(base64:decode(GData)),
            Authorization = onedrive_token(Settings, ?DRV_GET_TOKEN_OPTIONS),
            Headers = [{<<"Authorization">>, Authorization}],
            Url = ?DRV_FETCH_URL(DriveId, ContentId),
            lager:debug("getting ~p", [Url]),
            case kz_http:get(Url, Headers, ?GRAPH_HTTP_OPTIONS) of
                {'ok', 200, _ResponseHeaders, ResponseBody} ->
                    {'ok', ResponseBody};
                Resp ->
                    kz_att_util:handle_http_error_response(Resp, "Graph fetch error", Url)
            end
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
-spec resolve_put_url(map(), attachment_info()) -> kz_term:ne_binary().
resolve_put_url(Settings, AttInfo) ->
    Url = onedrive_format_url(Settings, AttInfo),
    kz_binary:join(?GRAPH_ME_UPLOAD_URL(Url), <<"/">>).

-spec onedrive_default_fields() -> kz_term:proplist().
onedrive_default_fields() ->
    [{group, [{arg, <<"id">>}
             ,<<"_">>
             ,{arg, <<"attachment">>}
             ]}
    ].

-spec onedrive_format_url(map(), attachment_info()) -> kz_term:ne_binary().
onedrive_format_url(Map, AttInfo) ->
    kz_att_util:format_url(Map, AttInfo, onedrive_default_fields()).

-spec onedrive_token(map(), map()) -> kz_term:ne_binary().
onedrive_token(#{oauth_doc_id := TokenDocId}, Options) ->
    {'ok', #{token := #{authorization := Authorization}}} = kz_auth_client:token_for_auth_id(TokenDocId, Options),
    Authorization.

-spec onedrive_put(binary(), kz_term:proplist(), binary()) ->
                          {'ok', tuple(), kz_term:proplist()} | gen_attachment:error_response().
onedrive_put(Url, Headers, Body) ->
    case kz_http:put(Url, Headers, Body, ?GRAPH_HTTP_OPTIONS) of
        {'ok', Code, ResponseHeaders, ResponseBody}
          when Code < 300 ->
            BodyJObj = kz_json:decode(ResponseBody),
            props:to_log(ResponseHeaders, <<"GRAPH HEADERS">>),
            lager:debug("graph result body : ~s", [kz_json:encode(BodyJObj, ['pretty'])]),
            Id = kz_json:get_value(<<"id">>, BodyJObj),
            DriveId = kz_json:get_value([<<"parentReference">>, <<"driveId">>], BodyJObj),
            case {Id, DriveId} of
                {undefined, undefined} -> gen_attachment:error_response(400, 'return_info_missing');
                {undefined, _} -> gen_attachment:error_response(400, 'return_id_missing');
                {_, undefined} -> gen_attachment:error_response(400, 'return_drive_missing');
                _ -> {ok, {DriveId, Id}, [{<<"body">>, BodyJObj} | kz_att_util:headers_as_binaries(ResponseHeaders)]}
            end;
        Resp ->
            kz_att_util:handle_http_error_response(Resp, "Graph error", Url)
    end.
