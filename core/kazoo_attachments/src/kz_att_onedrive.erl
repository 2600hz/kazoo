%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2019, 2600Hz
%%% @doc Google Drive for attachments.
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_att_onedrive).
-behaviour(gen_attachment).

-include("kz_att.hrl").

%% gen_attachment behaviour callbacks (API)
-export([put_attachment/6]).
-export([fetch_attachment/4]).

-export([onedrive_default_fields/0]).

-define(GRAPH_HTTP_OPTIONS, [{'version', "HTTP/1.1"}
                            ,{'ssl', [{'versions', ['tlsv1.2']}]}
                            ]).

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
put_attachment(#{'oauth_doc_id' := TokenDocId} = Settings,
               DbName, DocId, AName, Contents, Options) ->
    Authorization = kz_auth_client:token_for_auth_id(TokenDocId, ?DRV_PUT_TOKEN_OPTIONS),
    do_put_attachment(Authorization, Settings, DbName, DocId, AName, Contents, Options).

-spec do_put_attachment(kz_auth_client:token()
                       ,gen_attachment:settings()
                       ,gen_attachment:db_name()
                       ,gen_attachment:doc_id()
                       ,gen_attachment:att_name()
                       ,gen_attachment:contents()
                       ,gen_attachment:options()
                       ) -> gen_attachment:put_response().
do_put_attachment({'ok', #{'token' := #{'authorization' := Authorization}}}
                 ,Settings ,DbName, DocId, AName, Contents, Options) ->
    Routines = kz_att_error:put_routines(Settings, DbName, DocId, AName, Contents, Options),
    Headers = [{<<"Authorization">>, Authorization}
              ,{<<"Content-Type">>, kz_mime:from_filename(AName)}
              ],
    Url = resolve_put_url(Settings, {DbName, DocId, AName}),
    case onedrive_put(Url, Headers, Contents) of
        {'ok', ContentId, ResponseHeaders} ->
            Data = base64:encode(term_to_binary({Settings, ContentId})),
            {'ok', [{'attachment', [{<<"onedrive">>, Data}
                                   ,{<<"metadata">>, kz_json:from_list(ResponseHeaders)}
                                   ]}
                   ]};
        Resp ->
            handle_put_attachment_resp(Resp, Routines)
    end;
do_put_attachment({'error', _}, Settings, DbName, DocId, AName, Contents, Options) ->
    Routines = kz_att_error:put_routines(Settings, DbName, DocId, AName, Contents, Options),
    kz_att_error:new('oauth_failure', Routines).

-spec handle_put_attachment_resp({'error', kz_term:ne_binary(), kz_http:ret() | atom()} |
                                 gen_attachment:put_response()
                                ,kz_att_error:update_routines()
                                ) -> gen_attachment:put_response().
handle_put_attachment_resp({'error', Url, Resp}, Routines) ->
    NewRoutines = [{fun kz_att_error:set_req_url/2, Url}
                   | Routines
                  ],
    handle_http_error_response(Resp, NewRoutines).

-spec fetch_attachment(gen_attachment:handler_props()
                      ,gen_attachment:db_name()
                      ,gen_attachment:doc_id()
                      ,gen_attachment:att_name()
                      ) -> gen_attachment:fetch_response().
fetch_attachment(HandlerProps, DbName, DocId, AName) ->
    case kz_json:get_value(<<"onedrive">>, HandlerProps) of
        'undefined' ->
            Routines = kz_att_error:fetch_routines(HandlerProps, DbName, DocId, AName),
            kz_att_error:new('invalid_data', Routines);
        GData ->
            {#{oauth_doc_id := TokenDocId},
             {DriveId, ContentId}} = binary_to_term(base64:decode(GData)),
            Authorization = kz_auth_client:token_for_auth_id(TokenDocId, ?DRV_GET_TOKEN_OPTIONS),
            do_fetch_attachment(Authorization, DriveId, ContentId, HandlerProps, DbName, DocId, AName)
    end.

-spec do_fetch_attachment(kz_auth_client:token()
                         ,kz_term:ne_binary()
                         ,kz_term:ne_binary()
                         ,gen_attachment:handler_props()
                         ,gen_attachment:db_name()
                         ,gen_attachment:doc_id()
                         ,gen_attachment:att_name()
                         ) -> gen_attachment:fetch_response().
do_fetch_attachment({'ok', #{'token' := #{'authorization' := Authorization}}},
                    DriveId, ContentId, HandlerProps, DbName, DocId, AName) ->
    Headers = [{<<"Authorization">>, Authorization}],
    Url = ?DRV_FETCH_URL(DriveId, ContentId),
    lager:debug("getting ~p", [Url]),
    case kz_http:get(Url, Headers, ?GRAPH_HTTP_OPTIONS) of
        {'ok', 200, _ResponseHeaders, ResponseBody} ->
            {'ok', ResponseBody};
        Resp ->
            Routines = [{fun kz_att_error:set_req_url/2, Url}
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
-spec resolve_put_url(map(), attachment_info()) -> kz_term:ne_binary().
resolve_put_url(Settings, AttInfo) ->
    Url = onedrive_format_url(Settings, AttInfo),
    kz_binary:join(?GRAPH_ME_UPLOAD_URL(Url), <<"/">>).

-spec onedrive_default_fields() -> url_fields().
onedrive_default_fields() ->
    [{'group', [{'arg', <<"id">>}
               ,{'const', <<"_">>}
               ,{'arg', <<"attachment">>}
               ]}
    ].

-spec onedrive_format_url(map(), attachment_info()) -> kz_term:ne_binary().
onedrive_format_url(Map, AttInfo) ->
    kz_att_util:format_url(Map, AttInfo, onedrive_default_fields()).

-spec onedrive_put(binary(), kz_term:proplist(), binary()) ->
                          {'ok', {binary(), binary()}, kz_term:proplist()} |
                          {'error', kz_term:ne_binary(), kz_http:ret() | atom()}.
onedrive_put(Url, Headers, Body) ->
    case kz_http:put(Url, Headers, Body, ?GRAPH_HTTP_OPTIONS) of
        {'ok', Code, ResponseHeaders, ResponseBody}
          when Code < 300 ->
            BodyJObj = kz_json:decode(ResponseBody),
            props:to_log(ResponseHeaders, <<"GRAPH HEADERS">>),
            lager:debug("graph result body : ~s", [kz_json:encode(BodyJObj, ['pretty'])]),

            case {kz_json:get_value(<<"id">>, BodyJObj)
                 ,kz_json:get_value([<<"parentReference">>, <<"driveId">>], BodyJObj)
                 }
            of
                {'undefined', 'undefined'} -> {'error', Url, 'return_info_missing'};
                {'undefined', _DriveId} -> {'error', Url, 'return_id_missing'};
                {_Id, 'undefined'} -> {'error', Url, 'return_drive_missing'};
                {Id, DriveId} ->
                    {'ok'
                    ,{DriveId, Id}
                    ,[{<<"body">>, BodyJObj}
                      | kz_att_util:headers_as_binaries(ResponseHeaders)
                     ]
                    }
            end;
        Resp ->
            {'error', Url, Resp}
    end.

%% OneDrive REST API errors reference: https://docs.microsoft.com/en-us/onedrive/developer/rest-api/concepts/errors
-spec handle_http_error_response(kz_http:req(), kz_att_error:update_routines()) -> kz_att_error:error().
handle_http_error_response({'ok', RespCode, RespHeaders, RespBody} = _E, Routines) ->
    Reason = get_reason(RespCode, RespBody),
    NewRoutines = [{fun kz_att_error:set_resp_code/2, RespCode}
                  ,{fun kz_att_error:set_resp_headers/2, RespHeaders}
                  ,{fun kz_att_error:set_resp_body/2, RespBody}
                   | Routines
                  ],
    lager:error("onedrive error: ~p (code: ~p)", [_E, RespCode]),
    kz_att_error:new(Reason, NewRoutines);
handle_http_error_response({'error', {'failed_connect', Reason}} = _E, Routines) ->
    lager:error("onedrive failed to connect: ~p", [_E]),
    kz_att_error:new(Reason, Routines);
handle_http_error_response({'error', {Reason, _}} = _E, Routines) when is_atom(Reason) ->
    lager:error("onedrive request error: ~p", [_E]),
    kz_att_error:new(Reason, Routines);
handle_http_error_response(Reason, Routines) when is_atom(Reason) ->
    lager:error("onedrive request error: ~p", [Reason]),
    kz_att_error:new(Reason, Routines);
handle_http_error_response(_E, Routines) ->
    lager:error("dropbox request error: ~p", [_E]),
    kz_att_error:new('request_error', Routines).

-spec get_reason(atom() | pos_integer(), kz_term:ne_binary()) -> kz_term:ne_binary().
get_reason(RespCode, RespBody) when RespCode >= 400 ->
    %% If the `RespCode' value is >= 400 then the resp_body must contain an error object
    kz_json:get_ne_binary_value([<<"error">>, <<"message">>], kz_json:decode(RespBody));
get_reason(RespCode, _RespBody) ->
    kz_http_util:http_code_to_status_line(RespCode).
