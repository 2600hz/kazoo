%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2019, 2600Hz
%%% @doc Dropbox for attachments.
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_att_dropbox).

-behaviour(gen_attachment).

-include("kz_att.hrl").

%% `gen_attachment' behaviour callbacks (API)
-export([put_attachment/6]).
-export([fetch_attachment/4]).

-export([dropbox_default_fields/0]).

-define(DRV_UPLOAD_URL, <<"https://content.dropboxapi.com/2/files/upload">>).
-define(DRV_FETCH_URL, <<"https://content.dropboxapi.com/2/files/download">>).
-define(DRV_DROPBOX_HEADER(I), {<<"Dropbox-API-Arg">>, kz_json:encode(kz_json:from_list([{<<"path">>, I}]))}).

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
put_attachment(#{'oauth_doc_id' := TokenDocId} = Settings
              ,DbName, DocId, AName, Contents, _Options
              ) ->
    Authorization = kz_auth_client:token_for_auth_id(TokenDocId),
    do_put_attachment(Authorization, Settings, DbName, DocId, AName, Contents, _Options).

-spec do_put_attachment(kz_auth_client:token()
                       ,gen_attachment:settings()
                       ,gen_attachment:db_name()
                       ,gen_attachment:doc_id()
                       ,gen_attachment:att_name()
                       ,gen_attachment:contents()
                       ,gen_attachment:options()
                       ) -> gen_attachment:put_response().
do_put_attachment({'ok', #{'token' := #{'authorization' := Authorization}}}
                 ,Settings ,DbName, DocId, AName, Contents, Options
                 ) ->
    Url = resolve_path(Settings, {DbName, DocId, AName}),
    Headers = [{<<"Authorization">>, Authorization}
              ,{<<"Content-Type">>, <<"application/octet-stream">>}
              ,?DRV_DROPBOX_HEADER(Url)
              ],
    Routines = kz_att_error:put_routines(Settings, DbName, DocId, AName, Contents, Options),
    case dropbox_post(?DRV_UPLOAD_URL, Headers, Contents) of
        {'ok', ContentId, ResponseHeaders} ->
            Data = base64:encode(term_to_binary({Settings, ContentId})),
            {'ok', [{'attachment', [{<<"dropbox">>, Data}
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
    case kz_json:get_value(<<"dropbox">>, HandlerProps) of
        'undefined' ->
            Routines = kz_att_error:fetch_routines(HandlerProps, DbName, DocId, AName),
            kz_att_error:new('invalid_data', Routines);
        GData ->
            {#{'oauth_doc_id' := TokenDocId}, ContentId} = binary_to_term(base64:decode(GData)),
            Authorization = kz_auth_client:token_for_auth_id(TokenDocId),
            do_fetch_attachment(Authorization, ContentId, HandlerProps, DbName, DocId, AName)
    end.

-spec do_fetch_attachment(kz_auth_client:token()
                         ,kz_term:ne_binary()
                         ,gen_attachment:handler_props()
                         ,gen_attachment:db_name()
                         ,gen_attachment:doc_id()
                         ,gen_attachment:att_name()
                         ) -> gen_attachment:fetch_response().
do_fetch_attachment({'ok', #{'token' := #{'authorization' := Authorization}}}
                   ,ContentId, HandlerProps, DbName, DocId, AName) ->
    Headers = [{<<"Authorization">>, Authorization}
              ,?DRV_DROPBOX_HEADER(ContentId)
              ],
    case kz_http:get(?DRV_FETCH_URL, Headers) of
        {'ok', 200, _ResponseHeaders, ResponseBody} ->
            {'ok', ResponseBody};
        Resp ->
            Routines = [{fun kz_att_error:set_req_url/2, ?DRV_FETCH_URL}
                        | kz_att_error:fetch_routines(HandlerProps, DbName, DocId, AName)
                       ],
            handle_http_error_response(Resp, Routines)
    end;
do_fetch_attachment({'error', _}, _, HandlerProps, DbName, DocId, AName) ->
    Routines = kz_att_error:fetch_routines(HandlerProps, DbName, DocId, AName),
    kz_att_error:new('oauth_failure', Routines).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec resolve_path(map(), attachment_info()) -> kz_term:ne_binary().
resolve_path(Settings, AttInfo) ->
    <<"/", (dropbox_format_url(Settings, AttInfo))/binary>>.

-spec dropbox_default_fields() -> url_fields().
dropbox_default_fields() ->
    [{'group', [{'arg', <<"id">>}
               ,{'const', <<"_">>}
               ,{'arg', <<"attachment">>}
               ]}
    ].

-spec dropbox_format_url(map(), attachment_info()) -> kz_term:ne_binary().
dropbox_format_url(Map, AttInfo) ->
    kz_att_util:format_url(Map, AttInfo, dropbox_default_fields()).

-spec dropbox_post(binary(), kz_term:proplist(), binary()) ->
                          {'ok', binary(), [{binary(), binary()}]} |
                          {'error', kz_term:ne_binary(), kz_http:ret() | atom()}.
dropbox_post(Url, Headers, Body) ->
    case kz_http:post(Url, Headers, Body) of
        {'ok', 200, ResponseHeaders, ResponseBody} ->
            BodyJObj = kz_json:decode(ResponseBody),
            case kz_json:get_value(<<"id">>, BodyJObj) of
                'undefined' -> {'error', Url, 'return_id_missing'};
                ContentId ->
                    {'ok'
                    ,ContentId
                    ,[{<<"body">>, BodyJObj}
                      | kz_att_util:headers_as_binaries(ResponseHeaders)
                     ]
                    }
            end;
        Resp ->
            {'error', Url, Resp}
    end.

%% Dropbox REST API errors reference: https://www.dropbox.com/developers/documentation/http/documentation
-spec handle_http_error_response(kz_http:req(), kz_att_error:update_routines()) -> kz_att_error:error().
handle_http_error_response({'ok', RespCode, RespHeaders, RespBody} = _E, Routines) ->
    Reason = get_reason(RespCode, RespBody),
    NewRoutines = [{fun kz_att_error:set_resp_code/2, RespCode}
                  ,{fun kz_att_error:set_resp_headers/2, RespHeaders}
                  ,{fun kz_att_error:set_resp_body/2, RespBody}
                   | Routines
                  ],
    lager:error("dropbox error: ~p (code: ~p)", [_E, RespCode]),
    kz_att_error:new(Reason, NewRoutines);
handle_http_error_response({'error', {'failed_connect', Reason}} = _E, Routines) ->
    lager:error("dropbox failed to connect: ~p", [_E]),
    kz_att_error:new(Reason, Routines);
handle_http_error_response({'error', {Reason, _}} = _E, Routines)
  when is_atom(Reason) ->
    lager:error("dropbox request error: ~p", [_E]),
    kz_att_error:new(Reason, Routines);
handle_http_error_response(_E, Routines) ->
    lager:error("dropbox request error: ~p", [_E]),
    kz_att_error:new('request_error', Routines).

-spec get_reason(atom() | pos_integer(), kz_term:ne_binary()) -> kz_term:ne_binary().
get_reason(RespCode, RespBody) when RespCode >= 400 ->
    %% If the `RespCode' value is >= 400 then the resp_body must contain an error object
    kz_json:get_ne_binary_value([<<"error_summary">>], kz_json:decode(RespBody));
get_reason(RespCode, _RespBody) ->
    kz_http_util:http_code_to_status_line(RespCode).
