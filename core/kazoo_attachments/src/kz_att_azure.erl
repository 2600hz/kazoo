%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2019, 2600Hz
%%% @doc Azure for attachments.
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_att_azure).
-behaviour(gen_attachment).

-include("kz_att.hrl").

%% `gen_attachment' behaviour callbacks (API)
-export([put_attachment/6]).
-export([fetch_attachment/4]).

-export([azure_default_fields/0]).

%%%=============================================================================
%%% `gen_attachment' behaviour callbacks (API)
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
    CT = kz_mime:from_filename(AName),

    {Container, Name} = resolve_path(Settings, {DbName, DocId, AName}),
    Pid = azure_pid(Settings),
    AzureOptions = [{content_type, kz_term:to_list(CT)}, return_headers],
    Url = lists:concat([Container, "/", Name]),
    Routines = [{fun kz_att_error:set_req_url/2, Url}
                | kz_att_error:put_routines(Settings, DbName, DocId, AName, Contents, Options)
               ],
    try
        case erlazure:put_block_blob(Pid, Container, Name, Contents, AzureOptions) of
            {ok, Headers, _Body} ->
                props:to_log(Headers, <<"AZURE HEADERS">>),
                Data = base64:encode(term_to_binary({Settings#{file => Name}, Name})),
                Metadata = kz_json:from_list(kz_att_util:headers_as_binaries(Headers)),
                {'ok', [{'attachment', [{<<"azure">>, Data}
                                       ,{<<"metadata">>, Metadata}
                                       ]}
                       ]};
            Resp ->
                handle_erlazure_error_response(Resp, Routines)
        end
    catch
        %% Next line left just for reference (cause it's not easy to spot it in the code)
        %%_:{{{'case_clause', {'error', {'failed_connect', _}}}, _StackTrace}, _FnFailing} ->
        _:{{{'case_clause', ErrorResp}, _StackTrace}, _FnFailing} ->
            lager:debug("failed to put the attachment.~nStackTrace: ~p~nFnFailing: ~p",
                        [_StackTrace, _FnFailing]),
            handle_erlazure_error_response(ErrorResp, Routines)
    end.


-spec fetch_attachment(gen_attachment:handler_props()
                      ,gen_attachment:db_name()
                      ,gen_attachment:doc_id()
                      ,gen_attachment:att_name()
                      ) -> gen_attachment:fetch_response().
fetch_attachment(HandlerProps, DbName, DocId, AName) ->
    Routines = kz_att_error:fetch_routines(HandlerProps, DbName, DocId, AName),
    case kz_json:get_value(<<"azure">>, HandlerProps) of
        'undefined' ->
            kz_att_error:new('invalid_data', Routines);
        AzureData ->
            {Settings, _ContentId} = binary_to_term(base64:decode(AzureData)),
            {Container, Name} = resolve_path(Settings, {DbName, DocId, AName}),
            Pid = azure_pid(Settings),
            case erlazure:get_blob(Pid, Container, Name) of
                {'ok', _RespBody} = Resp -> Resp;
                Resp ->
                    Url = lists:concat([Container, '/', Name]),
                    NewRoutines = [{fun kz_att_error:set_req_url/2, Url} | Routines],
                    handle_erlazure_error_response(Resp, NewRoutines)
            end
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec resolve_path(map(), attachment_info()) -> {string(), string()}.
resolve_path(#{container := Container} = Settings, AttInfo) ->
    Url = azure_format_url(Settings, AttInfo),
    {kz_term:to_list(Container), kz_term:to_list(Url)}.

-spec azure_default_fields() -> url_fields().
azure_default_fields() ->
    [{'group', [{'arg', <<"id">>}
               ,{'const', <<"_">>}
               ,{'arg', <<"attachment">>}
               ]}
    ].

-spec azure_format_url(map(), attachment_info()) -> kz_term:ne_binary().
azure_format_url(Map, AttInfo) ->
    kz_att_util:format_url(Map, AttInfo, azure_default_fields()).

azure_pid(#{account := Account, key := Key}) ->
    azure_pid(Account, Key).

azure_pid(Account, Key) ->
    case kz_att_azure_sup:worker(Account) of
        'undefined' ->
            case kz_att_azure_sup:start_azure(Account, Key) of
                {'ok', Pid} -> Pid;
                {'error',{'already_started',Pid}} -> Pid
            end;
        Pid -> Pid
    end.

-spec handle_erlazure_error_response({'error', string() | binary() | atom()},
                                     kz_att_error:update_routines()) -> kz_att_error:error().
handle_erlazure_error_response({'error', {'failed_connect', _}} = _E, Routines) ->
    lager:error("azure request failed: ~p", [_E]),
    kz_att_error:new('failed_to_connect', Routines);
handle_erlazure_error_response({'error', Reason} = _E, Routines) -> % from erlazure:execute_request/2
    lager:error("azure request failed : ~p", [_E]),
    kz_att_error:new(Reason, Routines);
handle_erlazure_error_response(_E, Routines) ->
    lager:error("azure request failed: ~p", [_E]),
    kz_att_error:new('request_error', Routines).
